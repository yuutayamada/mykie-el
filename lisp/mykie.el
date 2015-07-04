;;; mykie.el --- Command multiplexer: Register multiple functions to a keybind -*- lexical-binding: t; -*-

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/mykie-el
;; Version: 0.3.1
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Keywords: Emacs, configuration, keybind

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;; This program can register multiple functions to a keybind easily.
;; For example:
;; (mykie:set-keys global-map ; <- you can specify nil to bind to global-map
;;   "C-j" ; You don't need kbd macro
;;   :default newline-and-indent   ; <- normal behavior
;;   :region  query-replace-regexp ; <- do query-replace-regexp in region
;;   "C-a"
;;   :default beginning-of-line
;;   :C-u     (message "hello") ; <- C-u + C-a, then you can see hello
;;   ;; You can add more keybinds
;;   ;; ...
;;   )
;;
;; Above function call newline-and-indent by default,
;; But call query-replace-regexp function if you select region.
;;
;; There are other way to bind a keybind:
;; (mykie:global-set-key "C-j" ; You don't need kbd macro
;;   :default newline-and-indent
;;   :region query-replace-regexp)
;;
;; You can see more example : https://github.com/yuutayamada/mykie-el
;;; Code:
(require 'cl-lib)
(autoload 'ido-active "ido")
(autoload 'ffap-file-at-point "ffap")
(when (require 'helm nil t)
  (autoload 'helm-show-mykie-keywords "helm-mykie-keywords" nil t))

;; CUSTOMIZE VARIABLE
(defvar mykie:region-conditions
  '((:region&repeat      . (mykie:repeat-p))
    (:region&C-u         . current-prefix-arg)
    (:region&prog        . mykie:prog-mode-flag)
    (:region&err         . (mykie:error-occur-p))
    ("^:region&.+-mode$" . (mykie:get-major-mode-state "region&")))
  "This variable is used at `mykie' function.
You don't need to contain region checking function. Mykie will check
whether region is active or not before check this variable.")

(defvar mykie:prefix-arg-conditions*
  '(("^:\\(M-\\|C-u\\*\\)[0-9]+!?$" . (mykie:get-prefix-arg-state)))
  "This variable is used at `mykie' function.
Mykie will check whether current-prefix-arg is non-nil
and make sure user pushed multiple time C-u more than one time.")

(defvar mykie:prefix-arg-conditions
  '((:C-u&repeat      . (mykie:repeat-p))
    (:C-u&err         . (mykie:error-occur-p))
    ("^:C-u&.+-mode$" . (mykie:get-major-mode-state "C-u&"))
    (:C-u&prog        . mykie:prog-mode-flag)
    (:C-u&email       . (mykie:thing-exist-p 'email))
    (:C-u&url         . (mykie:thing-exist-p 'url))
    (:C-u&file        . (mykie:file-at-point-p))
    (:C-u&bobp        . (bobp))
    (:C-u&eobp        . (eobp))
    (:C-u&bolp        . (bolp))
    (:C-u&eolp        . (eolp)))
  "This variable is used at `mykie' function.
You don't need to contain prefix-arg checking function. Mykie will check
whether current-prefix-arg is non-nil or not before check this variable.")

(defvar mykie:normal-conditions
  '((:repeat      . (mykie:repeat-p))
    (:err         . (mykie:error-occur-p))
    ("^:.+-mode$" . (mykie:get-major-mode-state))
    (:prog        . mykie:prog-mode-flag)
    (:comment     . (mykie:get-comment/string-state))
    (:ido         . (ido-active))
    (:email       . (mykie:thing-exist-p 'email))
    (:url         . (mykie:thing-exist-p 'url))
    (:file        . (mykie:file-at-point-p))
    (:minibuff    . (minibufferp))
    (:bobp        . (bobp))
    (:eobp        . (eobp))
    (:bolp        . (bolp))
    (:eolp        . (eolp))
    (:readonly    . buffer-read-only))
  "This variable is used at `mykie' function.")

(defvar mykie:group-conditions '(mykie:region-conditions
                                 mykie:prefix-arg-conditions*
                                 mykie:prefix-arg-conditions
                                 mykie:normal-conditions)
  "Mykie will check each condition by this list's order.
You can add another conditions and pre-check by `mykie:precheck-function'.")

(defvar mykie:use-major-mode-key-override nil
  "If this variable is non-nil, attach mykie's same global key function
to major-mode's key function if the function is exists.
You can specify 'both, 'global, 'self or t to this variable.
'both means do override self-insert-keys and other global-keys.
'global means do override  other global-keys only(such as C-j, not [a-z]).
'self means do override self-insert-keys only.
t means same as 'self. See also `mykie:attach-mykie-func-to'")

(defvar mykie:use-fuzzy-order t
  "If this variable is non-nil, you can change execution order when
you register key-bindings. This function is convenience if you want to
change keybind order by each keybind.

For example:
 (mykie:global-set-key \"C-j\"
   :default  new-line-and-indent
   :C-u&url  browse-url-at-point
   :C-u&eolp (fill-region (point-at-bol) (point-at-eol))

If you set above setting, then mykie is prior :C-u&url than :C-u&eolp.
If you want change the order, swap order between :C-u&url and
:C-u&eolp. Note: you can change order only same group conditions.
i.e., you can't change order region's function and C-u's function.")

(defvar mykie:major-mode-ignore-list '()
  "major-mode's list that ignore mykie's function if this list
contains current major-mode")

(defvar mykie:minor-mode-ignore-list '()
  "minor-mode's list that ignore mykie's function if this list
contains current minor-mode")

(defvar mykie:region-before-init-hook '(mykie:region-init))
(defvar mykie:region-after-init-hook  '(mykie:deactivate-mark))

(defvar mykie:region-func-predicate
  '(lambda ()
     (and (mykie:region-p)
          (cl-case mykie:current-state ((:region :region&C-u) t)))))

(defvar mykie:ignore-keybinds '("C-c")
  "Set this variable to avoid overriding specific key.
Normally use this variable to avoid overriding C-c key to use
`mode-specific-command-prefix'.")

(defvar mykie:default-keywords '(:default t)
  "Some function using :default keyword. So do not delete :default.
To change this variable use `add-to-list'.")

(defvar mykie:default-condition-keyword-alist
  '((mykie:region-conditions     (:region))
    (mykie:prefix-arg-conditions (:C-u :C-u!)))
  "Alist pair `mykie:group-conditions' element and default keyword(s) list.")

(defvar mykie:get-default-function
  (lambda (args)
    (cl-loop for i from 0 to (1- (length args)) by 2
             if (member (nth i args) mykie:default-keywords)
             do (cl-return (plist-get args (car it))))))

(defvar mykie:funcname-style 'dwim
  "Style of `mykie:make-funcname-function'.
You can specify 'old to make old style funcname.")

(defvar mykie:make-funcname-function
  (lambda (args keymap key &optional keymap-name)
    (let* ((keyname (replace-regexp-in-string " " "_" (key-description key)))
           (default-func (funcall mykie:get-default-function args))
           (funcname (if (listp default-func)
                         "anonymous-func"
                       default-func))
           (self-or-normal
            (if (eq default-func 'self-insert-command)
                "self-insert-command"
              "key")))
      (intern (format "mykie:%s:%s:%s"
                      (or keymap-name "") keyname
                      (cl-case mykie:funcname-style
                        ;; multiple-cursor refers to the key's function name
                        ;; So I think probably same function name is best.
                        (dwim (if (eq global-map keymap)
                                  self-or-normal
                                funcname))
                        (old self-or-normal)
                        (t   funcname)))))))

(defvar mykie:use-original-key-predicate nil
  "Predicate whether you use original keybind before load mykie.el.")

(defconst mykie:get-fallback-function
  (lambda (args)
    (cond ((mykie:ignore-mode-p)
           ;; Return default function
           (funcall mykie:get-default-function args))
          ((and mykie:use-original-key-predicate
                (funcall mykie:use-original-key-predicate))
           (let ((func (lookup-key
                        (bound-and-true-p mykie:original-map)
                        (car (plist-get args :key-info)))))
             `(call-interactively ,func)))))
  "Fallback function that returning fallback function's symbol.")

(defvar mykie:precheck-function
  (lambda (condition-name)
    (pcase condition-name
      (`mykie:region-conditions (mykie:region-p))
      (`mykie:prefix-arg-conditions*
       (and current-prefix-arg
            (not (equal '(4) current-prefix-arg))))
      (`mykie:prefix-arg-conditions current-prefix-arg)
      (_ t)))
  "Pre-check condition depending on CONDITION-NAME before check the
CONDITION-NAME's condition. If you add conditions to
`mykie:group-conditions', then you can add your precheck condition by
change this variable.
By default, this function check whether region is active or prefix-arg
is exists.")

;; INTERNAL VARIABLES
(defvar mykie:current-state nil)
(defvar mykie:current-args '())
(defvar mykie:current-point "")
(defvar mykie:current-thing nil)
(defvar mykie:current-file nil)
(defvar mykie:current-keyword nil)
(defvar mykie:region-str "")
(defvar mykie:C-u-num nil)
(defconst mykie:original-map (copy-keymap global-map)
  "Keymap before load mykie.el.")

(defvar mykie:global-keys '()
  "This variable will store global-map's key and mykie's args pair
list without self insert key such as \"a\" etc..
See also `mykie:attach-mykie-func-to' or `mykie:use-major-mode-key-override'")

(defvar mykie:self-insert-keys '()
  "This variable will store self-insert-key's key and mykie's args pair list.
To use this variable, you need to use function `mykie:define-key-with-self-key'
or `mykie:set-keys' with 'with-self-key argument.
See also `mykie:attach-mykie-func-to' or `mykie:use-major-mode-key-override'")

(defvar mykie:attached-mode-list '()
  "list that `mykie:attach-mykie-func-to' function attached mode name list.")

(defvar mykie:prog-mode-flag nil
  "Buffer local variable, t means this buffer is related programing mode.
Otherwise nil.")

(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'mykie:prog-mode-flag) t)))

;;;###autoload
(defmacro mykie:loop (&rest args)
  `(mykie:loop-core (quote ,args)))

(defun mykie:loop-core (keybinds)
  (let*
      (keynum
       (exist-p
        (lambda ()
          (cl-loop with input = (read-key)
                   with format = (lambda (input)
                                   (condition-case _err
                                       (char-to-string input)
                                     (error (vector input))))
                   for i from 0 to (- (length keybinds) 2) by 2
                   if (equal (nth i keybinds)
                             (funcall format input))
                   do (cl-return (setq keynum i))))))
    (cl-loop while (funcall exist-p)
             for func = (nth (1+ keynum) keybinds)
             if func
             do (mykie:execute func))))

;;;###autoload
(defmacro mykie:do-while (&rest args)
  "Firstly do 1th function of ARGS and then do `mykie:loop' with ARGS."
  `(progn (mykie:execute (nth 1 (quote ,args)))
          (mykie:loop-core (quote ,args))))

(defun mykie:execute (func)
  "Execute FUNC.
You can specify like following forms to the FUNC:
Example
 (mykie:loop
  \"j\" (lambda ()
        (newline-and-indent))
  \"m\" newline
  \"g\" (if current-prefix-arg
            (keyboard-quit)))"
  (mykie:run-hook 'before)
  (unless (ido-active)
    (run-hooks 'pre-command-hook))
  (let ((current-prefix-arg
         (if (mykie:contain-exclamation-p mykie:current-state)
             nil
           current-prefix-arg)))
    (cl-typecase func
      (command
       ;; Do not use `setq' because the command loop overrides
       ;; `last-command', so use timer here.
       (run-with-timer 0 nil 'set 'last-command func)
       (funcall 'call-interactively func))
      (function (funcall func))
      (list     (funcall 'eval `(progn ,func)))))
  (unless (or (ido-active) (bound-and-true-p multiple-cursors-mode))
    (run-hooks 'post-command-hook))
  (mykie:run-hook 'after))

;;;###autoload
(defun mykie:attach-mykie-func-to (&optional mode-symbol)
  "Attach mykie's functions to the MODE's same key function without :default.
Use the MODE's function as :default function.
If you didn't specify the MODE, then use current major-mode by default.
The MODE is mode name's symbol such as 'emacs-lisp-mode."
  (interactive)
  (let*
      ((mode (or mode-symbol major-mode))
       (ignore (or (member mode mykie:major-mode-ignore-list)
                   (member mode mykie:minor-mode-ignore-list)))
       (attach-func
        (lambda (mykie-global-keys)
          (cl-loop with keymap-name = (format "%s-map" mode)
                   with keymap      = (symbol-value (intern keymap-name))
                   for key in mykie-global-keys
                   for args = (funcall (lookup-key global-map key) t)
                   for mode-func = (lookup-key keymap key)
                   if (and (keymapp keymap)
                           (functionp mode-func)
                           (not (string-match "^mykie:" (symbol-name mode-func))))
                   do (mykie:clone-key
                       key args `(:default ,mode-func) `(,keymap-name . ,keymap))))))
    (if ignore
        (message (format "mykie overriding key: Ignore %S" mode))
      (condition-case err
          (if (member mode mykie:attached-mode-list)
              (error (format "Mykie: already attached %s" (symbol-name mode)))
            (cl-case mykie:use-major-mode-key-override
              (both   (funcall attach-func mykie:global-keys)
                      (funcall attach-func mykie:self-insert-keys))
              (global (funcall attach-func mykie:global-keys))
              (self   (funcall attach-func mykie:self-insert-keys))
              (t      (funcall attach-func mykie:self-insert-keys)))
            (add-to-list 'mykie:attached-mode-list mode))
        (error err)))))

(defun mykie:repeat-p ()
  (equal this-command last-command))

(defun mykie:region-p ()
  (or (region-active-p)
      (bound-and-true-p evil-visual-region-expanded)))

(defun mykie:get-C-u-times ()
  "Return times that your pushed C-u's times. And you can use mykie:C-u-num
variable to get the times after do this function if you want."
  (setq mykie:C-u-num (truncate (log (or (car current-prefix-arg) 1) 4)))
  mykie:C-u-num)

(defalias 'mykie:C-u-num 'mykie:get-C-u-times)

(defmacro mykie:aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'mykie:aif 'lisp-indent-function 2)

(defun mykie:thing-exist-p (thing)
  "Return thing-at-point's return value of THING.
If return value is non-nil, then save the value to `mykie:current-thing'."
  (mykie:aif (thing-at-point thing)
      (setq mykie:current-thing it)))

(defun mykie:get-comment/string-state ()
  "Return :comment if current point is comment or string face."
  (when (nth 8 (save-excursion (syntax-ppss
                                (if (bobp) (point) (1- (point))))))
    :comment))

(defun mykie:file-at-point-p ()
  "Return non-nil if current point is file related path.
And then save the path to `mykie:current-file' variable."
  (mykie:aif (ffap-file-at-point)
      (setq mykie:current-file it)))

(defun mykie:error-occur-p  ()
  "Return non-nil if `flycheck-current-errors' or `flymake-err-info' is non-nil."
  (or (bound-and-true-p flymake-err-info)
      (bound-and-true-p flycheck-current-errors)))

(defun mykie:get-major-mode-state (&optional prefix)
  "Return :major-mode, :C-u&major-mode or :region&major-mode if
you specified same state name to `mykie's args and matched
condition if you specified prefix(whether current-prefix-arg or region
active).
The major-mode replaced to `major-mode' name.
You can specify \"C-u&\" or \"region&\" to the PREFIX."
  (let ((keyword (intern (format ":%s%s" (or prefix "") major-mode))))
    (when (plist-get mykie:current-args keyword)
      keyword)))

(defun mykie:get-prefix-arg-state ()
  "Return keyword like :C-u, :C-*N or :M-N.
If current-prefix-arg is list, return :C-u or :C-u*N(N is replaced to
C-u's pushed times).
If current-prefix-arg is number, return :M-N(the N is replaced number
like M-1.). You can change the M-N's number by pushing M-[0-9] before
call `mykie' function."
  (let ((exclamation (if (mykie:contain-exclamation-p) "!" "")))
    (cl-typecase current-prefix-arg
      (list   (let ((times (mykie:get-C-u-times)))
                (if (= 1 times)
                    :C-u
                  (intern (format ":C-u*%i%s" times exclamation)))))
      (number (intern (format ":M-%i%s" current-prefix-arg exclamation))))))

(defun mykie:contain-exclamation-p (&optional keyword)
  (string-match ":.+!$" (symbol-name (or keyword mykie:current-keyword))))

;; Backward compatibility
(defalias 'mykie:get-C-u-keyword 'mykie:get-prefix-arg-state)

(defun mykie:get-skk-state ()
  "Return SKK(simple kana kanji)'s state.
If `on'(▽) mode then return :skk-on.
If `active'(▼) mode then return :skk-active."
  (when (bound-and-true-p skk-mode)
    (cl-case (bound-and-true-p skk-henkan-mode)
      (active :skk-active)
      (on     :skk-on))))

(defun mykie:ignore-mode-p ()
  "Return non-nil if matched specified modes.
If you specified :ignore-major-modes with major-mode's list to mykie's args,
then check whether major-mode list match current major-mode.
If you specified :ignore-minor-modes with minor-mode's list to mykie's args,
then check whether minor-mode list match current `minor-mode-list'."
  (or (member major-mode
              (plist-get mykie:current-args :ignore-major-modes))
      (cl-loop for mode in (plist-get mykie:current-args :ignore-minor-modes)
               if (member mode minor-mode-list)
               do (cl-return t))))

(defun mykie:initialize ()
  (if mykie:use-major-mode-key-override
      (add-hook  'change-major-mode-after-body-hook 'mykie:attach-mykie-func-to)
    (remove-hook 'change-major-mode-after-body-hook 'mykie:attach-mykie-func-to)))

(defun mykie:init (args)
  "Initialize mykie's global-variable before do mykie's command."
  (when (plist-get args :use-C-u-num)
    (mykie:get-C-u-times))
  (setq mykie:current-args args)
  (let
      ((fallback (funcall mykie:get-fallback-function args)))
    (when fallback
      (pcase fallback
        (`(call-interactively ,func) (call-interactively func))
        (fallback (mykie:execute fallback)))
      'exit)))

(defun mykie:region-init ()
  "Initialize mykie's global-variable for region.
You can use `mykie:region-str' variable that store region string.
If you specified 'kill or 'copy with :region-handle-flag of mykie's args,
then do `kill-region' or `copy-region-as-kill' before do mykie's command.
So you can use kill-ring variable that store region's variable if you want."
  (setq mykie:region-str
        (buffer-substring (region-beginning) (region-end)))
  (cl-case (plist-get mykie:current-args :region-handle-flag)
    (kill (kill-region         (region-beginning) (region-end)))
    (copy (copy-region-as-kill (region-beginning) (region-end)))))

(defun mykie:deactivate-mark ()
  "Deactivate region if you specified :deactivate-region of mykie's
args with non-nil after do mykie's command."
  (let
      ((deactivation
        (plist-get mykie:current-args :deactivate-region)))
    (when (or (and (eq 'region     deactivation)
                   (eq :region     mykie:current-state))
              (and (eq 'region&C-u deactivation)
                   (eq :region&C-u mykie:current-state))
              (eq      t          deactivation))
      (deactivate-mark))))

(defmacro mykie (&rest args)
  "Call function you are set functions.
You can set below keyword to ARGS by default:

*Functions*
 :default or t      | Call this if conditions aren't matched all conditions
 :C-u               | Call this if you pushed C-u key before pushing the key
 :C-u*N             | Call this if you pushed N times of C-u
 :M-N               | Call this if you pushed such as M-1
 :region            | Call this if you are selecting region
 :region&C-u        | Call this if you satisfied :region & :C-u condition
 :repeat            | Call this if you repeat same command at same point
 :bolp              | Call this if current point is beginning of line
 :eolp              | Call this if current point is end of line
 :bobp              | Call this if current point is beginning of buffer
 :eobp              | Call this if current point is end of buffer
 :C-u&bolp          | Call this if you satisfied :C-u & :bolp
 :C-u&eolp          | Call this if you satisfied :C-u & :eolp
 :C-u&bobp          | Call this if you satisfied :C-u & :bobp
 :C-u&eobp          | Call this if you satisfied :C-u & :eobp
 :email             | Call this if current point matched (thing-at-point 'email)
 :C-u&email         | Call this if you satisfied :C-u & :email
 :url               | Call this if current point matched (thing-at-point 'url)
 :C-u&url           | Call this if you satisfied :C-u & :url
 :MAJOR-MODE        | Call this if :MAJOR-MODE matched `major-mode'.
 :C-u&MAJOR-MODE    | Call this if you satisfied :C-u & :MAJOR-MODE
 :region&MAJOR-MODE | Call this if you satisfied :region & :MAJOR-MODE
 :prog              | Call this if current buffer is related `prog-mode'
 :C-u&prog          | Call this if you satisfied :C-u & :prog
 :region&prog       | Call this if you satisfied :region & :prog
 :err               | Call this if error is exists of flymake or flycheck.
 :C-u&err           | Call this if you satisfied :C-u & :err
 :region&err        | Call this if you satisfied :region & :err
 :minibuff          | Call this if current point is in minibuffer
 :readonly          | Call this if current buffer is read-only
 :comment           | Call this if current point is string or comment face

*Flags*
 :clone - Clone mykie's functions to other KEY.
   This function is convenient if you use Emacs either situation
   terminal and GUI.  Because terminal Emacs can't use partial keybind
   such as C-;, this keyword can clone same functions to another key
   without :default function.

 For example:
 (mykie:global-set-key \"C-;\"
   :default (message \"foo\")
   :region  comment-dwim
   :clone   \";\")

 Above example copy mykie's function to \";\" key without :default.

 :region-handle-flag - you can set 'copy and 'kill
   If you set 'kill then region's string is killed.
   If you set 'copy then region's string is copied.
   And you can use the value by `kill-ring'.

 But you can use `mykie:region-str' variable that have region's string too.

 :deactivate-region - deactivate region after region command execution
   If you set 'region then deactivate region when you did not push C-u.
   If you set 'region&C-u then deactivate region when you pushed C-u.
   If you set t then deactivate region in both cases."
  `(mykie:core (quote ,args)))

(defun mykie:core (args)
  (setq args (if (symbolp args) (symbol-value args) args))
  (cl-loop initially (when (eq 'exit (mykie:init args)) (cl-return))
           for conditions-sym in mykie:group-conditions
           for conditions = (symbol-value conditions-sym)
           for default-kw = (mykie:get-default-condition-keyword conditions-sym)
           if (and (funcall mykie:precheck-function conditions-sym)
                   (if mykie:use-fuzzy-order
                       (mykie:by-fuzzy-order args conditions default-kw)
                     (mykie:by-condition-order args conditions default-kw)))
           do (progn (setq mykie:current-state it)
                     (mykie:execute (plist-get args it))
                     (cl-return))
           finally (mykie:execute (funcall mykie:get-default-function args)))
  (unless (mykie:repeat-p)
    (setq mykie:current-point (point))))

(defun mykie:by-fuzzy-order (args conditions default-keywords)
  "Check CONDITIONS by keyword base that extracted from ARGS."
  (cl-loop with cond-len = (1- (length conditions))
           with default-keyword
           for i from 0 to (1- (length args)) by 2
           for keyword = (nth i args)
           if (member keyword mykie:default-keywords)
           do '() ; do nothing
           else if (cl-loop for j from 0 to cond-len
                            for expect = (car (nth j conditions))
                            if (or (eq expect keyword)
                                   (and (stringp expect)
                                        (string-match
                                         expect (symbol-name keyword))
                                        (setq mykie:current-keyword keyword)))
                            do (cl-return (nth j conditions)))
           do (when (eq keyword (mykie:check it))
                (cl-return keyword))
           else if (member keyword default-keywords)
           do (setq default-keyword keyword)
           finally return default-keyword))

(defun mykie:check (kw-and-condition)
  "Return keyword if checking condition is succeed.
If condition's result is keyword, return the value.
Otherwise return KW-AND-CONDITION's first element."
  (let ((keyword (car kw-and-condition))
        (result  (eval (cdr kw-and-condition))))
    (when result
      (if (keywordp result)
          result
        keyword))))

(defun mykie:by-condition-order (args conditions default-keywords)
  "Check CONDITIONS by the CONDITIONS order."
  (cl-loop for condition in conditions
           if (member (car condition) mykie:default-keywords)
           do '()
           else if (mykie:check condition)
           do (when (plist-get mykie:current-args it)
                (cl-return it))
           finally return (cl-loop with last = (1- (length mykie:current-args))
                                   for i from 0 to last by 2
                                   if (member (nth i args) default-keywords) do
                                   (cl-return (car it)))))

(defun mykie:get-default-condition-keyword (conditions-sym)
  "Get condition specific keyword that match CONDITIONS-SYM from
`mykie:default-condition-keyword-alist'."
  (car (assoc-default conditions-sym mykie:default-condition-keyword-alist)))

(defun mykie:run-hook (direction)
  (when (funcall mykie:region-func-predicate)
    (cl-case direction
      (before (run-hooks 'mykie:region-before-init-hook))
      (after  (run-hooks 'mykie:region-after-init-hook)))))

(defun mykie:kbd (key)
  (kbd key))

;; TODO: Fix this to work around in old version Emacs
(when (version< emacs-version "24.3")
  (defadvice mykie:kbd (around macro->function activate)
    (setq ad-return-value (apply (macroexpand (list 'kbd (ad-get-arg 0)))))))

(defun mykie:format-key (key)
  (cl-typecase key
    (vector key)
    (string (mykie:kbd key))
    (t (error "Invalid key"))))

;;;###autoload
(defmacro mykie:define-key (keymap key &rest args)
  "In KEYMAP, define key sequence KEY as `mykie' command with ARGS.
In other words, `mykie' + `define-key'.

Example:
 (mykie:define-key global-map \"y\"
   :default self-insert-command
   :region (message \"%s\" mykie:region-str)
   :C-u (message \"C-u y\"))"
  `(mykie:define-key-core
    (symbol-name (quote ,keymap)) ,keymap ,key (quote ,args)))
(put 'mykie:define-key 'lisp-indent-function 2)

(defun mykie:define-key-core (keymap-name keymap key args)
  (let* ((key (mykie:format-key key)))
    (unless (member (key-description key) mykie:ignore-keybinds)
      (if (eq nil (car args))
          (define-key keymap key nil)
        (let* ((args (append (mykie:parse-parenthesized-syntax args)
                             (unless (plist-get args :key-info)
                               `(:key-info (,key . ,keymap-name)))))
               (sym (funcall mykie:make-funcname-function
                             args keymap key keymap-name)))
          (when (and (equal "global-map" keymap-name)
                     (< 1 (length (key-description key))))
            (add-to-list 'mykie:global-keys key))
          (fset sym (mykie:make-mykie-function args))
          (define-key keymap key sym)
          (mykie:aif (plist-get args :clone)
              (progn
                (if (and (stringp it) (= 1 (length it)))
                    (add-to-list 'mykie:self-insert-keys it)
                  (add-to-list 'mykie:global-keys it))
                (mykie:clone-key
                 it (mykie:replace-property args `(:key-info (,it . "global-map")))
                 '(:default self-insert-command)))))))))

(defun mykie:make-mykie-function (args)
  (lambda (&optional get-args)
    (interactive)
    (if get-args args (funcall 'mykie:core args))))

(defun mykie:clone-key (key args default-keyword-and-func &optional keymap-info)
  (let
      ((new-args
        (mykie:filter (mykie:replace-property args default-keyword-and-func)
                      :clone))
       (map-name (or (car keymap-info) "global-map"))
       (map      (or (cdr keymap-info)  global-map)))
    (funcall 'mykie:define-key-core map-name map key new-args)))

(defun mykie:replace-property (args key-and-property)
  (append (mykie:filter args (car key-and-property))
          key-and-property))

(defun mykie:filter (args keyword)
  "Delete KEYWORD and the KEYWORD's function or property."
  (if (not (member keyword args))
      args
    (cl-loop with last = (1- (length args))
             with ignore = nil
             for i from 0 to last
             if (eq keyword (nth i args))
             do (push (1+ i) ignore)
             else if (not (member i ignore))
             collect (nth i args))))

;;;###autoload
(defmacro mykie:global-set-key (key &rest args)
  "Give KEY a global binding as `mykie' command.
In other words, `mykie' + `global-set-key'.

Example:
 (mykie:global-set-key \"z\"
   :default self-insert-command
   :region (message \"%s\" mykie:region-str)
   :C-u (message \"C-u z\"))"
  `(mykie:define-key-core "global-map" global-map ,key (quote ,args)))
(put 'mykie:global-set-key 'lisp-indent-function 1)

;;;###autoload
(defmacro mykie:define-key-with-self-key (key &rest args)
  "Set self-insert-key(KEY) with `mykie' command.
This function register :default `self-insert-command' automatically to ARGS.
Example:
  (mykie:define-key-with-self-key
      \"a\" :C-u (message \"I am C-u\"))"
  `(mykie:define-key-with-self-key-core ,key (quote ,args)))

(defun mykie:define-key-with-self-key-core (key args)
  (setq args (mykie:parse-parenthesized-syntax args))
  (add-to-list 'mykie:self-insert-keys key)
  (mykie:define-key-core "global-map" global-map (mykie:format-key key)
                         (append args
                                 '(:default self-insert-command)
                                 (mykie:aif mykie:major-mode-ignore-list
                                     `(:ignore-major-modes ,it))
                                 (mykie:aif mykie:minor-mode-ignore-list
                                     `(:ignore-minor-modes ,it)))))
(put 'mykie:define-key-with-self-key 'lisp-indent-function 1)

;;;###autoload
(defmacro mykie:set-keys (keymap-or-order &rest args)
  "Set keybinds as `mykie' command.
Examples:
  Set keybinds to global-map:
  (mykie:set-keys nil ; You can set 'global or global-map instead of nil too.
    \"C-a\"
    :default     (beginning-of-line)
    :C-u         mark-whole-buffer
    \"C-e\"
    :default     (end-of-line)
    :C-u         (message \"Hello\"))

  Set keybinds to specific keymap:
  (mykie:set-keys emacs-lisp-mode-map
    \"C-1\"
    :default (message \"C-1\")
    :C-u     (message \"C-1+C-u\")
    \"C-2\"
    :default (message \"C-2\")
    :C-u     (message \"C-2+C-u\"))

  Set keybinds for self-insert-key
  You don't need to specify :default state, it's specified to
  'self-insert-command automatically to it.
  (mykie:set-keys 'with-self-key
   \"a\"
   :C-u (message \"called a\")
   :region query-replace-regexp
   \"b\"
   :C-u (message \"called b\"))"
  `(mykie:set-keys-core (quote ,keymap-or-order) (quote ,args)))
(put 'mykie:set-keys 'lisp-indent-function 1)

(defun mykie:set-keys-core (keymap-or-order args)
  (let*
      ((pair (cl-typecase keymap-or-order
               (null   '(nil . global-map))
               (symbol (condition-case _err
                           (when (keymapp (symbol-value keymap-or-order))
                             (cons nil keymap-or-order))
                         (error (cons keymap-or-order 'global-map))))))
       (order      (car pair))
       (keymap-sym (cdr pair))
       (set-key
        (lambda (key-and-prop &optional keymap-name keymap)
          (let ((key (car key-and-prop))
                (property (mykie:format-property order (cdr key-and-prop))))
            (cl-case order
              (with-self-key
               (mykie:define-key-with-self-key-core key property))
              (t (mykie:define-key-core keymap-name keymap key property))))))
       (set-keys
        (lambda ()
          (cl-loop with last = (1- (length args))
                   with keymap-name = (symbol-name keymap-sym)
                   with keymap = (symbol-value keymap-sym)
                   for i from 0 to last
                   for next = (1+ i)
                   for key-or-prop = (nth i args)
                   collect key-or-prop into key-and-prop
                   if (or (equal i last)
                          (and (not (eq :clone (nth i args)))
                               (cl-typecase (nth next args)
                                 (string t)
                                 (vector t))))
                   do (progn
                        (funcall set-key key-and-prop keymap-name keymap)
                        (setq key-and-prop nil))))))
    (funcall set-keys)))

(defun mykie:format-property (order property)
  (let ((props (if (condition-case _err
                       (keywordp (caar property))
                     (error nil))
                   (mykie:parse-parenthesized-syntax property)
                 property)))
    (if (eq order 'with-self-key)
        props
      (if (not (or (member :default props)
                   (member t props)))
          (cl-case (length props)
            (1 (append '(:default) props))
            (2 (append '(:default) `(,props))))
        props))))

;;;###autoload
(defun mykie:parse-parenthesized-syntax (args)
  (cl-typecase (car args)
    (list (cl-loop for (keyword . function) in args
                   collect keyword into new-args
                   if (listp function)
                   collect `(progn ,@function) into new-args
                   else collect function into new-args
                   finally return new-args))
    (symbol args)))

;;;###autoload
(defmacro mykie:define-prefix-key (parent-map prefix-key params &rest mykie-keys)
  "Make prefix key with MYKIE-KEYS(mykielized keybindings).

This function takes PARENT-MAP as parent of PREFIX-KEY.

The PARAMS can take several parameters of anonymous function:
  :keep   -- keep prefix keymap as long as this function return non-nil
  :exit   -- exit prefix keymap when the function return non-nil
  :before -- call this function before entering prefix keymap
  :after  -- call this function after exited prefix keymap

Note that those keyword PARAMS are optional; which means if you set
nil to the PARAMS, created function will be exited after you type any
created key."
  `(let* ((key (mykie:format-key ,prefix-key))
          (keyname (key-description key))
          (prefix-map (intern (format "mykie:%s-prefix-%s"
                                      (quote ,parent-map) keyname)))
          (pred
           `(lambda ()
              (let ((exit ,(plist-get (quote ,params) :exit))
                    (keep ,(plist-get (quote ,params) :keep)))
                (or (when (functionp keep) (funcall keep))
                    (when (functionp exit) (not (funcall exit)))))))
          (on-enter (plist-get (quote ,params) :before))
          (on-exit  (plist-get (quote ,params) :after))
          (parent (symbol-value (quote ,parent-map))))
     (define-prefix-command prefix-map)
     (mykie:set-keys-core prefix-map (quote ,mykie-keys))
     (fset prefix-map
           `(lambda ()
              (interactive)
              (when (functionp ,on-enter) (funcall ,on-enter))
              (set-transient-map ,prefix-map ,pred ,on-exit)))
     (define-key parent key prefix-map)))
(put 'mykie:define-prefix-key 'lisp-indent-function 2)

(defmacro mykie:combined-command (&rest args)
  "This macro return lambda form with `mykie'.
So you can register keybind like this:

 (global-set-key (kbd \"C-j\")
  (mykie:combined-command
    :default newline-and-indent
    :C-u (fill-region (point-at-bol) (point-at-eol))))"
  `(lambda ()
     (interactive)
     (mykie:core (quote ,args))))

(defadvice mykie:combined-command
    (around ad-parse-parenthesized activate)
  "Parse args to convert parenthesized-syntax if it was needed."
  (ad-set-args 0 (mykie:parse-parenthesized-syntax (ad-get-args 0)))
  ad-do-it)

;;;###autoload
(defadvice mykie
    (around mykie:parse-parenthesized-syntax activate)
  "Parse args to convert parenthesized-syntax if it was needed."
  (ad-set-args 0 (mykie:parse-parenthesized-syntax (ad-get-args 0)))
  ad-do-it)

(when mykie:use-major-mode-key-override
  (mykie:initialize))

;; work around that magit doesn't bind mykie's self-insert keys.
(with-eval-after-load 'magit
  (cl-loop for key in mykie:self-insert-keys
           unless (member key '("?" "-" "="))
           do (define-key (bound-and-true-p magit-popup-mode-map) key 'magit-invoke-popup-action)))

(provide 'mykie)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mykie.el ends here
