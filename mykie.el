;;; mykie.el --- Command multiplexer: Register multiple functions to a keybind

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/mykie-el
;; Version: 0.0.8
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
;;   :default 'newline-and-indent   ; <- normal behavior
;;   :region  'query-replace-regexp ; <- do query-replace-regexp in region
;;   "C-a"
;;   :default 'beginning-of-line
;;   :C-u     '(message "hello") ; <- C-u + C-a, then you can see hello
;;   ;; You can add more keybinds
;;   ;; ...
;;   )
;;
;; Above function call newline-and-indent by default,
;; But call query-replace-regexp function if you select region.
;;
;; There are other way to bind a keybind:
;; (mykie:global-set-key "C-j" ; You don't need kbd macro
;;   :default 'newline-and-indent
;;   :region 'query-replace-regexp)
;;
;; You can see more example : https://github.com/yuutayamada/mykie-el
;;; Code:
(eval-when-compile (require 'cl))

;; CUSTOMIZE VARIABLE
(defvar mykie:region-conditions
  '((when (region-active-p)
      (or (and current-prefix-arg
               :region&C-u)
          (mykie:get-major-mode-state "region&")
          (mykie:get-prog-mode-state "region&")
          :region))))

(defvar mykie:prefix-arg-conditions
  '((mykie:get-major-mode-state "C-u&")
    (mykie:get-prog-mode-state "C-u&")
    (mykie:get-thing-state 'email :prefix "C-u&")
    (mykie:get-thing-state 'url   :prefix "C-u&")
    (when current-prefix-arg
      (or (and (eobp)        :C-u&eobp)
          (and (bobp)        :C-u&bobp)))
    (when current-prefix-arg
      (or (and (bolp)        :C-u&bolp)
          (and (eolp)        :C-u&eolp)))
    (mykie:get-prefix-arg-state)
    ;; Use :C-u if C-u*N isn't exists
    (when current-prefix-arg :C-u)))

(defvar mykie:normal-conditions
  '((when (mykie:repeat-p)   :repeat)
    (mykie:get-major-mode-state)
    (mykie:get-prog-mode-state)
    (mykie:get-thing-state   'email)
    (mykie:get-thing-state   'url)
    (when (minibufferp)      :minibuff)
    (when (bobp)             :bobp)
    (when (eobp)             :eobp)
    (when (bolp)             :bolp)
    (when (eolp)             :eolp)
    (when buffer-read-only   :readonly)))

(defvar mykie:before-user-region-conditions '())
(defvar mykie:after-user-region-conditions '())
(defvar mykie:before-user-prefix-arg-conditions '())
(defvar mykie:after-user-prefix-arg-conditions '())
(defvar mykie:before-user-normal-conditions '())
(defvar mykie:after-user-normal-conditions '())

(defvar mykie:conditions '()
  "This variable is evaluated in mykie's loop by each the when statement.
Then if the when statement is match, return value(like :C-u) and then
same keyword's function that you are specified is evaluated.
Note: Order is important. Above list element have more priority than
below elements. If you dislike :repeat's priority, then you can change
this behavior by this variable.")

(defun mykie:initialize ()
  (setq mykie:conditions
        (append
         ;; REGION
         mykie:before-user-region-conditions
         mykie:region-conditions
         mykie:after-user-region-conditions
         ;; PREFIX-ARGUMENT
         mykie:before-user-prefix-arg-conditions
         mykie:prefix-arg-conditions
         mykie:after-user-prefix-arg-conditions
         ;; NORMAL
         mykie:before-user-normal-conditions
         mykie:normal-conditions
         mykie:after-user-normal-conditions)))

(defvar mykie:ignore-major-modes-for-self-insert-key '()
  "major-mode's list that ignore mykie's function if this list
contains current major-mode")

(defvar mykie:ignore-minor-modes-for-self-insert-key '()
  "minor-mode's list that ignore mykie's function if this list
contains current minor-mode")

(defvar mykie:region-before-init-hook '(mykie:region-init))
(defvar mykie:region-after-init-hook  '(mykie:deactivate-mark))

(defvar mykie:region-func-predicate
  '(lambda ()
     (and (region-active-p)
          (case mykie:current-state ((:region :region&C-u) t)))))

(defvar mykie:make-funcname-function
  (lambda (args mykie:keymaps keymap key &optional keymap-name)
    (intern (format
             "mykie:%s:%s:%s:%s"
             (or keymap-name "")
             (replace-regexp-in-string
              " " "_"
              (key-description key))
             ;; Some programs check command name by
             ;; (string-match "self-insert-command" command-name)
             (if (eq (plist-get args :default) 'self-insert-command)
                 "self-insert-command" "key")
             ;; find keymap index
             (loop for k in mykie:keymaps
                   for i from 0
                   if (eq k keymap)
                   return (- (length mykie:keymaps) i))))))

;; INTERNAL VARIABLES
(defvar mykie:keymaps nil)
(defvar mykie:current-state nil)
(defvar mykie:current-args '())
(defvar mykie:current-point "")
(defvar mykie:current-thing nil)
(defvar mykie:region-str "")
(defvar mykie:C-u-num nil)

(defvar-local mykie:prog-mode-flag nil
  "Buffer local variable, t means this buffer is related programing mode.
Otherwise nil.")
(add-hook 'prog-mode-hook
          '(lambda () (setq-local mykie:prog-mode-flag t)))

(defun mykie:loop (&rest keybinds)
  (lexical-let*
      (keynum
       (exist-p
        (lambda ()
          (loop with input = (read-key)
                with format = (lambda (input)
                                (condition-case err
                                    (char-to-string input)
                                  (error (vector input))))
                for i from 0 to (- (length keybinds) 2) by 2
                if (equal (nth i keybinds)
                          (funcall format input))
                do (return (setq keynum i))))))
    (loop while (funcall exist-p)
          for func = (nth (1+ keynum) keybinds)
          if func
          do (mykie:execute func))))

(defun mykie:execute (func)
  "Execute FUNC.
You can specify like following forms to the FUNC:
Example
 (mykie:loop
  \"j\" (lambda ()
        (newline-and-indent))
  \"m\" 'newline
  \"g\" '(if current-prefix-arg
             (keyboard-quit)))"
  (run-hooks 'pre-command-hook)
  (cond
   ((commandp func)
    ;; Do not use `setq' because the command loop overrides
    ;; `last-command', so use timer here.
    (run-with-timer 0 nil 'set 'last-command func)
    (funcall 'call-interactively func))
   ((functionp func)
    (funcall func))
   ((listp func)
    (funcall 'eval `(progn ,func))))
  (run-hooks 'post-command-hook))

(defun mykie:repeat-p ()
  (equal this-command last-command))

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

(defun* mykie:get-thing-state (thing &key prefix)
  "Return :email, :url state.
The THING is you can specify 'email or 'url.
If the THING is 'email then check whether thing-at-point is email.
Then if there is email, return :email.
If the THING is 'url then check whether thing-at-point is url.
Then if there is url, return :url.
Also you can specify \"C-u&\" or \"region&\" to PREFIX.
If you specify \"C-u&\", check whether there is current-prefix-arg.
If you specify \"region&\", check whether region is active.
If Result is true, then return prefix + thing state such as
:C-u&url(if you specify \"&C-u\" to PREFIX in this case).
You can use `mykie:current-thing' variable to get
result(i.e., email address or url) after call this function."
  (lexical-let
      ((thing-state (mykie:concat-prefix-if-exist thing prefix)))
    (when thing-state
      (case thing
        ((url email)
         (mykie:aif (thing-at-point thing)
                    (setq mykie:current-thing it)
                    (setq mykie:current-thing nil))
         (when mykie:current-thing
           thing-state))))))

(defun mykie:concat-prefix-if-exist (state prefix)
  (lexical-let
      ((target-state (intern (concat ":" prefix (symbol-name state)))))
    (when (and (plist-get mykie:current-args target-state)
               (or (not prefix)
                   (and prefix (equal "C-u&"    prefix)
                        current-prefix-arg)
                   (and prefix (equal "region&" prefix)
                        (region-active-p))))
      target-state)))

(defun mykie:get-comment/string-state ()
  "Return :comment if current point is comment or string face."
  (when (nth 8 (save-excursion (syntax-ppss
                                (if (bobp) (point) (1- (point))))))
    :comment))

(defun mykie:get-major-mode-state (&optional prefix)
  "Return :major-mode, :C-u&major-mode or :region&major-mode if
you specified same state name to `mykie's args and matched
condition if you specified prefix(whether current-prefix-arg or region
active).
The major-mode replaced to `major-mode' name.
You can specify \"C-u&\" or \"region&\" to the PREFIX."
  (mykie:concat-prefix-if-exist major-mode prefix))

(defun mykie:get-prefix-arg-state ()
  "Return keyword like :C-u, :C-*N or :M-N.
If current-prefix-arg is list, return :C-u or :C-u*N(N is replaced to
C-u's pushed times).
If current-prefix-arg is number, return :M-N(the N is replaced number
like M-1.). You can change the M-N's number by pushing M-[0-9] before
call `mykie' function."
  (typecase current-prefix-arg
    (list   (lexical-let
                ((times (mykie:get-C-u-times)))
              (if (= 1 times)
                  :C-u
                (intern (concat ":C-u*" (number-to-string times))))))
    (number (intern (concat ":M-" (number-to-string current-prefix-arg))))))

;; Backward compatibility
(defalias 'mykie:get-C-u-keyword 'mykie:get-prefix-arg-state)

(defun mykie:get-prog-mode-state (&optional prefix)
  "Return :prog, :C-u&prog or :region&prog state if current buffer is
buffer that related programming. You can specify \"C-u&\" or
\"region&\" to PREFIX. You can use this function from Emacs 24.
Because this function utilize `prog-mode-hook'."
  (when mykie:prog-mode-flag
    (mykie:concat-prefix-if-exist 'prog prefix)))

(defun mykie:get-skk-state ()
  "Return SKK(simple kana kanji)'s state.
If `on'(▽) mode then return :skk-on.
If `active'(▼) mode then return :skk-active."
  (when (bound-and-true-p skk-mode)
    (case (bound-and-true-p skk-henkan-mode)
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
      (loop for mode in (plist-get mykie:current-args :ignore-minor-modes)
            if (member mode minor-mode-list)
            do (return t))))

(defun mykie:init (args)
  "Initialize mykie's global-variable before do mykie's command."
  (when (plist-get args :use-C-u-num)
    (mykie:get-C-u-times))
  (setq mykie:current-args args))

(defun mykie:region-init ()
  "Initialize mykie's global-variable for region.
You can use `mykie:region-str' variable that store region string.
If you specified 'kill or 'copy with :region-handle-flag of mykie's args,
then do `kill-region' or `copy-region-as-kill' before do mykie's command.
So you can use kill-ring variable that store region's variable if you want."
  (setq mykie:region-str
        (buffer-substring (region-beginning) (region-end)))
  (case (plist-get mykie:current-args :region-handle-flag)
    (kill (kill-region         (region-beginning) (region-end)))
    (copy (copy-region-as-kill (region-beginning) (region-end)))))

(defun mykie:deactivate-mark ()
  "Deactivate region if you specified :deactivate-region of mykie's
args with non-nil after do mykie's command."
  (lexical-let
      ((deactivation
        (plist-get mykie:current-args :deactivate-region)))
    (when (or (and (eq 'region     deactivation)
                   (eq :region     mykie:current-state))
              (and (eq 'region&C-u deactivation)
                   (eq :region&C-u mykie:current-state))
              (eq      t          deactivation))
      (deactivate-mark))))

(defun* mykie (&rest args &allow-other-keys)
  "Call function you are set functions.
You can set below keyword by default:
*Functions*
:default - this is default function
:default&bolp - call this if pushed key at bolp
:default&eolp - call this if pushed key at eolp
:C-u - call this  if you pushed this key after C-u
:C-u&bolp - call this if you pushed key after C-u and the point was bolp
:C-u&eolp - call this if you pushed key after C-u and the point was eolp
:region - call this if you pushed key when you are selecting region
:region&C-u - similar to above but call this if you pushed C-u before
:repeat - call this if you pushed key at same point
*Flags*
:region-handle-flag - you can set 'copy and 'kill
If you set 'kill then region's string is killed.
If you set 'copy then region's string is copied.
:deactivate-region - deactivate region after region command execution
If you set 'region then deactivate region when you did not push C-u.
If you set 'region&C-u then deactivate region when you pushed C-u.
If you set t then deactivate region in both cases.
You can use `mykie:region-str' variable that have region's string."
  (mykie:init args)
  (loop initially (when (mykie:ignore-mode-p)
                    (return (mykie:execute (plist-get args :default))))
        for condition in mykie:conditions
        for state = (eval condition)
        for func  = (plist-get args state)
        if (member state args) do
        (setq mykie:current-state state)
        (mykie:run-hook 'before)
        (mykie:execute func)
        (mykie:run-hook 'after)
        (return)
        finally (mykie:execute (plist-get args :default)))
  (unless (mykie:repeat-p)
    (setq mykie:current-point (point))))

(defun mykie:run-hook (direction)
  (case direction
    (before
     (when (funcall mykie:region-func-predicate)
       (run-hooks 'mykie:region-before-init-hook)))
    (after
     (when (funcall mykie:region-func-predicate)
       (run-hooks 'mykie:region-after-init-hook)))))

(defun mykie:format-key (key)
  (typecase key
    (vector key)
    (string (kbd key))
    (t (error "Invalid key"))))

(defmacro mykie:define-key (keymap key &rest args)
  "In KEYMAP, define key sequence KEY as `mykie' command with ARGS.
In other words, `mykie' + `define-key'.

Example:
 (mykie:define-key global-map \"y\"
   :default 'self-insert-command
   :region '(message \"%s\" mykie:region-str)
   :C-u '(message \"C-u y\"))"
  `(mykie:define-key-core
    (symbol-name (quote ,keymap))
    ,keymap ,key ,@args))
(put 'mykie:define-key 'lisp-indent-function 2)

(defun mykie:define-key-core (keymap-name keymap key &rest args)
  (unless (memq keymap mykie:keymaps) (push keymap mykie:keymaps))
  (lexical-let* ((key (mykie:format-key key))
                 (args args)
                 ;; Workaround: Assign command name
                 (sym (funcall mykie:make-funcname-function
                               args mykie:keymaps keymap key keymap-name)))
    (fset sym (lambda () (interactive) (apply 'mykie args)))
    (define-key keymap key sym)))

(defun mykie:global-set-key (key &rest args)
  "Give KEY a global binding as `mykie' command.
In other words, `mykie' + `global-set-key'.

Example:
 (mykie:global-set-key \"z\"
   :default 'self-insert-command
   :region '(message \"%s\" mykie:region-str)
   :C-u '(message \"C-u z\"))"
  (apply 'mykie:define-key-core "global-map" global-map key args))
(put 'mykie:global-set-key 'lisp-indent-function 1)

(defun mykie:define-key-with-self-key (key &rest args)
  "Set self-insert-key(KEY) with `mykie' command.
Example:
  (mykie:define-key-with-self-key
      \"a\" :C-u '(message \"I am C-u\"))"
  (apply 'mykie:define-key-core "global-map" global-map (mykie:format-key key)
         (append args
                 '(:default self-insert-command)
                 (mykie:aif mykie:ignore-major-modes-for-self-insert-key
                            `(:ignore-major-modes ,it)
                            nil)
                 (mykie:aif mykie:ignore-minor-modes-for-self-insert-key
                            `(:ignore-minor-modes ,it)
                     nil))))
(put 'mykie:define-key-with-self-key 'lisp-indent-function 1)

(defmacro mykie:set-keys (keymap-or-order &rest args)
  "Set keybinds as `mykie' command.
Examples:
  Set keybinds to global-map:
  (mykie:set-keys nil ; You can set 'global or global-map instead of nil too.
    \"C-a\"
    :default     '(beginning-of-line)
    :C-u         'mark-whole-buffer
    \"C-e\"
    :default     '(end-of-line)
    :C-u         '(message \"Hello\"))

  Set keybinds to specific keymap:
  (mykie:set-keys emacs-lisp-mode-map
    \"C-1\"
    :default '(message \"C-1\")
    :C-u     '(message \"C-1+C-u\")
    \"C-2\"
    :default '(message \"C-2\")
    :C-u     '(message \"C-2+C-u\"))

  Set keybinds for self-insert-key
  You don't need to specify :default state, it's specified to
  'self-insert-command automatically to it.
  (mykie:set-keys 'with-self-key
   \"a\"
   :C-u '(message \"called a\")
   :region 'query-replace-regexp
   \"b\"
   :C-u '(message \"called b\"))"
  `(let ((order (or ,keymap-or-order 'global)))
     (if (keymapp ,keymap-or-order)
         (mykie:set-keys-core
          (symbol-name ,keymap-or-order) order ,keymap-or-order ,@args)
       (mykie:set-keys-core nil order global-map ,@args))))
(put 'mykie:set-keys 'lisp-indent-function 1)

(defun mykie:set-keys-core (keymap-name order keymap &rest args)
  (lexical-let
      ((set-keys (lambda (func &optional keymap)
                   (loop with key-and-prop = '()
                         with last = (1- (length args))
                         for i from 0 to last
                         for next = (1+ i)
                         for key-or-prop = (nth i args)
                         collect key-or-prop into key-and-prop
                         if (or (equal i last)
                                (typecase (nth next args)
                                  (string t)
                                  (vector t)))
                         do (progn
                              (if keymap
                                  (apply func keymap-name keymap key-and-prop)
                                (apply func key-and-prop))
                              (setq key-and-prop nil))))))
    (case order
      (global
       (funcall set-keys 'mykie:global-set-key))
      (with-self-key
       (funcall set-keys 'mykie:define-key-with-self-key))
      (t (funcall set-keys 'mykie:define-key-core keymap)))))

(unless mykie:conditions
  (mykie:initialize))

(provide 'mykie)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mykie.el ends here
