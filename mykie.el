;;; mykie.el --- Command multiplexer: Register multiple functions to a keybind

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/mykie-el
;; Version: 0.1.0
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
(defvar mykie:region-conditions-base
  '((:region&C-u :region&prog :region&err :region)
    (or (and current-prefix-arg
             :region&C-u)
        (mykie:get-error-state "region&")
        (mykie:get-major-mode-state "region&")
        (mykie:get-prog-mode-state "region&")
        :region)))

(defvar mykie:prefix-arg-conditions-base
  '((:C-u&err :C-u&prog :C-u&email :C-u&url :C-u
              :C-u&bobp :C-u&eobp :C-u&bolp :C-u&eolp)
    (mykie:get-error-state "C-u&")
    (mykie:get-major-mode-state "C-u&")
    (mykie:get-prog-mode-state "C-u&")
    (mykie:get-thing-state 'email :prefix "C-u&")
    (mykie:get-thing-state 'url   :prefix "C-u&")
    (or (and (eobp)        :C-u&eobp)
        (and (bobp)        :C-u&bobp))
    (or (and (bolp)        :C-u&bolp)
        (and (eolp)        :C-u&eolp))
    (mykie:get-prefix-arg-state)
    ;; Use :C-u if C-u*N isn't exists
    :C-u))

(defvar mykie:normal-conditions-base
  '((:repeat :err :minibuff :bolp :eolp :bobp :eobp :readonly
             :url :email :comment :prog)
    (when (mykie:repeat-p)   :repeat)
    (mykie:get-error-state)
    (mykie:get-major-mode-state)
    (mykie:get-prog-mode-state)
    (mykie:get-comment/string-state)
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

(defvar mykie:region-conditions)
(defvar mykie:prefix-arg-conditions)
(defvar mykie:normal-conditions)

(defvar mykie:conditions '()
  "This variable is evaluated in mykie's loop by each the when statement.
Then if the when statement is match, return value(like :C-u) and then
same keyword's function that you are specified is evaluated.
Note: Order is important. Above list element have more priority than
below elements. If you dislike :repeat's priority, then you can change
this behavior by this variable.")

(defvar mykie:use-major-mode-key-override nil
  "If this variable is non-nil, attach mykie's same global key function
to major-mode's key function if the function is exists.
You can specify 'both, 'global, 'self or t to this variable.
'both means do override self-insert-keys and other global-keys.
'global means do override  other global-keys only(such as C-j, not [a-z]).
'self means do override self-insert-keys only.
t means same as 'self. See also `mykie:attach-mykie-func-to'")

(defvar mykie:use-lazy-order nil
  "If this variable is non-nil, you can change execution order when
  you register key-bindings.
This function is convenience if you want to change keybind order by each keybind.
For example:
 (mykie:set-keys nil
   \"C-0\"
   :default '(message \"hi\")
   :C-u*2   '(message \"howdy\")
   :C-u     '(message \"hello\")
   :C-u*3   '(message \"hey\") ; <- you can't see
   \"C-1\"
   :default '(message \"hi\")
   :C-u*3   '(message \"howdy\")
   :C-u     '(message \"hello\")
   :C-u*2   '(message \"hey\")) ; <- you can't see
This example you never be able to see hey message. Because `mykie' is prior
above keyword and function pair if you set non-nil to this variable.
Note: you can change order only same section's conditions.
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

(defvar mykie:get-fallback-function
  (lambda (args)
    (and (mykie:ignore-mode-p)
         ;; Return default function
         (plist-get args :default)))
  "Fallback function that retiring fallback function's symbol.")

;; INTERNAL VARIABLES
(defvar mykie:keymaps nil)
(defvar mykie:current-state nil)
(defvar mykie:current-args '())
(defvar mykie:current-point "")
(defvar mykie:current-thing nil)
(defvar mykie:region-str "")
(defvar mykie:C-u-num nil)

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
  (mykie:run-hook 'before)
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
  (run-hooks 'post-command-hook)
  (mykie:run-hook 'after))

(defun mykie:attach-mykie-func-to (&optional mode-symbol)
  "Attach mykie's functions to the MODE's same key function without :default.
Use the MODE's function as :default function.
If you didn't specify the MODE, then use current major-mode by default.
The MODE is mode name's symbol such as 'emacs-lisp-mode."
  (interactive)
  (lexical-let*
      ((mode (or mode-symbol major-mode))
       (attach-func
        (lambda (keys)
          (loop with keymap-name = (concat (symbol-name mode) "-map")
                with keymap      = (eval (intern keymap-name))
                for (key args) in keys
                for mode-func = (lookup-key keymap key)
                if (and (keymapp keymap)
                        (functionp mode-func)
                        (not (string-match "^mykie:" (symbol-name mode-func))))
                do (mykie:clone-key
                    key args `(:default ,mode-func) `(,keymap-name . ,keymap))))))
    (condition-case err
        (if (member mode mykie:attached-mode-list)
            (error (format "Mykie: already attached %s" (symbol-name mode)))
          (case mykie:use-major-mode-key-override
            (both   (funcall attach-func mykie:global-keys)
                    (funcall attach-func mykie:self-insert-keys))
            (global (funcall attach-func mykie:global-keys))
            (self   (funcall attach-func mykie:self-insert-keys))
            (t      (funcall attach-func mykie:self-insert-keys)))
          (add-to-list 'mykie:attached-mode-list mode))
      (error err))))

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
(put 'mykie:aif 'lisp-indent-function 2)

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

(defun mykie:get-error-state (&optional prefix)
  "Return :err, :C-u&err or :region&err if `flycheck-current-errors' or
`flymake-err-info' is non-nil.
You can specify \"C-u&\" or \"region&\" to the PREFIX."
  (when (or (bound-and-true-p flymake-err-info)
            (bound-and-true-p flycheck-current-errors))
    (mykie:concat-prefix-if-exist 'err prefix)))

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

(defun mykie:major-mode-key-exist-p (args)
  "Return function symbol if same key as the major-mode's key exists."
  (destructuring-bind (key . keymap) (plist-get args :key-info)
    (lookup-key
     (eval (intern (concat (symbol-name major-mode) "-map")))
     (kbd key))))

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

(defun mykie:initialize ()
  (setq mykie:region-conditions
        (mykie:marge-conditions mykie:before-user-region-conditions
                                mykie:region-conditions-base
                                mykie:after-user-region-conditions)
        mykie:prefix-arg-conditions
        (mykie:marge-conditions mykie:before-user-prefix-arg-conditions
                                mykie:prefix-arg-conditions-base
                                mykie:after-user-prefix-arg-conditions)
        mykie:normal-conditions
        (mykie:marge-conditions mykie:before-user-normal-conditions
                                mykie:normal-conditions-base
                                mykie:after-user-normal-conditions))
  (setq mykie:conditions '(mykie:region-conditions
                           mykie:prefix-arg-conditions
                           mykie:normal-conditions))
  (if mykie:use-major-mode-key-override
      (add-hook  'change-major-mode-after-body-hook 'mykie:attach-mykie-func-to)
    (remove-hook 'change-major-mode-after-body-hook 'mykie:attach-mykie-func-to)))

(defun mykie:marge-conditions (&rest conditions)
  (loop with merge = (lambda (p s)
                       (cons (append (car p) (car s))
                             (append (cdr p) (cdr s))))
        with merged = '()
        with last = (1- (length conditions))
        for i from 0 to last
        for next = (1+ i)
        for primary   = (nth i conditions) then (or merged (nth i conditions))
        for secondary = (nth next conditions)
        if (and (mykie:style-valid-p primary)
                (mykie:style-valid-p secondary))
        do (setq merged (funcall merge primary secondary))
        else if (mykie:style-valid-p primary)
        do (setq merged primary)
        else if (mykie:style-valid-p secondary)
        do (setq merged secondary)
        finally return merged))

(defun mykie:style-valid-p (conditions)
  "Check CONDITIONS is whether valid list.
Valid CONDITIONS is list that first element is list of keywords and
remaining elements are conditions that return keyword like :C-u if
condition succeed."
  (when conditions
    (destructuring-bind (appendixes . conds) conditions
      (if (and (listp appendixes) (listp conds)
               (loop for appendix in appendixes
                     if (not (keywordp appendix)) do
                     (return nil)
                     finally return t))
          t
        (message "mykie: condition style is not valid.")
        nil))))

(defun mykie:init (args)
  "Initialize mykie's global-variable before do mykie's command."
  (when (plist-get args :use-C-u-num)
    (mykie:get-C-u-times))
  (setq mykie:current-args args)
  (lexical-let
      ((fallback (funcall mykie:get-fallback-function args)))
    (when fallback
      (mykie:execute fallback)
      'exit)))

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
:bolp - call this if pushed key at bolp
:eolp - call this if pushed key at eolp
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
  (loop initially (when (eq 'exit (mykie:init args)) (return))
        with keywords = (loop for arg in args
                              if (keywordp arg)
                              collect arg)
        for conditions in mykie:conditions
        for cond-str = (symbol-name conditions)
        if (mykie:precheck-ok-p cond-str)
        do (when (eq 'done (mykie:iter args (eval conditions) keywords))
             (return)) ; exit from loop macro
        finally (mykie:execute (plist-get args :default)))
  (unless (mykie:repeat-p)
    (setq mykie:current-point (point))))

(defun mykie:iter (args conditions keywords)
  "Return 'done symbol if this function execute command or function.
If this function didn't execute command or function. Then `mykie' will
call this function again or execute :default function from `mykie'."
  (catch 'done
    (loop with appendixes = (car conditions)
          with conds      = (cdr conditions)
          for keyword in keywords
          for kw-str = (symbol-name keyword)
          if (or (member keyword appendixes)
                 (or (string-match "^:.+-mode$"      kw-str)
                     (string-match "^:M-[0-9]+$"     kw-str)
                     (string-match "^:C-u\\*[0-9]+$" kw-str)))
          do (loop for condition in conds
                   for state = (eval condition)
                   if (mykie:match-p state keyword args)
                   do (progn (setq mykie:current-state state)
                             (mykie:execute (plist-get args state))
                             (throw 'done 'done))))))

(defun mykie:match-p (state keyword args)
  "Return non-nil if ARGS is containing STATE.
If you set non-nil to `mykie:use-lazy-order', then this function check
whether STATE is equal to KEYWORD.
See also `mykie:use-lazy-order' variable."
  (and (or (null mykie:use-lazy-order)
           (and mykie:use-lazy-order
                (eq keyword state)))
       (member state args)))

(defun mykie:precheck-ok-p (cond-str)
  "Pre-check condition depending on state.
If COND-STR is containing \"mykie:region\" then check whether region is active.
If COND-STR is containing \"mykie:prefix-arg\" then check
`current-prefix-arg' is non-nil. Otherwise check whether containing
\"mykie:normal\"."
  (or (and (region-active-p)
           (string-match "^mykie:region" cond-str))
      (and current-prefix-arg
           (string-match "^mykie:prefix-arg" cond-str))
      (string-match "^mykie:normal" cond-str)))

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
                 (args (append args `(:key-info (,key . ,keymap-name))))
                 ;; Workaround: Assign command name
                 (sym (funcall mykie:make-funcname-function
                               args mykie:keymaps keymap key keymap-name)))
    (when (and (equal "global-map" keymap-name)
               (< 1 (length (key-description key))))
      (add-to-list 'mykie:global-keys `(,key ,args)))
    (fset sym (lambda () (interactive) (apply 'mykie args)))
    (define-key keymap key sym)
    (mykie:aif (plist-get args :clone)
        (mykie:clone-key it args '(:default self-insert-command)))))

(defun mykie:clone-key (key args default-keyword-and-func &optional keymap-info)
  (lexical-let
      ((new-args
        (mykie:filter (mykie:replace-property args default-keyword-and-func)
                      :clone))
       (map-name (or (car keymap-info) "global-map"))
       (map      (or (cdr keymap-info)  global-map)))
    (apply 'mykie:define-key-core map-name map key new-args)))

(defun mykie:replace-property (args key-and-property)
  (append (mykie:filter args (car key-and-property))
          key-and-property))

(defun mykie:filter (args keyword)
  "Delete KEYWORD and the KEYWORD's function or property."
  (if (not (member keyword args))
      args
    (loop with last = (1- (length args))
          with result = '()
          with ignore = nil
          for i from 0 to last
          if (eq keyword (nth i args))
          do (push (1+ i) ignore)
          else if (not (member i ignore))
          collect (nth i args))))

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
This function register :default `self-insert-command' automatically to ARGS.
Example:
  (mykie:define-key-with-self-key
      \"a\" :C-u '(message \"I am C-u\"))"
  (add-to-list 'mykie:self-insert-keys `(,key ,args))
  (apply 'mykie:define-key-core "global-map" global-map (mykie:format-key key)
         (append args
                 '(:default self-insert-command)
                 (mykie:aif mykie:major-mode-ignore-list
                     `(:ignore-major-modes ,it)
                   nil)
                 (mykie:aif mykie:minor-mode-ignore-list
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
         (mykie:set-keys-core order ,keymap-or-order ,@args)
       (mykie:set-keys-core order global-map ,@args))))
(put 'mykie:set-keys 'lisp-indent-function 1)

(defun mykie:set-keys-core (order keymap &rest args)
  (lexical-let
      ((set-keys (lambda (func)
                   (loop with key-and-prop = '()
                         with last = (1- (length args))
                         with keymap-name = (quote keymap)
                         for i from 0 to last
                         for next = (1+ i)
                         for key-or-prop = (nth i args)
                         collect key-or-prop into key-and-prop
                         if (or (equal i last)
                                (and (not (eq :clone (nth i args)))
                                     (typecase (nth next args)
                                       (string t)
                                       (vector t))))
                         do (progn
                              (case func
                                (mykie:define-key-core
                                 (apply func keymap-name keymap key-and-prop))
                                (t (apply func key-and-prop)))
                              (setq key-and-prop nil))))))
    (case order
      (global
       (funcall set-keys 'mykie:global-set-key))
      (with-self-key
       (funcall set-keys 'mykie:define-key-with-self-key))
      (t (funcall set-keys 'mykie:define-key-core)))))

(unless mykie:conditions
  (mykie:initialize))

(provide 'mykie)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mykie.el ends here
