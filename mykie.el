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
;; (defun mykie-sample ()
;;   (interactive)
;;   (mykie
;;    :default 'newline-and-indent
;;    :region  'query-replace-regexp))
;; (global-set-key (kbd "C-j") 'mykie-sample)
;;
;; Above function call newline-and-indent by default,
;; But call query-replace-regexp function if you select region.
;;
;; In short form:
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
          :region))))

(defvar mykie:prefix-arg-conditions
  '((mykie:get-major-mode-state t)
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
    (mykie:get-thing-state   'email)
    (mykie:get-thing-state   'url)
    (when (minibufferp)      :minibuff)
    (when (bobp)             :bobp)
    (when (eobp)             :eobp)
    (when (bolp)             :bolp)
    (when (eolp)             :eolp)))

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

(defvar mykie:region-before-init-hook '(mykie:region-init))
(defvar mykie:region-after-init-hook  '(mykie:deactivate-mark))

(defvar mykie:region-func-predicate
  '(lambda ()
     (and (region-active-p)
          (case mykie:current-state ((:region :region&C-u) t)))))

;; INTERNAL VARIABLES
(defvar mykie:keymaps nil)

;; DYNAMIC VARIABLES
(defvar mykie:current-state nil)
(defvar mykie:current-args '())
(defvar mykie:current-point "")
(defvar mykie:current-thing nil)
(defvar mykie:region-str "")
(defvar mykie:C-u-num nil)

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
  (setq mykie:C-u-num (truncate (log (or (car current-prefix-arg) 1) 4)))
  mykie:C-u-num)

(defalias 'mykie:C-u-num 'mykie:get-C-u-times)

(defmacro mykie:aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun* mykie:get-thing-state (thing &key prefix)
  (lexical-let
      ((thing-state (intern (concat ":" prefix (symbol-name thing)))))
    (when (and (plist-get mykie:current-args thing-state)
               (or (not prefix)
                   (and prefix (equal "C-u&"    prefix)
                        current-prefix-arg)
                   (and prefix (equal "region&" prefix)
                        (region-active-p))))
      (case thing
        ((url email)
         (mykie:aif (thing-at-point thing)
                    (setq mykie:current-thing it)
                    (setq mykie:current-thing nil))
         (when mykie:current-thing
           thing-state))))))

(defun mykie:get-major-mode-state (&optional C-u-prefix)
  (when (or (and C-u-prefix
                 current-prefix-arg)
            (and (not C-u-prefix)
                 (null current-prefix-arg)))
    (lexical-let
        ((prefix (if C-u-prefix ":C-u&" ":")))
      (intern (concat prefix (symbol-name major-mode))))))

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

(defun mykie:get-skk-state ()
  (when (bound-and-true-p skk-mode)
    (case (bound-and-true-p skk-henkan-mode)
      (active :skk-active)
      (on     :skk-on))))

(defun mykie:init (args)
  (when (plist-get args :use-C-u-num)
    (mykie:get-C-u-times))
  (setq mykie:current-args args))

(defun mykie:region-init ()
  (setq mykie:region-str
        (buffer-substring (region-beginning) (region-end)))
  (case (plist-get mykie:current-args :region-handle-flag)
    (kill (kill-region         (region-beginning) (region-end)))
    (copy (copy-region-as-kill (region-beginning) (region-end)))))

(defun mykie:deactivate-mark ()
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
  (loop for condition in mykie:conditions
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

(defun mykie:define-key (keymap key &rest args)
  "In KEYMAP, define key sequence KEY as `mykie' command with ARGS.
In other words, `mykie' + `define-key'.

Example:
 (mykie:define-key global-map \"y\"
   :default 'self-insert-command
   :region '(message \"%s\" mykie:region-str)
   :C-u '(message \"C-u y\"))"
  (unless (memq keymap mykie:keymaps) (push keymap mykie:keymaps))
  (lexical-let* ((key (mykie:format-key key)) 
                 (args args)
                 ;; Workaround: Assign command name
                 (sym (intern (format 
                               "mykie:%s:%s:%s"
                               ;; Some programs check command name by
                               ;; (string-match "self-insert-command" command-name)
                               (if (eq (plist-get args :default) 'self-insert-command)
                                   "self-insert-command" "key")
                               ;; find keymap index
                               (loop for k in mykie:keymaps
                                     for i from 0
                                     if (eq k keymap) 
                                     return (- (length mykie:keymaps) i))
                               ;; Use unique string for key(string or vector)
                               (sha1 (format "%S" key))))))
    (fset sym (lambda () (interactive) (apply 'mykie args)))
    (define-key keymap key sym)))
(put 'mykie:define-key 'lisp-indent-function 2)

(defun mykie:global-set-key (key &rest args)
  "Give KEY a global binding as `mykie' command.
In other words, `mykie' + `global-set-key'.

Example:
 (mykie:global-set-key \"z\"
   :default 'self-insert-command
   :region '(message \"%s\" mykie:region-str)
   :C-u '(message \"C-u z\"))"
  (apply 'mykie:define-key global-map key args))
(put 'mykie:global-set-key 'lisp-indent-function 1)

(defun mykie:define-key-with-self-key (key &rest args)
  "Set self-insert-key(KEY) with `mykie' command.
Example:
  (mykie:define-key-with-self-key
      \"a\" :C-u '(message \"I am C-u\"))"
  (apply 'mykie:define-key global-map (mykie:format-key key)
         (append args '(:default self-insert-command))))
(put 'mykie:define-key-with-self-key 'lisp-indent-function 1)

(defun mykie:set-keys (direction &rest args)
  "Set keybinds as `mykie' command.
Examples:
  Set keybinds to global-map:
  (mykie:set-keys 'global
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
  (lexical-let
      ((set-keys (lambda (func &optional keymap)
                   (loop with tmp = '()
                         with last = (1- (length args))
                         for i from 0 to last
                         for next = (1+ i)
                         for key-or-prop = (nth i args)
                         collect key-or-prop into tmp
                         if (or (equal i last)
                                (typecase (nth next args)
                                  (string t)
                                  (vector t)))
                         do (progn
                              (if keymap
                                  (apply func keymap tmp)
                                (apply func tmp))
                              (setq tmp nil))))))
    (case direction
      (global
       (funcall set-keys 'mykie:global-set-key))
      (with-self-key
       (funcall set-keys 'mykie:define-key-with-self-key))
      (t (if (keymapp direction)
             (funcall set-keys 'mykie:define-key direction)
           (error "Key parse failed, make sure your setting"))))))
(put 'mykie:set-keys 'lisp-indent-function 1)

(unless mykie:conditions
  (mykie:initialize))

(provide 'mykie)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mykie.el ends here
