;;; mykie.el --- user keybinding support tool for Emacs

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/mykie
;; Version: 0.0.5
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
;; You can see more example : https://github.com/yuutayamada/mykie-el
;;; Code:
(eval-when-compile (require 'cl))

;; CUSTOMIZE VARIABLE
(defvar mykie:conditions
  '((when (region-active-p)
      (or (and current-prefix-arg
               :region&C-u)
          :region))
    (when current-prefix-arg
      (or (and (eobp)        :C-u&eobp)
          (and (bobp)        :C-u&bobp)))
    (when current-prefix-arg
      (or (and (bolp)        :C-u&bolp)
          (and (eolp)        :C-u&eolp)))
    (mykie:get-C-u-keyword)
    (when current-prefix-arg :C-u) ; Use :C-u if C-u*X isn't exists
    (when (mykie:repeat-p)   :repeat)
    (when (minibufferp)      :minibuff)
    (when (bobp)             :bobp)
    (when (eobp)             :eobp)
    (when (bolp)             :bolp)
    (when (eolp)             :eolp))
  "This variable is evaluated in mykie's loop by each the when statement.
Then if the when statement is match, return value(like :C-u) and then
same keyword's function that you are specified is evaluated.
Note: Order is important. Above list element have more priority than
below elements. If you dislike :repeat's priority, then you can change
this behavior by this variable.")

(defvar mykie:region-before-init-hook '(mykie:region-init))
(defvar mykie:region-after-init-hook  '(mykie:deactivate-mark))

(defvar mykie:region-func-predicate
  '(lambda ()
     (and (region-active-p)
          (case mykie:current-state ((:region :region&C-u) t)))))

;; DYNAMIC VARIABLES
(defvar mykie:current-state nil)
(defvar mykie:current-args '())
(defvar mykie:current-point "")
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
  \"g\" (if current-prefix-arg
            (keyboard-quit)))"
  (run-hooks 'pre-command-hook)
  (cond
   ((commandp func)
    (unwind-protect
        (funcall 'call-interactively func)
      (setq last-command func)))
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

(defun mykie:get-C-u-keyword ()
  "Return :C-u or :C-u*X(X is replaced to C-u's pushed times)."
  (lexical-let
      ((times (mykie:get-C-u-times)))
    (if (= 1 times)
        :C-u
      (intern (concat ":C-u*" (number-to-string times))))))

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

(provide 'mykie)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mykie.el ends here
