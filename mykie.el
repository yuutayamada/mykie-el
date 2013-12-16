;;; mykie.el --- user keybinding support tool for Emacs

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/mykie
;; Version: 0.0.1
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
(require 'thingatpt)

(defvar mykie:condition-list
  `((c-mode    ("<"  . ".h>"))
    (jade-mode ("#{" . "}"))
    (html-mode ("<"  . ">"))
    (t         ("\"" . "\""))))

(defvar mykie:url
  "https://www.google.com/search?newwindow=1&q=")

(defvar mykie:current-point "")
(defvar mykie:region-str "")
(defvar mykie:current-thing nil)
(defvar mykie:C-u-num nil)

(defun mykie:browse-url ()
  (browse-url
   (concat mykie:url "\"" mykie:region-str "\"")))

(defun mykie:query-x-times (type)
  (when (equal type :number)
    (string-to-number
     (read-from-minibuffer "Input x times: "))))

(defun mykie:replace-regexp (&optional direction)
  (case direction
    (word (setq current-prefix-arg nil))
    (t    (setq current-prefix-arg nil)))
  (call-interactively 'query-replace-regexp))

(defun mykie:get-C-u-state (C-u-arg use-C-u-num default)
  (lexical-let
      ((arg (if (and use-C-u-num (or C-u-arg (null default)))
                (mykie:get-C-u-times)
              C-u-arg)))
    (typecase arg
      (null   :default)
      (number :C-u-num)
      (list   :C-u-list-num))))

(defun mykie:backword (type)
  (let ((regexp
         (case type
           (:number "[0-9]")
           (:string "[a-zA-Z0-9ぁ-んァ-ン上-黑]"))))
    (if (looking-at "[ \n]")
        (backward-char))
    (while (looking-at regexp)
      (backward-char))
    (forward-char)))

(defun mykie:kill-or-copy-region (&optional region-handle-flag)
  (case region-handle-flag
    (kill (kill-region         (region-beginning) (region-end)))
    (t    (copy-region-as-kill (region-beginning) (region-end)))))

(defun mykie:replace-string (word)
  (let* ((separator
          (loop for (mode . sep) in mykie:condition-list
                if (or (equal major-mode mode) (equal t mode))
                do (return (car sep)))))
    (mykie:backword :string)
    (kill-word 1)
    (insert (car separator) word (cdr separator))))

(defun mykie:replace-number (number)
  (mykie:backword :number)
  (kill-word 1)
  (insert (number-to-string number))
  (backward-char))

(defun mykie:get-thing (thing-type)
  ""
  (lexical-let* ((type  (or thing-type 'word))
                 (thing (thing-at-point type)))
    (case type
      ('word
       (when thing
         (if (< 0 (string-to-number thing))
             (cons :number (string-to-number thing))
           (cons :string thing)))))))

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

(defun* mykie:execute-from-functions (function &optional bolp-func eolp-func
                                              &key default)
  (cond ((and bolp-func (bolp)) (mykie:execute bolp-func))
        ((and eolp-func (eolp)) (mykie:execute eolp-func))
        (t (mykie:execute (or function default)))))

(defun mykie:repeat-p ()
  (equal this-command last-command))

(defun mykie:execute-by-type (default thing-type
                             C-u&string C-u&string&bolp C-u&string&eolp
                             C-u&number C-u&number&bolp C-u&number&eolp)
  (setq mykie:current-thing (mykie:get-thing thing-type))
  (case (car mykie:current-thing)
    (:string
     (mykie:execute-from-functions
      C-u&string
      C-u&string&bolp C-u&string&eolp
      :default default))
    (:number
     (mykie:execute-from-functions
      C-u&number
      C-u&number&bolp C-u&number&eolp
      :default default))))

(defun mykie:execute-by-pushed-times (func-list)
  (loop with num = (truncate (log (or (car current-prefix-arg) 1) 4))
        for i from 0 to (length func-list)
        if (and (equal num 0))
        do (error "This function have to call when :C-u-only")
        else if (and (equal num i))
        do (mykie:execute (nth (1- num) func-list))))

(defun mykie:default (default default&bolp default&eolp repeat)
  (if (and repeat (mykie:repeat-p))
      (mykie:execute repeat)
    (mykie:execute-from-functions
     default default&bolp default&eolp)))

(defun mykie:region (C-u region-handle-flag region region&C-u default deactivate-region)
  (setq mykie:region-str (buffer-substring (region-beginning) (region-end)))
  (when region-handle-flag
    (mykie:kill-or-copy-region region-handle-flag))
  (cond ((and region&C-u C-u)
         (mykie:execute region&C-u))
        (region
         (mykie:execute region))
        (t (mykie:execute default)))
  (mykie:deactivate-region-maybe C-u deactivate-region  region region&C-u))

(defun mykie:deactivate-region-maybe (C-u deactivate-region region region&C-u)
  (when (or (and (eq deactivate-region 'region) (not C-u) region)
            (and (eq deactivate-region 'region&C-u) C-u region&C-u)
            (and (eq deactivate-region t) (or region region&C-u)))
    (deactivate-mark)))

(defun mykie:get-C-u-times ()
  (setq mykie:C-u-num (truncate (log (or (car current-prefix-arg) 1) 4)))
  mykie:C-u-num)

(defun* mykie (&key
               default default&bolp default&eolp
               C-u C-u&bolp C-u&eolp
               C-u&string C-u&string&bolp C-u&string&eolp
               C-u&number C-u&number&bolp C-u&number&eolp
               region region-handle-flag region&C-u
               repeat thing-type use-C-u-num deactivate-region)
  "Call function you are set functions.
You can set below keyword:
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
:use-C-u-num - if you set non-nil to this, then you can use
`mykie:C-u-num' variable that have number of C-u's pushed times(i.e.,
prefix-argument).
:region-handle-flag - you can set 'copy and 'kill
If you set 'kill then region's string is killed.
If you set 'copy then region's string is copied.
:deactivate-region - deactivate region after region command execution
If you set 'region then deactivate region when you did not push C-u.
If you set 'region&C-u then deactivate region when you pushed C-u.
If you set t then deactivate region in both cases.
You can use `mykie:region-str' variable that have region's string."
  (interactive)
  (lexical-let*
      ((mykie:arg current-prefix-arg)
       (mykie:C-u-state (mykie:get-C-u-state mykie:arg use-C-u-num default)))
    (if (region-active-p) ; For region
        (mykie:region mykie:arg region-handle-flag region region&C-u default deactivate-region)
      (case mykie:C-u-state
        (:default      ; not pushed C-u
         (mykie:default default default&bolp default&eolp repeat))
        (:C-u-list-num ; pushed C-u
         (cond (C-u
                (mykie:execute C-u))
               ((or (and C-u&bolp (bolp))
                    (and C-u&eolp (eolp)))
                (mykie:execute-from-functions nil C-u&bolp C-u&eolp))
               (t
                (mykie:execute-by-type
                 default thing-type
                 C-u&string C-u&string&bolp C-u&string&eolp
                 C-u&number C-u&number&bolp C-u&number&eolp))))
        (:C-u-num ; if set t to use-C-u-num
         (when C-u (mykie:execute C-u))))))
  (unless (mykie:repeat-p)
    (setq mykie:current-point (point))))

(provide 'mykie)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mykie.el ends here
