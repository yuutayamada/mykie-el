;;; helm-mykie-keywords.el --- Show mykie's keywords by helm.el

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>

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

;;; Code:
(eval-when-compile (require 'cl))
(require 'mykie)
(require 'helm)

(defvar helm-mykie-keywords-source nil)
(defvar helm-mykie-keywords-action
  '(("Insert" .
     (lambda (keyword) (insert keyword)))))

(defun mykie:set-helm-mykie-keyword ()
  (setq helm-mykie-keywords-source
        (loop with base = `((candidates-in-buffer)
                            (action . ,helm-mykie-keywords-action))
              with get-default = (lambda (cond-sym)
                                   (if (equal 'mykie:normal-conditions cond-sym)
                                       mykie:default-keywords
                                     (car
                                      (assoc-default
                                       cond-sym
                                       mykie:default-condition-keyword-alist))))
              with make = (lambda (cond-sym)
                            (loop with format = (lambda (kw)
                                                  (typecase kw
                                                    (symbol (symbol-name kw))
                                                    (string kw)))
                                  with default-kw = (funcall get-default cond-sym)
                                  with keywords
                                  for (kw . c) in (symbol-value cond-sym)
                                  collect (funcall format kw) into keywords
                                  finally return (append keywords default-kw)))
              for cond in mykie:group-conditions
              for keywords = (funcall make cond)
              for name = (list (cons 'name (symbol-name cond)))
              for func = `(lambda ()
                            (helm-init-candidates-in-buffer
                             ,cond
                             (with-temp-buffer
                               (loop for kw in (quote ,keywords)
                                     do (insert (format "%s\n" kw)))
                               (buffer-string))))
              collect (append base name
                              `((init . ,func))))))

;;;###autoload
(defun helm-show-mykie-keywords ()
  "Show mykie.el keywords."
  (interactive)
  (unless helm-mykie-keywords-source
    (mykie:set-helm-mykie-keyword))
  (helm
   :sources helm-mykie-keywords-source
   :buffer "*helm mykie*"))

(provide 'helm-mykie-keywords)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; helm-mykie-keywords.el ends here
