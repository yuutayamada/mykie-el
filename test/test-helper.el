(eval-when-compile (require 'cl))
(require 'ert)
(require 'mykie)

(defvar test-result nil)

(defun cmd-do (expect map key)
  (setq test-result nil)
  (when (eq 'C-u expect)
    (setq current-prefix-arg '(4)))
  (call-interactively (lookup-key map (kbd key)))
  (should (eq expect test-result)))

(defun cmds-do (expects map key)
  (loop initially (setq current-prefix-arg nil)
        for expect in expects
        do (cmd-do expect map key)))
