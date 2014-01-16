;; mykie:define-key
(ert-deftest mykie-define-key ()
  (mykie:define-key global-map "C-0"
    :default (setq test-result 'default)
    :C-u     (setq test-result 'C-u))
  (cmds-do '(default C-u) global-map "C-0"))

(ert-deftest mykie-define-key-paren ()
  (mykie:define-key global-map "C-0"
    (:default (message "")
              (setq test-result 'default))
    (:C-u     (message "")
              (setq test-result 'C-u)))
  (cmds-do '(default C-u) global-map "C-0"))

;; mykie:global-set-key
(ert-deftest mykie-global-set-key ()
  (mykie:global-set-key "C-0"
    :default (setq test-result 'default)
    :C-u     (setq test-result 'C-u))
  (cmds-do '(default C-u) global-map "C-0"))

(ert-deftest mykie-global-set-key-paren ()
  (mykie:global-set-key "C-0"
    (:default (message "")
              (setq test-result 'default))
    (:C-u     (message "")
              (setq test-result 'C-u)))
  (cmds-do '(default C-u) global-map "C-0"))

;; mykie:define-key-with-self-key
(ert-deftest mykie-define-key-with-self-key ()
  (mykie:define-key-with-self-key "a"
    :C-u (setq test-result 'C-u))
  (cmds-do '(C-u) global-map "a"))

(ert-deftest mykie-define-key-with-self-key-paren ()
  (mykie:define-key-with-self-key "a"
    (:C-u (message "")
          (setq test-result 'C-u)))
  (cmds-do '(C-u) global-map "a"))

;; mykie:set-keys
(ert-deftest mykie-set-keys ()
  (mykie:set-keys global-map
    ;; PAREN
    "C-0"
    (:default (message "")
              (setq test-result 'default))
    (:C-u     (message "")
              (setq test-result 'C-u))
    "C-9"
    (:default (message "")
              (setq test-result 'default))
    (:C-u     (message "")
              (setq test-result 'C-u))
    ;; NORMAL
    "C-1"
    :default  (setq test-result 'default)
    :C-u      (setq test-result 'C-u))
  (cmds-do '(default C-u) global-map "C-0")
  (cmds-do '(default C-u) global-map "C-9")
  (cmds-do '(default C-u) global-map "C-1"))

;; mykie:combined-command
(ert-deftest mykie-combined-command ()
  (global-set-key (kbd "C-4")
                  (mykie:combined-command
                   :default (setq test-result 'default)
                   :C-u     (setq test-result 'C-u)))
  (cmds-do '(default C-u) global-map "C-4"))

(ert-deftest mykie-combined-command-paren ()
  (global-set-key (kbd "C-5")
                  (mykie:combined-command
                   (:default (message "")
                             (setq test-result 'default))
                   (:C-u     (message "")
                             (setq test-result 'C-u))))
  (cmds-do '(default C-u) global-map "C-5"))

;; mykie
(ert-deftest mykie ()
  (global-set-key (kbd "C-3")
                  (lambda ()
                    (interactive)
                    (mykie :default (setq test-result 'default)
                           :C-u     (setq test-result 'C-u))))
  (cmds-do '(default C-u) global-map "C-3"))

(ert-deftest mykie-paren ()
  (global-set-key (kbd "C-3")
                  (lambda ()
                    (interactive)
                    (mykie
                     (:default (message "")
                               (setq test-result 'default))
                     (:C-u     (message "")
                               (setq test-result 'C-u)))))
  (cmds-do '(default C-u) global-map "C-3"))
