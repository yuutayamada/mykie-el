
;;; Code:

;; basic keywords
(ert-deftest test-core-keywords ()
  "should bind :default, :C-u and :region correctly"
  (mykie:define-key global-map "C-0"
    :default     (setq test-result 'default)
    :C-u         (setq test-result 'C-u)
    :region      (setq test-result 'region))
  (cmds-do '(default C-u region) global-map "C-0"))


(ert-deftest test-bob-eob-bol-eol ()
  "should bind :bobp, :eobp, :bolp, :eolp, :C-u&bobp, :C-u&eobp,
:C-u&bolp :C-u&eolp correctly"
  (mykie:define-key global-map "C-0"
    :bobp        (setq test-result 'bobp)
    :eobp        (setq test-result 'eobp)
    :bolp        (setq test-result 'bolp)
    :eolp        (setq test-result 'eolp)
    :C-u&bobp    (setq test-result 'C-u&bobp)
    :C-u&eobp    (setq test-result 'C-u&eobp)
    :C-u&bolp    (setq test-result 'C-u&bolp)
    :C-u&eolp    (setq test-result 'C-u&eolp))
  (cmds-do '(bobp eobp bolp eolp
             C-u&bobp C-u&eobp C-u&bolp C-u&eolp)
           global-map "C-0"))

(ert-deftest test-err ()
  "should bind :err, :C-u&err and :region&err correctly"
  (mykie:define-key global-map "C-0"
    :err         (setq test-result 'err)
    :C-u&err     (setq test-result 'C-u&err)
    :region&err  (setq test-result 'region&err))
  (cmds-do '(err C-u&err region&err)
           global-map "C-0"))

(ert-deftest test-commnet ()
  "should bind :comment correctly"
  (mykie:define-key global-map "C-0" :comment (setq test-result 'comment))
  (cmds-do '(comment) global-map "C-0"))

(ert-deftest test-email ()
  "should bind :email correctly"
  (mykie:define-key global-map "C-0"
    :email (setq test-result 'email)
    :C-u&email (setq test-result 'C-u&email))
  (cmds-do '(email C-u&email) global-map "C-0"))

(ert-deftest test-url ()
  "should bind :url correctly"
  (mykie:define-key global-map "C-0"
    :url (setq test-result 'url)
    :C-u&url (setq test-result 'C-u&url))
  (cmds-do '(url C-u&url) global-map "C-0"))

(ert-deftest test-prog ()
  "should bind :prog correctly"
  (mykie:define-key global-map "C-0"
    :prog (setq test-result 'prog)
    :C-u&prog (setq test-result 'C-u&prog)
    :region&prog (setq test-result 'region&prog))
  (cmds-do '(prog C-u&prog region&prog) global-map "C-0"))

(ert-deftest test-readonly ()
  "should bind :readonly correctly"
  (mykie:define-key global-map "C-0"
    :readonly    (setq test-result 'readonly))
  (cmds-do '(readonly) global-map "C-0"))

(ert-deftest test-file ()
  "should bind :file correctly"
  (mykie:define-key global-map "C-0"
    :file      (setq test-result 'file)
    :C-u&file  (setq test-result 'C-u&file))
  (cmds-do '(file) global-map "C-0"))

(ert-deftest test-C-u ()
  (mykie:define-key global-map "C-0"
    :C-u*2       (setq test-result 'C-u*2)
    :region&C-u  (setq test-result 'region&C-u))
  (cmds-do '(C-u*2 region&C-u) global-map "C-0"))

(ert-deftest test-M-N ()
  (mykie:define-key global-map "C-0"
    :M-1         (setq test-result 'M-1)
    :M-2         (setq test-result 'M-2)
    :M-3         (setq test-result 'M-3)
    :M-4         (setq test-result 'M-4))
  (cmds-do '(M-1 M-2 M-3 M-4)
           global-map "C-0"))

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

(ert-deftest mykie-set-keys-short-hand ()
  "`mykie:set-keys' should call function even user doesn't specify
:default keyword."
  (mykie:set-keys global-map
    "C-e" (setq test-result 'default)
    "H-m" (setq test-result 'default)
    "S-a" (setq test-result 'default)
    "A-c" (setq test-result 'default)
    "M-s" (setq test-result 'default))
  (cmds-do '(default) global-map "C-e")
  (cmds-do '(default) global-map "H-m")
  (cmds-do '(default) global-map "S-a")
  (cmds-do '(default) global-map "A-c")
  (cmds-do '(default) global-map "M-s"))

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

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; no-byte-compile: t
;; End:

;;; mykie-test.el ends here
