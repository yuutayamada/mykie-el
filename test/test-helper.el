(eval-when-compile (require 'cl))
(require 'ert)
(require 'mykie)

(defvar test-result nil)
(defvar test-region-state nil)
(defvar test-C-u-state nil)
(defvar test-err-state nil)
(defvar flycheck-current-errors nil)
(defvar test-mail "foo@mail.com")
(defvar test-url  "https://example.com")
(defvar test-C-u-keywords '())

(defun cmd-do (expect map key)
  (fset 'mykie:region-p (lambda () test-region-state))
  (with-temp-buffer
    (test-init expect)
    (when test-region-state (set-mark (point)))
    (call-interactively (lookup-key map (mykie:kbd key)))
    (case expect
      (readonly (read-only-mode 0)) ; <- This is introduced from emacs 24.3
      ((url C-u&url)     (should (equal test-url  mykie:current-thing)))
      ((email C-u&email) (should (equal test-mail mykie:current-thing)))
      ((file C-u&file)   (should (equal "./" mykie:current-file))))
    (should (eq expect test-result))))

(defun cmds-do (expects map key)
  (loop initially (setq current-prefix-arg nil)
        for expect in expects
        do (setq current-prefix-arg nil)
        do (cmd-do expect map key)))

(defun test-init (expect)
  (lexical-let
      ((kw-str (symbol-name expect)))
    (insert "   \n \n")
    (goto-char (1+ (point-min)))
    (setq test-result nil
          current-prefix-arg
          (cond
           ((string-match "C-u\\*\\([0-9]\\)" kw-str)
            (list (lsh 4 (string-to-number (match-string 1 kw-str)))))
           ((string-match "M-\\([0-9]\\)" kw-str)
            (string-to-number (match-string 1 kw-str)))
           (t (case expect
                ((C-u C-u&err C-u&prog region&C-u
                      C-u&bobp C-u&eobp C-u&bolp C-u&eolp
                      C-u&email C-u&url C-u&file) '(4)))))
          test-region-state (case expect
                              ((region region&err region&C-u region&prog) t))
          flycheck-current-errors (case expect
                                    ((region&err C-u&err err) t)))
    (case expect
      ((region&prog C-u&prog prog) (emacs-lisp-mode))
      ((url   C-u&url)   (insert test-url))
      ((email C-u&email) (insert test-mail))
      ((file  C-u&file)  (insert "./"))
      (readonly (read-only-mode t))
      (comment  (emacs-lisp-mode)
                (insert ";; Comment")
                (backward-char 4))
      ((C-u&bobp bobp) (goto-char (point-min)))
      ((C-u&eobp eobp) (call-interactively 'end-of-buffer))
      ((C-u&bolp bolp) (insert "\n ") (goto-char (point-at-bol)))
      ((C-u&eolp eolp) (goto-char (point-min))
       (goto-char (point-at-eol))))))
