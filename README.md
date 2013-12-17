# mykie
Keybind configuration support tool for Emacs

## Description
This program can register multiple function to a keybind easily.

## Installation
You can install from MELPA by M-x package-install RET mykie.

## Configuration
Mykie.el don't serve specific command.
You need to register your keybinds.

```lisp
(require 'mykie)
;; this is example
(global-set-key (kbd "C-j")
  '(lambda ()
     (interactive)
     (mykie
      :default            '(progn
                             (delete-trailing-whitespace)
                             (case major-mode
                               (org-mode (org-return-indent))
                               (t        (newline-and-indent))))
      :C-u&eolp           '(fill-region (point-at-bol) (point-at-eol))
      :region             'query-replace-regexp)))
```

You can change condition's order and condition by `mykie:conditions` variable
from version 0.0.4. For example:

```lisp
(setq mykie:conditions
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
    (when current-prefix-arg :C-u)
    ;; -- this is NOT default condition --
    (case major-mode
      (org-mode        :org)
      (emacs-list-mode :emacs))
    ;; -----------------------------------
    (when (mykie:repeat-p)   :repeat)
    (when (minibufferp)      :minibuff)
    (mykie:thing)
    (when (bobp)             :bobp)
    (when (eobp)             :eobp)
    (when (bolp)             :bolp)
    (when (eolp)             :eolp)))
```

Above example is added a condition that compare for majar-mode.
You can call added function name like:

```lisp
(global-set-key (kbd "C-0")
                '(lambda ()
                   (interactive)
                   (mykie
                    :region  '(message "You are selecting region now")
                    :org     '(message "major-mode is org-mode")
                    :emacs   '(message "major-mode is emacs-lisp-mode")
                    :default '(message "default func"))))
```

Note: above element priority of `mykie:conditions` is high than below condition.
So you can't call :default function if you are selecting region and if
you set :region's function.

## Example
Below codes are samples for mykie.el

```lisp
(global-set-key (kbd "C-j")
  '(lambda ()
     (interactive)
     (mykie
      ;; You can specify lambda form.
      :default (lambda () (minibuffer-message "default"))
      ;; You can specify list form.
      :C-u     '(minibuffer-message "You pushed C-u")
      ;; You can specify symbol form.
      :region  'query-replace-regexp)))
```

```lisp
(defun mykie-sample ()
  "This is example function for C-j, but you can use this function"
  (interactive)
  (mykie
   :default    '(message "this is default function")
   :repeat     '(message "this is executed if pushed same point")
   :bolp       '(message "this is called if pushed at bolp")
   :eolp       '(message "this is called if pushed at eolp")
   :C-u&bolp   '(message "this is called if pushed at bolp after pushed C-u")
   :C-u&eolp   '(message "this is called if pushed at eolp after pushed C-u")
   :region     '(message "this is called if pushed it when selecting region")
   :region&C-u '(message "this is called if pushed it after pushed C-u when selecting region")))
(global-set-key (kbd "C-o") mykie-sample)
```

Also you can combine a case statement and mykie:C-u-num that can get pushed
times of C-u.
For example:

```lisp
(defun mykie-pushed-x-times ()
  (interactive)
  (mykie
   :default    '(message "default func")
   :C-u        '(case (mykie:C-u-num)
                  (1 (message "you pushed C-u one time, aren't you?"))
                  (2 (message "you pushed C-u two times, aren't you?"))
                  (t (message (format "you pushed C-u %d times, aren't you?"
                                      mykie:C-u-num))))
   :region     'query-replace-regexp))
(global-set-key (kbd "C-o") mykie-pushed-x-times)
```

Below mykie:loop function is trial now.
You can set keybinds and functions' pair.
Keybinds can specify [a-zA-Z] only.
(Maybe this function fail if you set existing keybind)

```lisp
(defun mykie:vi-faker ()
  (interactive)
  (let
      ((scroll (lambda (up-or-down)
                 (condition-case err
                     (case up-or-down
                       (up   (scroll-up-command))
                       (down (scroll-down-command)))
                   (error (minibuffer-message err))))))
    (mykie:loop
     ;; vi style
     "h" 'backward-char
     "j" 'next-line
     "k" 'previous-line
     "l" 'forward-char
     ;; less
     "f" '(funcall scroll 'up)
     "b" '(funcall scroll 'down))))
```
