# mykie
Command multiplexer: Register multiple functions to a keybind

## Description
This program can register multiple function to a keybind easily.

## Installation
You can install from MELPA by M-x package-install RET mykie.

## Configuration
Mykie.el don't serve specific command.
You need to register your keybinds.
For example:

```lisp
  (require 'mykie)
  ;; You can set 'global or global-map instead of nil to specify global-map.
  ;; If you want to specify specific keymap then specify the keymap name
  ;; like emacs-lisp-mode-map instead of nil.
  (mykie:set-keys nil
    "C-a"
    :default     '(beginning-of-line)
    :C-u         'mark-whole-buffer
    "C-e"
    :default     '(end-of-line)
    :C-u         '(message "Hello")
    ;; ... You can add more keybinds
    )
```

Above example is registered two keybinds in global-map.
The C-a binding default behavior is beginning-of-line.
But do mark-whole-buffer if you pushed C-u key before pushing C-a.
As you can see, you can add more keybinds like above C-e.

You can specify like this too.

```lisp
(mykie:global-set-key "C-j"
   :default  '(progn
                (delete-trailing-whitespace)
                (case major-mode
                  (org-mode (org-return-indent))
                  (t        (newline-and-indent))))
   :C-u&eolp '(fill-region (point-at-bol) (point-at-eol))
   :region   'query-replace-regexp)
```

### Change condition(from v0.0.4)
When you pushed `mykie`s keybind, mykie.el is trying to compare each
`mykie:conditions`s condition and then if the condition is returning
value like :C-u then mykie search same keyword from current mykie
function.
If same keyword argument exists, then do related function.
Otherwise, compare next condition.
If comparing is failed then do :default function.

You can change condition's order and condition by `mykie:conditions` variable.
For example:

```lisp
(setq mykie:conditions
  '(;; REGION
    (when (region-active-p)
      (or (and current-prefix-arg
               :region&C-u)
          :region))
    ;; PREFIX-ARGUMENT
    (when current-prefix-arg
      (or (and (eobp)        :C-u&eobp)
          (and (bobp)        :C-u&bobp)))
    (when current-prefix-arg
      (or (and (bolp)        :C-u&bolp)
          (and (eolp)        :C-u&eolp)))
    (mykie:get-prefix-arg-state)
    (when current-prefix-arg :C-u) ; Use :C-u if C-u*X isn't exists
    ;; -- this is NOT default condition --
    (when t :your-favorite-keyword)
    ;; -----------------------------------
    ;; NORMAL
    (when (mykie:repeat-p)   :repeat)
    (when (minibufferp)      :minibuff)
    (when (bobp)             :bobp)
    (when (eobp)             :eobp)
    (when (bolp)             :bolp)
    (when (eolp)             :eolp)))
```

and this is test command.

```lisp
(mykie:global-set-key "C-0"
  :region  '(message "You are selecting region now")
  :your-favorite-keyword '(message "howdy")
  :default '(message "default func"))))
```

Above example is added a condition that return :your-favorite-keyword.
Maybe You can see "howdy" message by pushing C-0.

Note: above element priority of `mykie:conditions` is high than below condition.
So you can't call :default function if you are selecting region and if
you set :region's function.

### Add your condition
There is another way to add your favorite condition.(from v0.0.7)
For example.
```lisp
(setq mykie:before-user-normal-conditions
      '((when t :this-is-test))
(mykie:initialize)
```
Then you can use :this-is-test keyword argument.
This way is convenience if you want to add condition without changing
default conditions.

You can use below variables to add your conditions

    mykie:before-user-region-conditions
    mykie:after-user-region-conditions
    mykie:before-user-prefix-arg-conditions
    mykie:after-user-prefix-arg-conditions
    mykie:before-user-normal-conditions
    mykie:after-user-normal-conditions

## Examples
Below codes are samples for mykie.el

### Available Forms

You can specify below forms.

```lisp
(mykie:global-set-key "C-0"
   ;; You can specify lambda form.
   :default (lambda () (minibuffer-message "default"))
   ;; You can specify list form.
   :C-u     '(minibuffer-message "You pushed C-u")
   ;; You can specify symbol form.
   :region  'query-replace-regexp)
```

### Key Definition

There are four patterns to specify `mykie` keybinds.

1. `mykie:global-set-key`
```lisp
(mykie:global-set-key "C-0"
  :default (message "hi"))
```

2. `mykie:define-key`  
```lisp
(mykie:define-key emacs-lisp-mode "C-0"
  :default (message "hi hello"))
```

3. `mykie:define-key-with-self-key`  
This function define key to self-insert-key(like [a-zA-Z]) with
`mykie` functions.  
This function add :default 'self-insert-command automatically to
specified key.
```lisp
(mykie:define-key-with-self-key
    "a" :C-u '(message "I am C-u"))
;; try C-u and then a
```

4. `mykie:set-keys`  
This function is convenience if you want to set multiple keybinds.  
For global-map
```lisp
  (mykie:set-keys 'global
    "C-a"
    :default     '(beginning-of-line)
    :C-u         'mark-whole-buffer
    "C-e"
    :default     '(end-of-line)
    :C-u         '(message "Hello"))
```
For specific keymap
```lisp
  Set keybinds to specific keymap:
  (mykie:set-keys emacs-lisp-mode-map
    "C-1"
    :default '(message "C-1")
    :C-u     '(message "C-1+C-u")
    "C-2"
    :default '(message "C-2")
    :C-u     '(message "C-2+C-u"))
```
For self-insert-key
```lisp
  Set keybinds for self-insert-key
  You don't need to specify :default state, it's specified to
  'self-insert-command automatically to it.
  (mykie:set-keys 'with-self-key
   "a"
   :C-u '(message "called a")
   :region 'query-replace-regexp
   "b"
   :C-u '(message "called b"))
```

### Available Keyword(State)
Below example is common keywords.
You can make sure available full keywords at `mykie:conditions` variable.
(mykie.el is using *state* as terminology of condition's keyword)

```lisp
(mykie:global-set-key "C-0"
   :default    '(message "this is default function")
   :repeat     '(message "this is executed if pushed same point")
   :bolp       '(message "this is called if pushed at bolp")
   :eolp       '(message "this is called if pushed at eolp")
   :C-u&bolp   '(message "this is called if pushed at bolp after pushed C-u")
   :C-u&eolp   '(message "this is called if pushed at eolp after pushed C-u")
   :region     '(message "this is called if pushed it when selecting region")
   :region&C-u '(message "this is called if pushed it after pushed C-u when selecting region"))
```

Also you can utilize C-u's pushed times.
For example:

```lisp
(mykie:global-set-key "C-0"
   :default '(message "default func")
   :C-u     '(message "C-u")
   :C-u*2   '(message "You pushed C-u 2 times aren't you?")
   :C-u*3   '(message "You pushed C-u 3 times aren't you?")
   :C-u*4   '(message "You pushed C-u 4 times aren't you?")
   :region  'query-replace-regexp)
```

Also you can utilize M-[0-9] pushing times.

```lisp
(mykie:global-set-key "C-0"
   :default '(message "default func")
   :C-u     '(message "C-u")
   :M-1     '(message "You pushed M-1 aren't you?")
   :M-2     '(message "You pushed M-2 aren't you?")
   :M-3     '(message "You pushed M-3 aren't you?")
   :M-12    '(message "You might pushed M-1 and M-2 aren't you?")
   :region  'query-replace-regexp)
```

As you may know, you can do commands
[M-1 M-2 C-0], [M-1 2 C-0], or [C-u 1 2 C-0] to call :M-12's function.

Also you can utilize :email and :url keyword.
And you can use `mykie:current-thing' variable that store thing's variable.
```lisp
(mykie:global-set-key "C-0"
   :C-u&url            '(browse-url-at-point)
   :email              '(message mykie:current-thing)
   :default            '(message "default"))
;; example ↓ try C-0 on below url or email address
;; http://www.google.com
;; example@email
```

### mykie:loop function

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
