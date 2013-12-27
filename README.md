# Mykie.el | Command multiplexer

Do you have enough keybinds in Emacs?
No? Then this program strong help you to add other functions to
**a single** keybind.

## Installation

You can install from MELPA by M-x package-install RET mykie.

## Configuration

Below configuration is common setting for mykie.el.
You can see more example in example section.
Note: below `mykie:use-major-mode-key-override` is bit complex,
See below section 'Major-mode's keys overriding' for details.

```lisp
(require 'mykie)
(setq mykie:use-major-mode-key-override t)
(mykie:initialize)
;; You can set 'global or global-map instead of nil to specify global-map.
;; If you want to specify specific keymap, specify the keymap name
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
The C-a binding default behavior is `beginning-of-line`.
But do `mark-whole-buffer` if you pushed C-u key before pushing C-a.
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

### Available Keywords

You can specify below keywords to mykie's arguments such as :default
or :C-u etc..(You may saw those keywords on above examples)

Note: below keyword can specify function only. See Available Forms too.

| KEYWORD            | DESCRIPTION |
|:-------------------|:----------- |
| :default           | default function, call this if conditions aren't matched all conditions
| :C-u               | Call this if you pushed C-u key before pushing the key
| :C-u*N             | Call this if you pushed N times of C-u(replace N to number)
| :M-N               | Call this if you pushed such as M-1(replace N to number)
| :region            | Call this if you are selecting region
| :region&C-u        | Call this if you satisfied :region & :C-u condition
| :repeat            | Call this if you repeat same command at same point
| :bolp              | Call this if current point is beginning of line
| :eolp              | Call this if current point is end of line
| :bobp              | Call this if current point is beginning of buffer
| :eobp              | Call this if current point is end of buffer
| :C-u&bolp          | Call this if you satisfied :C-u & :bolp
| :C-u&eolp          | Call this if you satisfied :C-u & :eolp
| :C-u&bobp          | Call this if you satisfied :C-u & :bobp
| :C-u&eobp          | Call this if you satisfied :C-u & :eobp
| :email             | Call this if current point matched (thing-at-point 'email)
| :C-u&email         | Call this if you satisfied :C-u & :email
| :url               | Call this if current point matched (thing-at-point 'url)
| :C-u&url           | Call this if you satisfied :C-u & :url
| :MAJOR-MODE        | Call this if :MAJOR-MODE matched major-mode. for example you can specify like this: :emacs-lisp-mode '(message "hello")
| :C-u&MAJOR-MODE    | Call this if you satisfied :C-u & :MAJOR-MODE
| :region&MAJOR-MODE | Call this if you satisfied :region & :MAJOR-MODE
| :prog              | Call this if current buffer is related programming see also `prog-mode' from Emacs. Note this function can use from Emacs 24.1.
| :C-u&prog          | Call this if you satisfied :C-u & :prog
| :region&prog       | Call this if you satisfied :region & :prog
| :minibuff          | Call this if current point is in minibuffer
| :readonly          | Call this if current buffer is read-only
| :comment           | Call this if current point is string or comment face

There are other convenience keywords
Below keywords can't specify function. Instead specify other thing.
See below description.

| KEYWORD             | VALUE          | DESCRIPTION                    |
|:--------------------|:---------------|:-------------------------------|
| :clone              | KEY as string  | Clone mykie's functions to other KEY. this function is convenient if you use Emacs either situation terminal and GUI. Because terminal Emacs can't use partial keybind such as C-;, this keyword can clone same functions to another key without :default function. For example: :clone ";" (<- if you want to clone origin key to ";") |
| :deactivate-region  | symbol         | deactivate selecting region after mykie executed command. You can specify this t, 'region, 'region&C-u. |
| :region-handle-flag | symbol         | Do copying or killing before command executing. This function is convenience if you want to use kill-ring's variable. But there is mykie:region-str variable that always store region's strings. |

### Change condition(from v0.0.4)

When you pushed `mykie`s keybind, mykie.el is trying to compare each
`mykie:conditions`s condition and then if the condition is returning
value like :C-u then mykie search same keyword from current mykie
function.
If same keyword argument exists, then do related function.
Otherwise, compare next condition.
If comparing is failed then do :default function.

You can change condition's order and condition by `mykie:conditions`
variable.
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

Note: above element priority of `mykie:conditions` is high than below
condition. So you can't call :default function if you are selecting
region and if you set :region's function.

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

| variable name                           | description |
|:----------------------------------------|:------------|
| mykie:before-user-region-conditions     | check this before check conditions related region
| mykie:after-user-region-conditions      | check this after check conditions related region
| mykie:before-user-prefix-arg-conditions | check this before check conditions related prefix-argument
| mykie:after-user-prefix-arg-conditions  | check this after check conditions related prefix-argument
| mykie:before-user-normal-conditions     | check this before check normal conditions
| mykie:after-user-normal-conditions      | check this after check normal conditions

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

- `mykie:global-set-key`

```lisp
(mykie:global-set-key "C-0"
  :default (message "hi"))
```

- `mykie:define-key`

```lisp
(mykie:define-key emacs-lisp-mode "C-0"
  :default (message "hi hello"))
```

-  `mykie:define-key-with-self-key`

This function define key to self-insert-key(like [a-zA-Z]) with
`mykie` functions.
This function add :default 'self-insert-command automatically to
specified key.

```lisp
(mykie:define-key-with-self-key
    "a" :C-u '(message "I am C-u"))
```

- `mykie:set-keys`
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
;; Set keybinds to specific keymap:
(mykie:set-keys emacs-lisp-mode-map
  "C-1"
  :default '(message "C-1")
  :C-u     '(message "C-1+C-u")
  "C-2"
  :default '(message "C-2")
  :C-u     '(message "C-2+C-u"))
```

For self-insert-command like "a", "b", "c" etc..

```lisp
;; Set keybinds for self-insert-key
;; You don't need to specify :default state, it's specified to
;; 'self-insert-command automatically to it.
(mykie:set-keys 'with-self-key
  "a"
  :C-u '(message "called a")
  :region 'query-replace-regexp
  "b"
  :C-u '(message "called b"))
```

### Major-mode's keys overriding

Here is an example:
```lisp
;; You can specify 'both or 'global 'self or t to
;; mykie:use-major-mode-key-override.
;; 'both means use overriding major-mode keys both case.
;; 'global means use overriding major-mode keys by global-map's keys
;; without self-insert-command keys.
;; 'self means use overriding major-mode keys by self-insert-command keys.
;; if you set nil, then don't overriding major-mode key.
(setq mykie:use-major-mode-key-override 'both)
(mykie:initialize)
(mykie:set-keys nil ; <- nil means registering global-map
  "C-w" :default 'tetris :C-u '(message "C-u+C-w"))
(mykie:set-keys 'with-self-key ; <- this means registering
  ;; ↓ You don't need to specify :default and self-insert-command
  ;; :default 'self-insert-command
  "1"  :region 'sort-lines
  "2"  :region 'align
  "3"  :region 'query-replace
  "c"  :C-u '(message "C-u+c"))
```


There are some problem, several program is using :C-u key to change the
key's behavior or can't override automatically(although mykie.el is using
change-major-mode-after-body-hook to override major-mode keys.)

To avoid major-mode key overriding, you can do like this:

```lisp
(setq mykie:major-mode-ignore-list '(emacs-lisp-mode)
      mykie:minor-mode-ignore-list '(diff-minor-mode))
```

You can use below configuration. This way can specify specific keybind
only. Then you can play tetris.

```lisp
(mykie:set-keys nil ; <- nil means registering global-map
  "C-t"
  :default 'tetris :C-u '(message "C-u+C-t")
  :ignore-major-modes '(emacs-lisp-mode)
  :ignore-major-modes '(diff-minor-mode))
```

Second case you can override major-mode's keys like this:
```lisp
;; specify major-mode name as symbol.
(mykie:attach-mykie-func-to 'emacs-lisp-mode)
```

### Available Keyword(State)

Below example is common keywords.
You can make sure available full keywords at `mykie:conditions` variable.
(mykie.el is using **state** as terminology of condition's keyword)

-   Normal Keywords Example

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

-   C-u*N example

    You can utilize C-u's pushed times.
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

-   M-N example

You can utilize M-[0-9] pushing times.

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

-   :email, :url example

You can utilize :email and :url keyword.
And you can use `mykie:current-thing` variable that store thing's
variable.

```lisp
(mykie:global-set-key "C-0"
   :C-u&url            '(browse-url-firefox mykie:current-thing)
   :email              '(message mykie:current-thing)
   :default            '(message "default"))
;; example ↓ try C-0 on below url or email address
;; http://www.google.com
;; example@email
```

-   :clone example

Below an example can use comment-dwim function even terminal Emacs by C-;
key. (Because terminal Emacs ignore "C-" key.)

```lisp
(mykie:set-keys nil
  "C-;"
  :default 'doctor
  :region  'comment-dwim
  :clone   ";")
```

-   :prog example

This is an example using [quickrun.el](https://github.com/syohex/emacs-quickrun).

```lisp
(mykie:set-keys nil
  "M-q"
  :default     'tetris
  :prog        'quickrun
  :C-u&prog    'quickrun-with-arg
  :region&prog 'quickrun-region)
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

## License
`mykie.el` is released under GPL v3.
