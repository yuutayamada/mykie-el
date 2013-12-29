# Mykie.el | Command multiplexer

Do you have enough keybinds in Emacs?
No? Then this program strong help you to add other functions to
**a single** keybind.

## Installation

You can install from MELPA by M-x package-install RET mykie.

## Configuration

Below configuration is common setting for mykie.el.
You can see more example in example section.
Note: below `mykie:use-major-mode-key-override` is a bit complex,
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

## Available Keywords

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
| :err               | Call this if flymake-err-info or flycheck-current-errors is non-nil
| :C-u&err           | Call this if you satisfied :C-u & :err
| :region&err        | Call this if you satisfied :region & :err
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

## lazy ordering(from v0.1.1)

You can change ordering by each key-bindings when you register key-bindings.
To use this function, set t to mykie:use-lazy-order variable:

```lisp
(setq mykie:use-lazy-order t)
(mykie:set-keys nil
   "C-0"
   :default '(message "hi")
   :email   '(message "prior :email than :emacs-lisp-mode")
   :emacs-lisp-mode '(message "You can't see this if point is at email")
   :C-u*2   '(message "howdy")
   :C-u     '(message "hello")
   :C-u*3   '(message "hey") ; <- you can't see
   "C-1"
   :default '(message "hi")
   :C-u*3   '(message "howdy")
   :C-u     '(message "hello")
   :C-u*2   '(message "hey")) ; <- you can't see
```

Note: This function is available only related same conditions.
i.e., you can't change ordering between region & C-u and so on.
Note: You don't need care :default keyword's ordering.

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
  :default '(message "hi"))
```

- `mykie:define-key`

```lisp
(mykie:define-key emacs-lisp-mode "C-0"
  :default '(message "hi hello"))
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
I think almost Emacs lisp package do not use the major-mode keybind
with C-u prefix.(of course there are exception like Magit)
This section explain how to attach mykie's function of global-map to
specific keymap.

Here is an example:
```lisp
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

You can specify 'both or 'global 'self or t to
mykie:use-major-mode-key-override.
'both means use overriding major-mode keys both case.
'global means use overriding major-mode keys by global-map's keys
without self-insert-command keys.
'self means use overriding major-mode keys by self-insert-command keys.
if you set nil, then don't overriding major-mode key.

I was mentioned before, there is a problem, several program is using
:C-u key to change the key's behavior.
To avoid major-mode key overriding, you can specify specific modes like this:

```lisp
(setq mykie:major-mode-ignore-list '(emacs-lisp-mode)
      mykie:minor-mode-ignore-list '(diff-minor-mode))
```

You can use below configuration to avoid overriding major-mode key.
This way can ignore overriding major-mode key only specific keybind.
So you can't play tetris if current major-mode that you specified to
:ignore-major-modes has same keybind.

```lisp
(setq mykie:use-major-mode-key-override 'both)
(mykie:initialize)
(mykie:set-keys nil ; <- nil means registering global-map
  "C-w"
  :default '(message ":default will change to major-mode's function of same key")
  :C-u 'tetris
  :ignore-major-modes '(magit-status-mode)
  :ignore-major-modes '(diff-minor-mode))
```

You can specify specific mode by your hand.

```lisp
(setq mykie:use-major-mode-key-override nil)
(mykie:initialize)
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

### Continuous Command

Below mykie:loop function is trial now.
You can set keybinds and functions' pair.
Key-bindings can specify [a-zA-Z] only.
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

There is a similar command that do first command and then wait user input.

```lisp
(mykie:global-set-key
  "C-j"
  :default '(mykie:do-while
               "j" 'newline-and-indent
               "u" 'undo))
```

Above command do newline-and-indent and then wait user input.

## Customizing

You can change or attach `mykie`s conditions.

Here is an example to attach conditions.

```lisp
(setq mykie:before-user-normal-conditions
      '(;; appendixes to use lazy ordering
        (:point-one :point-two)
        ;; conditions
        (when (equal 1 (point)) :point-one)
        (when (equal 2 (point)) :point-two))
(mykie:initialize)
```

Then you can use :point-one and :point-two keyword.
Above example used `mykie:before-user-normal-conditions` variable.
But there are also below variables.

| VARIABLE                                | DESCRIPTION |
|:----------------------------------------|:------------|
| mykie:before-user-region-conditions     | attach this to mykie:region-conditions-base before it
| mykie:region-conditions-base            | default conditions for region function
| mykie:after-user-region-conditions      | attach this to mykie:region-conditions-base after it
| mykie:before-user-prefix-arg-conditions | attach this to mykie:prefix-arg-conditions-base before it
| mykie:prefix-arg-conditions-base        | default conditions for prefix-arg function
| mykie:after-user-prefix-arg-conditions  | attach this to mykie:prefix-arg-conditions-base after it
| mykie:before-user-normal-conditions     | attach this to mykie:normal-conditions-base after it
| mykie:normal-conditions-base            | default conditions for normal function
| mykie:after-user-normal-conditions      | attach this to mykie:normal-conditions-base after it

Note: if you are using lazy ordering, then those variable's ordering does not make sense.

## License
`mykie.el` is released under GPL v3.
