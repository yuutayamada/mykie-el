[![Build Status](https://travis-ci.org/yuutayamada/mykie-el.png?branch=master)](https://travis-ci.org/yuutayamada/mykie-el)

# Mykie.el | Command multiplexer

Do you have enough keybinds in Emacs?
No? Then this program strongly helps you to add other functions to
**a single** keybind.

## Requirement

This package needs Emacs 24.3 or higher.

## Installation

You can install from MELPA by M-x package-install RET mykie.

## Configuration

Below configuration is common setting for mykie.el.
You can see more example in mykie-example file.
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
  :default     (beginning-of-line)
  :C-u         mark-whole-buffer
  "C-e"
  :default     (end-of-line)
  :C-u         (message "Hello")
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
   :default  (progn
                (delete-trailing-whitespace)
                (case major-mode
                  (org-mode (org-return-indent))
                  (t        (newline-and-indent))))
   :C-u&eolp (fill-region (point-at-bol) (point-at-eol))
   :region   query-replace-regexp)
```

## Available Keywords

You can specify below keywords to mykie's arguments such as :default
or :C-u etc..(You may saw those keywords on above examples)

Note: below keyword can specify function only. See Available Forms too.

| KEYWORD            | DESCRIPTION |
|:-------------------|:----------- |
| :default or t      | default function, call this if conditions aren't matched all conditions
| :C-u               | Call this if you pushed C-u key before pushing the key
| :C-u!              | like :C-u, but reset `current-prefix-arg`
| :C-u*N             | Call this if you pushed N times of C-u(replace N to a number)
| :C-u*N!            | like :C-u*N, but reset `current-prefix-arg`
| :M-N               | Call this if you pushed such as M-1(replace N to a number)
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
| :file              | Call this if current point matched `ffap-file-at-point`
| :C-u&file          | Call this if you satisfied :C-u & :file
| :MAJOR-MODE        | Call this if :MAJOR-MODE matched major-mode. for example you can specify like this: :emacs-lisp-mode (message "hello")
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
| :comment           | Call this if current point is comment

There are other convenience keywords
Below keywords can't specify function. Instead specify other thing.
See below description.

| KEYWORD             | VALUE          | DESCRIPTION                    |
|:--------------------|:---------------|:-------------------------------|
| :clone              | KEY as string  | Clone mykie's functions to other KEY. this function is convenient if you use Emacs either situation terminal and GUI. Because terminal Emacs can't use partial keybind such as C-;, this keyword can clone same functions to another key without :default function. For example: :clone ";" (<- if you want to clone origin key to ";") |
| :deactivate-region  | symbol         | deactivate selecting region after mykie executed command. You can specify this t, 'region, 'region&C-u. |
| :region-handle-flag | symbol         | Do copying or killing before command executing. This function is convenience if you want to use kill-ring's variable. But there is mykie:region-str variable that always store region's strings. |

## Basic Usage
There are four patterns to specify `mykie` keybinds.

`mykie:global-set-key`  

```lisp
(mykie:global-set-key "C-0"
  :default (message "hi"))
```

`mykie:define-key`  

```lisp
(mykie:define-key emacs-lisp-mode "C-0"
  :default (message "hi hello"))
```

`mykie:define-key-with-self-key`  

This function binds mykie keybinds to self-insert-key(like [a-zA-Z]).

```lisp
(mykie:define-key-with-self-key
    "a" :C-u (message "I am C-u"))
```

`mykie:set-keys`  

This function is convenience if you want to set multiple keybinds.  

For global-map  

```lisp
(mykie:set-keys nil
  "C-a"
  :default     (beginning-of-line)
  :C-u         mark-whole-buffer
  "C-e"
  :default     (end-of-line)
  :C-u         (message "Hello"))
```
For specific keymap  

```lisp
;; Set keybinds to specific keymap:
(mykie:set-keys emacs-lisp-mode-map
  "C-1"
  :default (message "C-1")
  :C-u     (message "C-1+C-u")
  "C-2"
  :default (message "C-2")
  :C-u     (message "C-2+C-u"))
```

For self-insert-command like "a", "b", "c" etc..  

```lisp
;; Set keybinds for self-insert-key
;; You don't need to specify :default state, it's specified to
;; self-insert-command automatically to it.
(mykie:set-keys with-self-key
  "a"
  :C-u (message "called a")
  :region query-replace-regexp
  "b"
  :C-u (message "called b"))
```

## Too many keyword...

If you can use [Helm](https://github.com/emacs-helm/helm), then
`helm-mykie-keywords` shows you available keywords.

## Fuzzy Ordering

This function name was lazy ordering until v0.1.1.
From v0.2.0, the name was renamed to fuzzy ordering and this function is
turn on by default.

You can change the order of commands by the order of each key-bindings
when you register key-bindings.
Below C-j binding precedes :C-u&url than :C-u&eolp.

```lisp
(mykie:set-keys nil
   "C-0"
   :default  (message "hi")
   :C-u&url  (browse-url-at-point) ; prioritize :C-u&url than :C-u&eolp
   :C-u&eolp (fill-region (point-at-bol) (point-at-eol))
   :C-u      (message "hello"))
```

## Shorthand for key binding and unbinding
mykie's :default keywords is redundant to specify just one function.
If you don't specify :default keyword and the function is only one,
you can use shorthand of `mykie:set-keys` function to register and
un-register multiple keybinds.

To register keybinds, you can write like this:
```lisp
(mykie:set-keys nil ; <- global-map
  "C-0" (message "foo") ; Eval as S-expression
  "C-1" emacs-version   ; Eval as command
  "C-2"
  :default (message "You can combine :default keyword as well.")
  :C-u     (message "You can combine other keyword as well.")
  )
```

To Un-register keybinds, you can write like this:
```lisp
(mykie:set-keys nil ; <- global-map
  "C-0" "C-1" ; <- those keys are unregistered
  ;; You can register keybinds in this function
  "C-3" :default (message "Hello")
  )
```

## Nested Mykie keybinds

You can implement your function with `mykie' function.

```lisp
(global-set-key (kbd "C-o") 
                '(lambda ()
                   (interactive)
                   (mykie :default (message "default")
                          :C-u (mykie :emacs-lisp-mode 
                                      (message "You can call filter condition")))))
```

## Prefix key with mykielized keybinds

You can make prefix keymap with mykielized keybinds. To be simple, I
omitted :default keywords in below example, but you can add other
keyword that I showed above. Note that this function can take some
parameters and you can handle the exit time by :keep and :exit keyword
with your anonymous function. Also you can specify kind of wrapping
function by using :before and :after with your specified function.
These guys are convenience if you want to change color of modeline,
cursor, or etc. when you do in and out of keymaps.

```lisp
(mykie:define-prefix-key global-map "M-j"
  (:keep
   (lambda ()
      ;; you can keep this prefix keymap as long as
      ;; this function return non-nil.
      ;; ... Implement something ...
      )
   :exit ; exit this prefix keymap when this function return non-nil
   (lambda ()
     (or (member last-command
                 '(mc/keyboard-quit
                   self-insert-command
                   mc/insert-numbers))
         (eq last-command-event (string-to-char "q"))))
   :before
   (lambda () ; this function called before you enter this prefix keymap
     (set-face-background 'modeline "red"))
   :after
   (lambda () ; this function called when you exit this prefix keymap
     (set-face-background 'modeline "blue")))
  ;; multiple-cursors ;;
  ;; You can use other keyword like :region or something, but
  ;; please don't forget :default keyword as well.
  "a"   mc/mark-all-like-this
  "q"   :default nil
  "n"   mc/skip-to-next-like-this
  "p"   mc/skip-to-previous-like-this
  "j"   mc/mark-next-like-this
  "k"   mc/mark-previous-like-this
  "m"   mc/mark-more-like-this-extended
  "u"   mc/unmark-next-like-this
  "U"   mc/unmark-previous-like-this
  "*"   mc/mark-all-like-this
  "M-j" mc/mark-all-like-this-dwim
  "i"   mc/insert-numbers
  "o"   mc/sort-regions
  "O"   mc/reverse-regions)
```

## Overriding of Major-Mode

I think almost Emacs lisp package do not use the major-mode keybind
with C-u prefix. (of course there are exception like Magit)
This section explains how to attach mykie's function of global-map to
specific keymap.

Here is an example:
```lisp
(setq mykie:use-major-mode-key-override 'both)
(mykie:initialize)
(mykie:set-keys nil ; <- nil means registering global-map
  "C-w" :default tetris :C-u (message "C-u+C-w"))
(mykie:set-keys with-self-key ; <- this means registering
  ;; ↓ You don't need to specify :default and self-insert-command
  ;; :default self-insert-command
  "1"  :region sort-lines
  "2"  :region align
  "3"  :region query-replace
  "c"  :C-u (message "C-u+c"))
```

You can specify 'both or 'global 'self or t to
mykie:use-major-mode-key-override.
'both means use overriding major-mode keys both case.
'global means use overriding major-mode keys by global-map's keys
without self-insert-command keys.
'self means use overriding major-mode keys by self-insert-command keys.
if you set nil, then don't overriding major-mode key.

Although, I mentioned before, there is a problem, several program are
using :C-u key to change the behavior of keys.
To avoid major-mode key overriding, you can specify specific modes
like this:

```lisp
(setq mykie:major-mode-ignore-list '(emacs-lisp-mode)
      mykie:minor-mode-ignore-list '(diff-minor-mode))
```

Also you can use below configuration to avoid overriding major-mode key.
This way ignores overriding major-mode keys.

```lisp
(setq mykie:use-major-mode-key-override 'both)
(mykie:initialize)
(mykie:set-keys nil ; <- nil means registering global-map
  "C-w"
  :default (message ":default will change to major-mode's function of same key")
  :C-u tetris
  :ignore-major-modes (magit-status-mode)
  :ignore-major-modes (diff-minor-mode))
```

You can specify specific mode.

```lisp
(setq mykie:use-major-mode-key-override nil)
(mykie:initialize)
(mykie:attach-mykie-func-to 'emacs-lisp-mode)
```

## Continuous Command

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
     "h" backward-char
     "j" next-line
     "k" previous-line
     "l" forward-char
     ;; less
     "f" (funcall scroll 'up)
     "b" (funcall scroll 'down))))
```

There is a similar command that do first command and then wait user input.

```lisp
(mykie:global-set-key
  "C-j"
  :default (mykie:do-while
              "j" newline-and-indent
              "u" undo))
```

Above command do newline-and-indent and then wait user input.

## Customizing own condition

You can change or attach `mykie`s conditions.

Here is an example to attach your customized conditions.

```lisp
(setq mykie:normal-conditions
      (append mykie:normal-conditions
              '(;; You can set keyword and predicate pair
                (:lisp . (case major-mode
                           ((lisp-mode emacs-lisp-mode) t)))
                ;; or regexp and predicate that return keyword pair.
                ;; below example is you can use :24.3.1 or :24.5.1 as mykie's args
                ("^:24\.\\(3\\|5\\)\..$" . (pcase emacs-version
                                             (`"24.3.1" :24.3.1)
                                             (`"24.5.1" :24.5.1))))))
```

## Parenthesized style

You can use parenthesized style.
But, I don't implement :default short hand feature in this style.
So, please keep in your mind, or use normal style.

```lisp
(mykie:global-set-key "C-j"
  (:default (delete-trailing-whitespace)
            (case major-mode
              (org-mode (org-return-indent)
              (t        (newline-and-indent)))))
  (:C-u&url browse-url-at-point)
  (:C-u&eolp (fill-region (point-at-bol) (point-at-eol))))
```

You can use mykie:define-key, mykie:global-set-key,
mykie:define-key-with-self-key, and mykie:set-keys to register
keybind with parenthesized style.

## Contributor(s)
Here's a [list](https://github.com/yuutayamada/mykie-el/contributors)
of all the people who have contributed to the development of mykie.

## Note

From v0.2.0, quote is not needed anymore when you register keybinds.
So if you ware already using old mykie.el, please delete needless
quotes before you use new mykie.el.
Also condition's form was changed from v0.2.0.
See Customizing section if you want to add specific condition.
And fuzzy order(lazy order) was turn on by default.
See fuzzy order section too.

## License
`mykie.el` is released under GPL v3.
