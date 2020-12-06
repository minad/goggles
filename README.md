# goggles.el - Pulse modified region

[![](https://melpa.org/packages/goggles-badge.svg)](https://melpa.org/#/goggles)

Goggles highlights the modified region using `pulse`.
Currently the commands undo, yank, kill and delete are supported.

This library is the holy counterpart of `evil-goggles`.
Another comparable library is `volatile-highlights`, which does not use `pulse`.
By setting `goggle-pulse` to `nil`, the `goggles-mode` behaves similarily to the `volatile-highlights-mode`.

![goggles](https://github.com/minad/goggles/blob/master/goggles.gif?raw=true)

## Usage

~~~ elisp
(use-package goggles
  :demand t
  :config
  (goggles-mode)
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing
~~~

## Comparison to volatile-highlights

Goggles is mainly meant as a replacement for volatile-highlights, which
is very popular, but has a few downsides from my perspective.

* It does not support pulse. The request to use pulse for modification
  highlighting has come up a few times (e.g. recently on reddit).
* It is essentially unmaintained, no commit for 5 years.
* It contains obsolete code (cl instead of cl-lib, old advice mechanism,
  untested xemacs support)
* I disagree with the way its written, which is unnecessarily complicated. For
  example it contains an overly complicated extension mechanism. This extension
  mechanism somehow seems to require unnecessary bytecode compilation at
  startup.

Since improving volatile-highlights does not seem possible to me, except
rewriting it, I decided to create another package. This way breakage for
volatile-highlights users is avoided.
