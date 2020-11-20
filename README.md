# goggles.el - Pulse modified region

![goggles](https://github.com/minad/goggles/blob/master/goggles.gif?raw=true)

Goggles highlights the modified region using `pulse`.
Currently the commands undo, yank, kill and delete are supported.

This library is the holy counterpart of `evil-goggles`.
Another comparable library is `volatile-highlights`, which does not use `pulse`.
By setting `goggle-pulse` to `nil`, the `goggles-mode` behaves similarily to the `volatile-highlights-mode`.

## Usage

~~~ elisp
(use-package goggles
  :demand t
  :config
  (goggles-mode)
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing
~~~
