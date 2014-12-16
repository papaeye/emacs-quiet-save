quiet-save.el
=============

This is a fork of [kentaro/auto-save-buffers-enhanced](https://github.com/kentaro/auto-save-buffers-enhanced).

I forked it for four main reasons:

1. To shorten the name.
2. To learn Emacs Lisp from such a small code base.
3. To remove obsolete features like SVK support.
4. To remove unrelated features like saving *scratch*.

"quiet" means you don't have to type C-x C-s periodically and you
don't see "Wrote *filename*" in the echo area.

quiet-save borrows many codes from the following site/software:

- <http://homepage3.nifty.com/oatu/emacs/misc.html>
- <https://github.com/cask/shut-up>
- recentf.el

To use quiet-save, just add the following code into your .emacs:

```elisp
(require 'quiet-save)
(quiet-save-mode)
```

If you want quiet-save to work only with the files under the
directories checked out from Git or Mercurial, add the following
code into your .emacs:

```elisp
(setq quiet-save-vc-root-backends '(git hg))
(setq quiet-save-keep '(quiet-save-vc-root))
```
