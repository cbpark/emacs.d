## How to use

```
if [ -f ~/.emacs ]; then echo "-- .emacs found"; mv -v ~/.emacs ~/.emacs.old; fi
if [ -d ~/.emacs.d ]; then echo "-- .emacs.d found"; mv -v ~/.emacs.d ~/.emacs.d.old; fi
git clone git@github.com:cbpark/emacs.d.git ~/.emacs.d
cd ~/.emacs.d/lisp && emacs -Q -batch -eval '(batch-byte-recompile-directory 0)'
emacs -nw --debug-init
```
