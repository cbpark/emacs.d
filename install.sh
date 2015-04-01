#! /bin/sh

function backup {
    if [ -e $HOME/$1 ]; then
        echo "-- $1 found."
        mv -v $HOME/$1 $HOME/"$1.old"
    fi
}

if [ "$(pgrep "^[eE]macs")" ]; then
    echo "-- Emacs is running. Shutdown it and try again."
    exit
else
    backup ".emacs"
    backup ".emacs.d"
    git clone git@github.com:cbpark/emacs.d.git $HOME/.emacs.d
    pushd
    cd $HOME/.emacs.d/lisp 
    emacs -Q -batch -eval '(batch-byte-recompile-directory 0)'
fi
