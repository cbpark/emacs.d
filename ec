#! /bin/sh

if [ -z $DISPLAY ]; then
    emacsclient -a "" -t "$@"
else
    emacsclient -a "" -nc "$@"
fi
