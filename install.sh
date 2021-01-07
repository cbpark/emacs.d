#! /bin/sh

EMACSD=$HOME/.emacs.d

function backup {
    if [ -e $HOME/$1 ]; then
        echo "-- $1 found."
        mv -v $HOME/$1 $HOME/"$1.old"
    fi
}

if [ "$(pgrep "^[eE]macs")" ]; then
    echo "-- Emacs is running. Shutdown it and try again."
    exit 1
else
    for oldfile in ".emacs" ".emacs.d"; do
        backup $oldfile
    done

    git clone git@github.com:cbpark/emacs.d.git ${EMACSD} \
        || git clone https://github.com/cbpark/emacs.d.git ${EMACSD} \
        || { echo "-- git clone failed."; exit 1; }

    # if command -v aspell >/dev/null 2>&1; then
    #     for aspelldict in ".aspell.en.prepl" ".aspell.en.pws"; do
    #         ln -sf ${EMACSD}/aspell/${aspelldict#*.} $HOME/$aspelldict
    #     done
    # fi

    mkdir -pv ${EMACSD}/{backup,autosave,etc}
    mkdir -pv $HOME/Documents/org
    pushd || exit 1
    cd $HOME/.emacs.d/lisp || exit 1
    emacs -Q --batch --eval '(batch-byte-recompile-directory 0)'
    popd || exit 1

    echo -e "-- Succeesfully installed.\n-- Happy Hacking!"
fi
