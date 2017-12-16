# emacs.d

## How to install

```
curl -L https://raw.githubusercontent.com/cbpark/emacs.d/master/install.sh | $SHELL
```

The required emacs packages will be automatically installed when running Emacs after the installation.

[GNU Emacs](https://www.gnu.org/software/emacs/) with the version >= 25 must be installed. Check the version of Emacs by running `emacs --version` in a shell or `M-x emacs-version` in Emacs before installing this. [Aspell](http://aspell.net) and its dictionary are recommended to be installed, but not required.

## Recommended emacs packages

- [anzu](https://github.com/syohex/emacs-anzu)
- [auctex](https://www.gnu.org/software/auctex/)
- [expand-region](https://github.com/magnars/expand-region.el)
- [ggtags](https://github.com/leoliu/ggtags)
- [magit](https://magit.vc/)
- [undo-tree](http://www.dr-qubit.org/undo-tree.html)

## Systemd unit

It is obtained from [ArchWiki](https://wiki.archlinux.org/index.php/Emacs). Copy `emacs.service` to `~/.config/systemd/user` and then enable the unit as a normal user. It is assumed that the Emacs executable is `/usr/bin/emacs`.

```
mkdir -p ~/.config/systemd/user
cp ~/.emacs.d/emacs.service ~/.config/systemd/user
systemctl --user enable emacs
systemctl --user start emacs
```
