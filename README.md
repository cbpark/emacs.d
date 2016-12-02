## How to install

```
curl -L https://raw.githubusercontent.com/cbpark/emacs.d/master/install.sh | $SHELL
```

The required emacs packages will be automatically installed when running Emacs after the installation.

GNU Emacs with the version >= 25 must be installed. Check the version of Emacs `emacs --version` in a shell or `M-x emacs-version` in Emacs before installing this.

### Systemd unit

Copy `emacs.service` to `~/.config/systemd/user` and then enable the unit.

```
systemctl --user enable emacs
systemctl --user start emacs
```
