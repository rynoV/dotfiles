#!/bin/sh

# This is for starting emacs from within neovim for using magit. It allows
# emacs to talk to the running neovim instance through the $NVIM socket set
# automatically within neovim. It starts an emacs frame connected to the
# running emacs daemon, so frame variables are used to avoid interference with
# other frames
emacsclient -a \'\' -c -nw -F "((calum-nvim . \"$NVIM\") (calum-magit-mode . t))" --eval "(magit-status)"
