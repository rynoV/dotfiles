#!/bin/sh

# For use with a neovim config that forwards command line arguments to a host
# instance when it is run within a host neovim terminal (flattening the neovim
# instances)
if [ -z "$NVIM" ]; then
  nvim "$@"
else
  nvim --headless "$@"
fi
