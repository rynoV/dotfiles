fish_vi_key_bindings

bind --mode insert \cf accept-autosuggestion
bind --mode insert \cj down-or-search
bind --mode insert \ck up-or-search

# Ctrl+Alt+g to grep
bind --mode insert \e\cg rga-fzf

bind --mode insert \co ranger-cd

# XDG folders: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
set --export XDG_CONFIG_HOME $HOME/.config
set --export XDG_CACHE_HOME $HOME/.cache
set --export XDG_DATA_HOME $HOME/.local/share
set --export XDG_STATE_HOME $HOME/.local/state

set PATH ~/.local/bin $PATH
set PATH ~/.local/bin/evil-software $PATH
set --export VISUAL emacsclient --no-wait --create-frame
set --export EDITOR emacsclient --no-wait --create-frame
set --export BROWSER browser
set MANPATH $MANPATH /usr/share/man

set --export FZF_DEFAULT_OPTS "--cycle --layout=reverse --border --height=90% --preview-window=wrap --marker='*' \
--bind='ctrl-alt-y:execute-silent(echo {+} | xclip -selection clipboard -rmlastnl)'
"

# Use ctrl-e in the fzf file finder to edit the file(s), ctrl-alt-e to edit in
# vscode, copy absolute paths with ctrl-alt-y
set --export MY_FZF_FILE_OPTS --bind "\
ctrl-e:execute(nvim-qt {+} &> /dev/tty)+abort,\
ctrl-alt-e:execute(code {+})+abort,\
ctrl-alt-y:execute-silent(echo {+} | xargs readlink -f | xclip -selection clipboard -rmlastnl)"

set fzf_dir_opts $MY_FZF_FILE_OPTS
set fzf_fd_opts --hidden --exclude=.git --follow

set --export JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
set PATH $JAVA_HOME/bin $PATH

set PATH $PATH /usr/local/go/bin
set --export GOPATH ~/.var/go

set MANPATH $MANPATH /usr/local/texlive/2022/texmf-dist/doc/man
set INFOPATH $INFOPATH /usr/local/texlive/2022/texmf-dist/doc/info
set PATH $PATH /usr/local/texlive/2022/bin/x86_64-linux

alias g magit

alias c code

alias x xdg-open

alias la "ls -a"

alias cm chezmoi

if type -q pacman
    alias pm pacman
    # Browse remote packages
    alias pml "pacman -Slq | fzf --preview 'pacman -Si {}' --layout=reverse"
    # Browse installed packages
    alias pmq "pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse"
    # Browse groups
    alias pmg "pacman -Sgq | fzf --preview 'pacman -Sgq {}' --layout=reverse"
    # Browse packages in a group passed as the argument
    alias pmgl "pacman_browse_group"
end

test "$TERM" = "xterm-kitty" && alias ssh "kitty +kitten ssh"

set --export GHCUP_USE_XDG_DIRS
set -gx PATH $XDG_DATA_HOME/cabal/bin $PATH $XDG_DATA_HOME/ghcup/bin # ghcup-env

set PATH $HOME/.nix-profile/bin /nix/var/nix/profiles/default/bin $PATH
set MANPATH $HOME/.nix-profile/share/man /nix/var/nix/profiles/default/share/man $MANPATH

if type -q any-nix-shell
    any-nix-shell fish --info-right | source
end

set --export GNUPGHOME $XDG_DATA_HOME/gnupg
# https://stackoverflow.com/a/55032706
set --export GPG_TTY (tty)

set --export CABAL_DIR $XDG_DATA_HOME/cabal

set --export NVM_DIR $XDG_DATA_HOME/nvm

set --export LESSHISTFILE $XDG_STATE_HOME/lesshst

# For "pass" password manager
set --export PASSWORD_STORE_DIR ~/.var/password-store

# Clean up duplicates in the path, from https://unix.stackexchange.com/a/14896
set PATH (printf "%s" "$PATH" | awk -v RS=':' '!a[$1]++ { if (NR > 1) printf RS; printf $1 }')

# Start X at login
if status is-login
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        exec startx -- -keeptty >~/.var/xorg.log 2>&1
    end
end
