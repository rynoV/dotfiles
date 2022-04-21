function pacman_browse_group --description "Browse packages in a pacman group using fzf"
    pacman -Sgq $argv[1] | fzf --preview 'pacman -Si {}' --layout=reverse
end
