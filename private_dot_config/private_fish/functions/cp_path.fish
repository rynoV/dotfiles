function cp_path --description 'Choose a file using FZF and copy the absolute path to the clipboard'
    set rel_path (fzf)
    echo $PWD/$rel_path | xclip -selection clipboard -rmlastnl
end

