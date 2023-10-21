# Based off https://github.com/junegunn/fzf/releases/tag/0.43.0
function fzf_preview_file --description "Made for use as $fzf_preview_file_cmd in _fzf_preview_file"
    set file_path $argv[1]
    if file --mime-type "$file_path" | grep -qF image/
        # --transfer-mode=memory is the fastest option but if you want fzf to be able
        # to redraw the image on terminal resize or on 'change-preview-window',
        # you need to use --transfer-mode=stream.
        kitty icat --clear --transfer-mode=memory --stdin=no --place={$FZF_PREVIEW_COLUMNS}x{$FZF_PREVIEW_LINES}@0x0 "$file_path" | sed \$d
    else
        bat --color=always "$file_path"
    end
end
