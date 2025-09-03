# https://stackoverflow.com/a/65187315/14703577
function rga-fzf --description "search for text in files recursively and display in fzf"
    argparse --stop-nonopt --name=rga-fzf 'rg-opts=' -- $argv; or return
    set --query _flag_rg_opts; or set -l _flag_rg_opts ''
    set -x RG_PREFIX rga --files-with-matches $_flag_rg_opts
    set -l file
    set file (
        FZF_DEFAULT_COMMAND="$RG_PREFIX '$argv'" \
            fzf $MY_FZF_FILE_OPTS --preview="test ! -z {}; and rga --pretty --context 5 {q} {} | bat --style=numbers --color=always --file-name {}" \
                --phony \
                --query "$argv" \
                --bind "change:reload:$RG_PREFIX {q}"
    );
    commandline --append $file
    commandline --function repaint
end
