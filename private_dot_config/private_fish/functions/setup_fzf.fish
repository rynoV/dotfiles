# Note this should be called in config.fish so that the fzf.fish plugin's conf.d/fzf.fish setup runs before this
function setup_fzf
    # Built-in fzf settings
    set -gx FZF_DEFAULT_OPTS "--cycle --layout=reverse --style=minimal --height=40% --preview-window=wrap --marker='*'
--bind='ctrl-y:execute-silent(echo {+} | string collect | fish_clipboard_copy),\
alt-p:toggle-preview'
"

    # Default command when using as `fzf` directly
    set -gx FZF_DEFAULT_COMMAND 'fd --type f --strip-cwd-prefix --hidden --follow --exclude .git'

    # For fish, ctrl-t uses the currently typed path (if any) as the starting point, and provides it as $dir
    set -gx FZF_CTRL_T_COMMAND "fd --type f --hidden --follow --exclude .git --search-path \$dir"

    # with-nth stuff skips showing the date/time
    set -gx FZF_CTRL_R_OPTS "
--bind 'ctrl-y:execute-silent(echo -n {3..} | string collect | fish_clipboard_copy)+abort'
--color footer:italic
--footer \"CTRL-Y -> copy command, CTRL-R -> chronological/relevance, ALT-R -> raw (use CTRL-N/CTRL-P), CTRL-/ and ALT-/ -> toggle wrap,\nSHIFT-DELETE -> delete command(s),ALT-ENTER -> reformat and insert command(s), ALT-T -> cycle prefix, ALT-P -> preview\"
--with-nth 3.. --bind 'alt-t:change-with-nth(1,3..|2..|3..)'"

    # Use ctrl-e in the fzf file finder to edit the file(s), ctrl-alt-e to edit in
    # vscode, copy absolute paths with ctrl-y, copy windows paths from wsl
    # with ctrl-alt-y
    set -gx FZF_CTRL_T_OPTS "--preview '~/scripts/fzf-preview.sh {}'
--color footer:italic
--footer \"CTRL-E -> editor, CTRL-ALT-E -> vscode, CTRL-Y -> copy absolute path, CTRL-ALT-Y -> copy Windows path, ALT-P -> preview\"
--bind '\
ctrl-e:execute($EDITOR {+} &> /dev/tty)+abort,\
ctrl-alt-e:execute(code {+})+abort,\
ctrl-y:execute-silent(echo {+} | xargs readlink -f | string collect | fish_clipboard_copy),\
ctrl-alt-y:execute-silent(for line in {+}; wslpath -w -a \$line; end | string collect | fish_clipboard_copy)'"

    # fzf.fish settings
    set fzf_diff_highlighter delta --paging=never --width=20

    # Fzf is configured using both PatrickF1/fzf.fish and the built-in fzf --fish.
    # For fzf.fish, we change the defaults which use ctrl-alt, because they don't
    # work consistently in the neovim terminal, particularly when running in
    # windows terminal or neovide. We also disable the history and directory
    # options and instead use the built-in ctrl-t and alt-c from fzf --fish
    fzf_configure_bindings --history= --directory= --processes=ctrl-p --git_log=ctrl-g --git_status=ctrl-s --variables=ctrl-v

    # This sets up shift-tab, ctrl-r, ctrl-t, and alt-c bindings, with some
    # useful configuration for each:
    # https://github.com/junegunn/fzf?tab=readme-ov-file#key-bindings-for-command-line.
    # shift-tab is very nice because it hooks in to fish's completion system,
    # so you can fuzzy search over anything fish can complete; great in
    # combination with fish's awesome built-in git completions.
    fzf --fish | source
end
