function magit --description 'Start Magit. Use -q/--quick to start using emacsclient'
    argparse --stop-nonopt --name=magit 'q/quick' -- $argv; or return
    # https://stackoverflow.com/a/29635131/14703577
    # Check if argument was provided and that it is a directory
    if begin; test (count $argv) -gt 0; and test -d $argv[1]; end
        set dir $argv
    else
        set dir $PWD
    end
    # Note that the & makes this run in the background
    if set --query _flag_quick
        myemacs --no-wait --eval "(magit-status \"$dir\")"
    else
        emacs --magit --eval "(magit-status)" --chdir $dir &
    end
end
