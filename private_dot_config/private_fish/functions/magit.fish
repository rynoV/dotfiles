function magit --description 'Start Magit'
    # https://stackoverflow.com/a/29635131/14703577
    # Check if argument was provided and that it is a directory
    if begin; test (count $argv) -gt 0; and test -d $argv[1]; end
        set dir $argv
    else
        set dir $PWD
    end
    # Note that the & makes this run in the background
    emacs --magit --eval "(magit-status)" -d :0 --chdir $dir &
end
