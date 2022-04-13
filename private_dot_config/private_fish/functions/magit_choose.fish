function magit_choose --description 'Start Magit after choosing repository'
    # Note that the & makes this run in the background
    emacs --magit -d :0 --eval "(magit-choose)" &
end
