function fish_greeting
    if type -q colorscript
        colorscript --random
        echo
    else
        fish_logo
    end
end
