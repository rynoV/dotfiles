# https://bbs.archlinux.org/viewtopic.php?pid=1271552#p1271552
function pacman --description "pacman but with sudo added automatically if necessary"
    switch $argv[1]
        case '-D' '-R*' '-U*' '-S' '-Sc*' '-Sd*' '-Su*' '-Sw*' '-Sy*'
            /usr/bin/sudo /usr/bin/pacman $argv
        case '*'
            /usr/bin/pacman $argv
    end
end
