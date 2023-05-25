function highlight_output
    # Usage: highlight_output COLOR KEYWORDS COMMAND [ARGS...]
    # Keywords is a space separated string of words

    set -l color $argv[1]
    set -l keywords $argv[2]
    set -l command $argv[3]
    set -l args $argv[4..-1]

    # Escape color codes for Fish shell
    set -l escape_code (echo -e "\e[""$color""m")
    set -l reset_code (echo -e "\e[0m")

    # Convert keywords to a regular expression pattern
    set -l pattern (string join "|" (string split " " (string escape --no-quoted $keywords)))

    # Run the command and capture its output in real time
    eval $command $args | sed -u "s/\($pattern\)/$escape_code\1$reset_code/g"
end
