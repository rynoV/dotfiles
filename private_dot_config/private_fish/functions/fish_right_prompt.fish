function fish_right_prompt
    if test (prompt_hostname) != "calums-pc"
        set_color $fish_color_host_remote
        echo '['
        prompt_hostname
        echo ']'
    end
end
