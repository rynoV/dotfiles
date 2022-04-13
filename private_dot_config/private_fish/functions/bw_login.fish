function bw_login --description 'Login to Bitwarden CLI and set BW_SESSION'
    set -l __BW_SESSION (bw login --raw)
    # Make sure we don't overwrite the existing session when bw login fails
    # (e.g. when we are already logged in)
    if test $status -eq 0
        set --export --universal BW_SESSION $__BW_SESSION
    end
end
