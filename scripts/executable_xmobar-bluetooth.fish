#!/usr/bin/fish

# Script to be run by xmobar that shows whether bluetooth is on,
# whether any devices are connected, and the number of connected
# devices. This script must be in the path

if test (bluetoothctl show | grep "Powered: yes")
    # https://superuser.com/a/1613152
    set --local numdevices (bluetoothctl devices | cut -f2 -d ' ' | while read uuid; bluetoothctl info $uuid; end | grep -e "Connected: yes" | wc --lines)
    if test $numdevices -gt 0
        echo -n "<fc=#B48EAD> $numdevices</fc>"
    else
        echo -n "<fc=#8FBCBB> $numdevices</fc>"
    end
else
    echo "<fc=#D08770></fc>"
end
