function restart-pipewire
    set -l airpods_mac_address "74:65:0C:3E:1C:B7"
    bluetoothctl disconnect $airpods_mac_address
    sleep 1
    rfkill block bluetooth
    systemctl --user restart pipewire.service
    systemctl --user restart pipewire-media-session.service
    systemctl --user restart pipewire.socket
    systemctl --user restart pipewire-pulse.socket
    rfkill unblock bluetooth
    sleep 4
    bluetoothctl connect $airpods_mac_address
end

