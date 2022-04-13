#!/bin/sh

# Switch to headset. `pactl list cards | grep bluez_card --after-context 60` can
# be used to find the profile, look for "device.name", and the "Profiles"
# section will have the profile name
pactl set-card-profile "bluez_card.74_65_0C_3E_1C_B7" headset-head-unit-msbc
