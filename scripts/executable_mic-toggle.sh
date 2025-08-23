#!/bin/bash

pactl set-source-mute @DEFAULT_SOURCE@ toggle

MUTE_STATUS="$(pactl get-source-mute @DEFAULT_SOURCE@ | awk '{print $2}')"
if [ "${MUTE_STATUS}" == "no" ]; then
  notify-send --icon=mic-ready "Microphone is enabled"
elif [ "${MUTE_STATUS}" == "yes" ]; then
  notify-send --icon=mic-off "Microphone is disabled"
else
  notify-send --icon=dialog-error "Something is wrong with mic-toggle.sh"
fi
