#!/bin/bash

MUTE=$(pactl get-sink-mute @DEFAULT_SINK@ | awk 'NR=1 {print $2}')
VOL=$(pactl get-sink-volume @DEFAULT_SINK@ | awk 'NR==1 {print $5}')
if [ "$MUTE" == "yes" ]; then
    echo "--"
else
    echo $VOL
fi
