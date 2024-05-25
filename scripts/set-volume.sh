#!/bin/bash

case $1 in
    up)
        pactl set-sink-mute "@DEFAULT_SINK@" false
        pactl set-sink-volume "@DEFAULT_SINK@" "+5%"
    ;;
    down)
        pactl set-sink-mute "@DEFAULT_SINK@" false
        pactl set-sink-volume "@DEFAULT_SINK@" "-5%"
    ;;
    toggle)
        pactl set-sink-mute "@DEFAULT_SINK@" toggle
    ;;
    *)
        echo "usage: $0 [up|down|toggle]" >&2
        exit 1
    ;;
esac
