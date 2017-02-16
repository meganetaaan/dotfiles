#!/bin/bash -e

PRIMARY="eDP-1"
EXT1="HDMI-1"

if (xrandr | grep "$EXT1 connected"); then
    xrandr --output $PRIMARY --primary --auto --output $EXT1 --auto --right-of $PRIMARY
    echo "second screen (HDMI) enabled"
else
    xrandr --output $EXT1 --off
    xrandr --output $PRIMARY --primary --auto
    echo "second screen disabled"
fi
