#!/usr/bin/env bash
set -euo pipefail


picom --config ~/.config/picom.conf &
nitrogen --restore &
setxkbmap -layout ch,us -option grp:alt_space_toggle &
nm-applet &
playerctld daemon
xmodmap ~/.config/xmodmap/xmodmap.conf
bash ~/.config/polybar/launch.sh &

/bin/sh ~/bin/update-notify
