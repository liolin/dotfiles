#!/usr/bin/env bash
set -euo pipefail


picom --config ~/.config/picom.conf &
nitrogen --restore &
setxkbmap -layout ch,us -option grp:alt_space_toggle &
nm-applet &
playerctld daemon
xmodmap ~/.config/xmodmap/xmodmap.conf
/bin/sh ~/bin/update-notify

bash ~/.config/polybar/launch.sh &
