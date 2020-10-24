#!/usr/bin/env bash
set -euo pipefail


picom --config ~/.config/picom.conf &
nitrogen --restore &
setxkbmap -layout ch,us -option grp:alt_space_toggle &
nm-applet &

bash ~/.config/polybar/launch.sh &
