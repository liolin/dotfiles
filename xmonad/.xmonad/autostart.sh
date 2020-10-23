#!/usr/bin/env bash
set -euo pipefail


picom --config ~/.config/picom.conf &
nitrogen --restore &
setxkbmap -layout ch,us -option grp:alt_space_toggle &
#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x282c34 --height 21 &
nm-applet &

bash ~/.config/polybar/launch.sh &
