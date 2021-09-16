#!/usr/bin/env bash


/usr/bin/nm-applet &
/usr/bin/picom --daemon --config ~/.config/picom.conf
/usr/bin/setxkbmap -layout ch,us -option grp:alt_space_toggle
/usr/bin/playerctld daemon
/usr/bin/xmodmap ~/.config/xmodmap/xmodmap.conf
/usr/bin/conky
/usr/bin/nitrogen --restore
/bin/sh ~/bin/update-notify
/usr/bin/keybase-gui
