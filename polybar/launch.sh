#!/usr/bin/env bash

CONFIG_FILE="${HOME}/dot/etc/polybar/config"

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
~/dot/src/polybar/build/bin/polybar -c $CONFIG_FILE example

echo "Bars launched..."
