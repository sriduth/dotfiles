#!/bin/bash

# Read the current brightness
max_brightness=$(head /sys/class/backlight/intel_backlight/max_brightness)

# Calculate a 1% step
step=$(echo "(1.0 * $max_brightness)/100" | bc -q)

# Get the current brightness
current_brightness=$(head /sys/class/backlight/intel_backlight/actual_brightness)

if [ "$1" == "inc" ]; then
    new_brightness=$(echo "$current_brightness + $step" | bc -q)
elif [ "$1" == "dec" ]; then
    new_brightness=$(echo "$current_brightness - $step" | bc -q)
else
    new_brightness=$current_brightness
fi

# Write out the new brightness
echo $new_brightness > /sys/class/backlight/intel_backlight/brightness
echo $new_brightness


