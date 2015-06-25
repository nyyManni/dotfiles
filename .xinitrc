#!/bin/bash

# enable num-lock on boot and finnish keymap
numlockx &
setxkbmap "fi"

# set custom keymapping
if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

# start pulseaudio for sound
/usr/bin/start-pulseaudio-x11

# start Unified Remote Server in the background
/opt/urserver/urserver --daemon

# set desktop background
eval "$(cat ~/.fehbg)"

# start comptositor
compton -b --vsync opengl

# start unclutter to remove mouse cursor when typing
unclutter -keystroke &

# disable screensaver
xset dpms 0 0 0
xset -dpms
xset s 0

# Set correct resolution to the monitor on the other X display
DISPLAY=$DISPLAY.1 xrandr --output VGA-0 --mode 1440x900

# launch the window manager
exec i3


