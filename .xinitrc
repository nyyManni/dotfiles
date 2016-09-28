#!/bin/bash

# enable num-lock on boot and finnish keymap
numlockx &
setxkbmap "fi"

# Set up custom keybindings
xbindkeys

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
nohup compton --vsync opengl &

# start unclutter to remove mouse cursor when typing
unclutter -keystroke &

# disable screensaver
xset dpms 0 0 0
xset -dpms
xset s 0
xset r rate 200 25

# set cursor
xsetroot -cursor_name left_ptr &


# start generating the i3 config with common settings
cat ~/.i3/config_common > ~/.i3/config

# use hostname to figure out on which machine this file exists
if [ "$(cat /etc/hostname)" = "overkill" ]; then
    # specific to desktop
    
    # Set correct resolution to the monitor on the other X display
#    DISPLAY="${DISPLAY}.1" xrandr --output VGA-0 --mode 1440x900

    # append the rest of i3's settings
    cat ~/.i3/config_gtx >> ~/.i3/config

    # Open HUD view from raspi
#    DISPLAY="${DISPLAY}.1" awesome &
#    DISPLAY="${DISPLAY}.1" firefox http://raspi:3000 -profile=hud &
else
    # specific to laptop

    # append the rest of i3's settings
    cat ~/.i3/config_asus >> ~/.i3/config
fi

# launch the window manager
exec i3


