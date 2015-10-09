#!/bin/bash

xset q | grep -e 'DPMS is Enabled' > /dev/null
if [ $? -eq 0 ]
then
    xset dpms 0 0 0
    xset -dpms
    xset s 0
#   ~/bin/screensaverclient.py -d
    exit
else
    xset +dpms
    xset dpms 5 5 5
    xset s 5
    #    ~/bin/screensaverclient.py -e
    xset dpms force off
    #xset dpms off
    exit
fi

