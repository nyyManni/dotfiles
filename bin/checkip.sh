#!/bin/bash

IP=$(curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//')
PREVIP=$(cat /var/ipaddr)

#echo $IP
#echo $PREVIP

if [ "$IP" == "$PREVIP" ]; then
    # IP has not changed
    exit
else
    # IP has changed!
    echo ${IP} > /var/ipaddr
    echo "ip changed!"
fi
