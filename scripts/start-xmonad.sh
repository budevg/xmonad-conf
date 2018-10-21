#!/bin/bash

IS_NIXOS=0
SW=""
if [ -f /etc/NIXOS ]; then
    IS_NIXOS=1
    SW="/run/current-system/sw"
fi

xdg-user-dirs-update
dbus-launch
xfsettingsd
synapse -s &
# xscreensaver -no-splash &
# xfce4-panel &
trayer --edge top \
       --align right \
       --SetDockType true \
       --SetPartialStrut true \
       --expand true \
       --width 10 \
       --transparent true \
       --tint 0x191970 \
       --height 17 &
nm-applet &
xcompmgr -n &
exo-open $SW/etc/xdg/autostart/polkit-gnome-authentication-agent-1.desktop
if [ $IS_NIXOS -eq 0 ]; then
    update-notifier &
fi
map_keys.sh

exec ~/.xmonad/xmonad-x86_64-linux


# xfce-base.sh
# xdg-user-dirs-update
# dbus-launch
# xfsettingsd
# xfce4-panel &

# xfce-extras.sh
# system-config-printer-applet &
# xfce4-volumed
# xfce4-power-manager
# xscreensaver -no-splash &
# update-notifier &
# nm-applet &
# /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
