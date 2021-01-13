#!/usr/bin/env bash

# To use this file, create /usr/share/xsessions/awesome.desktop with
# the below lines:
# 
#  [Desktop Entry]
#  Name=awesomeDesktop
#  Comment=A Desktop environment with Awesome Window Manager
#  TryExec=/etc/X11/Xsession
#  Exec=/etc/X11/Xsession
#  Type=Application
#  Icon=/usr/share/pixmaps/awesome.xpm
#  Keywords=Window manager

setxkbmap -option ctrl:nocaps  # Map control

xscreensaver &
ibus-daemon &
nm-applet &
dropbox start &

x-terminal-emulator &
slack &

gnome-settings-daemon &
gnome-keyring-daemon &

exec awesome
