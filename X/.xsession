#!/usr/bin/env bash

# /etc/X11/xinit/xinitrc
#
# global xinitrc file, used by all X sessions started by xinit (startx)

# invoke global X session script
# . /etc/X11/Xsession

setxkbmap -option ctrl:nocaps  # Map control

ibus-daemon &
nm-applet &
dropbox start &

emacs &
x-terminal-emulator &

gnome-settings-daemon &
gnome-keyring-daemon &

exec awesome
