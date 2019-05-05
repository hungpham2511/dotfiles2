#!/usr/bin/env bash
setxkbmap -option ctrl:nocaps  # Map control

ibus-daemon &
nm-applet &
dropbox start &

gnome-settings-daemon &
gnome-keyring-daemon &

exec awesome

