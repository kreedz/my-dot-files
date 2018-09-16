#!/bin/bash
#setxkbmap us,ru -option grp:ctrl_shift_toggle
setxkbmap us,ru -option -variant dvp, 
xcape -e 'Control_L=Escape'
# setxkbmap -option "ctrl:swapcaps"
# xmodmap -e "keycode 69 = Tab"
xbindkeys
pgrep -u $USER -x nm-applet || (nm-applet &)
pgrep -u $USER -x xflux || (xflux -l 54.44N -g 55.58E &)
xset b off
