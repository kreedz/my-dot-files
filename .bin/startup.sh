#!/bin/bash
setxkbmap us,ru -option -variant dvp, 
setxkbmap -option ctrl:nocaps
setxkbmap -option grp:alt_shift_toggle
xcape -e 'Control_L=Escape'
xcape -e 'Shift_L=BackSpace'
xbindkeys
pgrep -u $USER -x nm-applet || (nm-applet &)
pgrep -u $USER -x xflux || (xflux -l 54.44N -g 55.58E &)
xset b off
