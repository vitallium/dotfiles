#!/bin/sh

# .Xresources and .Xmodmap {{{
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
  xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
  xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
  xmodmap "$usermodmap"
fi
# }}}

# Startup {{{

# X11 stuff {{{
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xlayoutdisplay --dpi 109 -q

# Stop screen from going blank
xset s off
xset -dpms
xset s noblank

# Hide Cursor when typing
xbanish &

# Faster key repeat and delay
xset r rate 210 40

# }}}

# Keyboard Layout {{{
setxkbmap -layout us,ru -option 'ctrl:nocaps' -option 'grp:alt_space_toggle'
# }}}

# Start i3
exec i3
# }}}

# vim:fdm=marker
