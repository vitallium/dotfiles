#!/bin/sh

[ -f ~/.xprofile ] && . ~/.xprofile

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

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Gnome Keyring {{{
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
# }}}

# Keyboards {{{
setxkbmap -layout us,ru -option 'ctrl:nocaps' -option 'grp:alt_space_toggle'
# }}}

# Xorg settings {{{
xlayoutdisplay -q

# Stop screen from going blank
xset s off
xset -dpms
xset s noblank

# Hide Cursor when typing
xbanish &

# Faster key repeat and delay
xset r rate 210 40
# }}}

wal -R

exec i3
