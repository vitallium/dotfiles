;==========================================================
;
; THIS IS AN EXAMPLE CONFIGURATION FILE!
; IT IS NOT SUPPOSED TO WORK OUT OF THE BOX SINCE IS CONTAINS
; SETTINGS THAT ARE SPECIFIC TO THE MACHINE WHICH WAS USED
; TO GENERATE IT.
; Please refer to the web documentation hosted at:
; https://github.com/polybar/polybar#configuration
; and
; https://github.com/polybar/polybar/wiki/Configuration
; if you want to automatically generate one for you.
;
;==========================================================
;
;
;   ██████╗  ██████╗ ██╗  ██╗   ██╗██████╗  █████╗ ██████╗
;   ██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝██╔══██╗██╔══██╗██╔══██╗
;   ██████╔╝██║   ██║██║   ╚████╔╝ ██████╔╝███████║██████╔╝
;   ██╔═══╝ ██║   ██║██║    ╚██╔╝  ██╔══██╗██╔══██║██╔══██╗
;   ██║     ╚██████╔╝███████╗██║   ██████╔╝██║  ██║██║  ██║
;   ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝
;
;
;   To learn more about how to configure Polybar
;   go to https://github.com/polybar/polybar
;
;   The README contains a lot of information
;
;==========================================================

[colors]
background = ${xrdb:color0:#222}
background-alt = ${xrdb:color0:#222}
foreground = ${xrdb:color7:#222}
foreground-alt = ${xrdb:color7:#222}
primary = ${xrdb:color1:#222}
secondary = ${xrdb:color2:#222}
alert = ${xrdb:color3:#222}

[bar/mybar]
width = 100%
height = 40px
offset-x = 0%
offset-y = 0%
radius = 0
fixed-center = false
bottom = false
underline-size = 0

background = ${colors.background}
foreground = ${colors.foreground}

line-size =  0
line-color = #f00

border-size = 0px
border-color = #00000000

padding-left = 0
padding-right = 2

module-margin-left = 1
module-margin-right = 2

font-0 = mononoki Nerd Font:pixelsize=13;4
font-1 = mononoki Nerd Font:fontformat=truetype:size=12;3
font-2 = mononoki Nerd Font:pixelsize=12;1
font-3 = mononoki Nerd Font:fontformat=truetype:size=17;3
font-4 = mononoki Nerd Font:fontformat=truetype:size=14;3

modules-left = i3
modules-center = xwindow
modules-right = pulseaudio xkeyboard cpu temperature date powermenu
tray-position = right
tray-maxsize = 24
tray-padding = 2

;wm-restack = bspwm
wm-restack = i3

;override-redirect = true

;scroll-up = bspwm-desknext
;scroll-down = bspwm-deskprev

;scroll-up = i3wm-wsnext
;scroll-down = i3wm-wsprev

cursor-click = pointer
cursor-scroll = ns-resize

[module/xwindow]
type = internal/xwindow
label = %title%

[module/xkeyboard]
type = internal/xkeyboard
blacklist-0 = num lock

format-prefix = " "
format-prefix-foreground = ${colors.foreground-alt}
format-prefix-underline = ${colors.secondary}

label-layout = %layout%
label-layout-underline = ${colors.secondary}

label-indicator-padding = 2
label-indicator-margin = 1
label-indicator-background = ${colors.secondary}
label-indicator-underline = ${colors.secondary}

[module/filesystem]
type = internal/fs
interval = 25

mount-0 = /

label-mounted = %{F#0a81f5}%mountpoint%%{F-}: %percentage_used%%
label-unmounted = %mountpoint% not mounted
label-unmounted-foreground = ${colors.foreground-alt}

[module/i3]
type = internal/i3
format = <label-state> <label-mode>
index-sort = true
wrapping-scroll = false

; Only show workspaces on the same output as the bar
pin-workspaces = true
strip-wsnumbers = true

label-mode-padding = 2
label-mode-foreground = #000
label-mode-background = ${colors.primary}

; focused = Active workspace on focused monitor
label-focused = %name%
label-focused-background = ${colors.alert}
label-focused-underline= ${colors.background}
label-focused-foreground= ${colors.background}
label-focused-padding = 2

; unfocused = Inactive workspace on any monitor
label-unfocused = %name%
label-unfocused-padding = 2

; visible = Active workspace on unfocused monitor
label-visible = %name%
label-visible-background = ${self.label-focused-background}
label-visible-underline = ${self.label-focused-underline}
label-visible-padding = ${self.label-focused-padding}

; urgent = Workspace with urgency hint set
label-urgent = %name%
label-urgent-background = ${colors.alert}
label-urgent-underline = ${self.label-focused-underline}
label-urgent-padding = 2

; Separator in between workspaces
; label-separator = |

[module/cpu]
type = internal/cpu
interval = 2
format-prefix = ﬙
format-prefix-font = 4
format-prefix-foreground = ${colors.alert}
format-underline = #f90000
label = %percentage:2%%

[module/memory]
type = internal/memory
interval = 2
format-prefix = " "
format-prefix-foreground = ${colors.foreground-alt}
format-underline = #4bffdc
label = %percentage_used%%

[module/date]
type = internal/date
interval = 5

date =
date-alt = " %Y-%m-%d"

time = %H:%M
time-alt = %H:%M:%S

format-prefix = 
format-prefix-foreground = ${colors.alert}
format-prefix-font = 5
format-underline = #0a6cf5

label = %date% %time%

[module/pulseaudio]
type = internal/pulseaudio

format-volume = <label-volume> <bar-volume>
label-volume = VOL %percentage%%
label-volume-foreground = ${root.foreground}

label-muted = muted
label-muted-foreground = #666

bar-volume-width = 10
bar-volume-foreground = ${colors.alert}

bar-volume-gradient = false
bar-volume-indicator = ⏽
bar-volume-indicator-font = 2
bar-volume-fill = ⏽
bar-volume-fill-font = 2
bar-volume-fill-foreground = ${colors.alert}
bar-volume-empty = ⏽
bar-volume-empty-font = 2
bar-volume-empty-foreground = ${colors.foreground-alt}

[module/temperature]
type = internal/temperature
thermal-zone = 0
warn-temperature = 80
hwmon-path = /sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon1/temp1_input

format = <ramp><label>
format-underline = #f50a4d
format-warn = <ramp><label-warn>
format-warn-prefix = 
format-warn-prefix-font = 5
format-warn-prefix-foreground = ${colors.alert}

format-prefix = " "
format-prefix-font = 5
format-prefix-foreground = ${colors.alert}

label = %temperature-c%
label-warn = %temperature-c%
label-warn-foreground = ${colors.secondary}

ramp-0 = 
ramp-1 = 
ramp-2 = 
ramp-foreground = ${colors.foreground-alt}

[module/powermenu]
type = custom/menu

expand-right = true

format-spacing = 1

label-open = 
label-open-foreground = ${colors.alert}
label-close =  cancel
label-close-foreground = ${colors.alert}
label-separator = |
label-separator-foreground = ${colors.foreground-alt}

menu-0-0 = logout
menu-0-0-exec = menu-open-1
menu-0-1 = reboot
menu-0-1-exec = menu-open-1
menu-0-2 = power off
menu-0-2-exec = menu-open-2

menu-1-0 = cancel
menu-1-0-exec = menu-open-0
menu-1-1 = reboot
menu-1-1-exec = sudo reboot

menu-2-0 = power off
menu-2-0-exec = sudo poweroff
menu-2-1 = cancel
menu-2-1-exec = menu-open-0

[settings]
screenchange-reload = true
;compositing-background = xor
;compositing-background = screen
;compositing-foreground = source
;compositing-border = over
;pseudo-transparency = false

[global/wm]
margin-top = 5
margin-bottom = 5

; vim:ft=dosini
