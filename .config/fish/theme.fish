# special

set background '#1d2021'
set foreground '#d5c4a1'
set cursor '#d5c4a1'

# colors

set color0 '#1d2021'
set color1 '#fb4934'
set color2 '#b8bb26'
set color3 '#fabd2f'
set color4 '#83a598'
set color5 '#d3869b'
set color6 '#8ec07c'
set color7 '#d5c4a1'
set color8 '#665c54'
set color9 '#fb4934'
set color10 '#b8bb26'
set color11 '#fabd2f'
set color12 '#83a598'
set color13 '#d3869b'
set color14 '#8ec07c'
set color15 '#fbf1c7'

# fzf colors

set -x FZF_DEFAULT_OPTS "
    $FZF_DEFAULT_OPTS
    --color fg:7,bg:0,hl:1,fg+:232,bg+:1,hl+:255
    --color info:7,prompt:2,spinner:1,pointer:232,marker:1"

# fix LS_COLORS being unreadable

set -x LS_COLORS "$LS_COLORS:su=30;41:ow=30;42:st=30;44:"

# pure prompt

set -g pure_symbol_prompt "::"
set -g pure_symbol_git_unpulled_commits "↓"
set -g pure_symbol_git_unpushed_commits "↑"
set -g pure_reverse_prompt_symbol_in_vimode false
set -g pure_color_primary $color4
set -g pure_color_info $color6
set -g pure_color_mute $color8
set -g pure_color_success $color2
# set -g pure_color_normal
set -g pure_color_danger $color1
set -g pure_color_light $color7
set -g pure_color_warning $color3
set -g pure_color_dark $color0

# fish colors

set -g fish_color_command $color5 --bold
set -g fish_color_comment $color7
set -g fish_color_end $color5
set -g fish_color_error $color1
set -g fish_color_escape $color6
set -g fish_color_operator $color6
set -g fish_color_param $color4
set -g fish_color_quote $color3
set -g fish_color_redirection $color4
