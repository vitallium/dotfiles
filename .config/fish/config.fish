set -x LC_ALL en_US.UTF-8

set -gx fish_greeting              ''
set -gx FZF_DEFAULT_OPTS           "
  --color=dark
  --color=fg:-1,bg:-1,hl:#5fff87,fg+:-1,bg+:-1,hl+:#ffaf5f
  --color=info:#af87ff,prompt:#5fff87,pointer:#ff87d7,marker:#ff87d7,spinner:#ff87d7
  --bind ctrl-a:select-all,ctrl-d:deselect-all,tab:toggle+up,shift-tab:toggle+down
"
set -gx FZF_DEFAULT_COMMAND        'rg --files --no-ignore --hidden --iglob "!.DS_Store" --iglob "!.git" --iglob "!node_modules"'
set -gx FZF_PREVIEW_COMMAND        'bat {}'
set -gx FZF_CTRL_T_COMMAND         $FZF_DEFAULT_COMMAND
set -gx EDITOR                     nvim
set -gx HOMEBREW_NO_ANALYTICS      1
set -gx BAT_THEME                  'Nord'
set -gx RIPGREP_CONFIG_PATH        {$HOME}'/.config/ripgrep/rc'

# TODO: Refactor to function
test -e {$HOME}/.config/fish/aliases.fish; and . {$HOME}/.config/fish/aliases.fish
test -e {$HOME}/.config/fish/functions.fish; and . {$HOME}/.config/fish/functions.fish
test -e {$HOME}/.config/fish/paths.fish; and . {$HOME}/.config/fish/paths.fish

test -e {$HOME}/.config/fish/gitlab.fish; and . {$HOME}/.config/fish/gitlab.fish
test -e {$HOME}/.config/fish/chromium.fish; and . {$HOME}/.config/fish/chromium.fish
test -e {$HOME}/.config/fish/crystal.fish; and . {$HOME}/.config/fish/crystal.fish

# fzf key bindings
test -e (brew --prefix fzf)/shell/key-bindings.fish; and . (brew --prefix fzf)/shell/key-bindings.fish

# pkg-config paths
set -gx PKG_CONFIG_PATH (brew --prefix icu4c)'/lib/pkgconfig:'(brew --prefix openssl)'/lib/pkgconfig'

source (brew --prefix asdf)/asdf.fish

kitty + complete setup fish | source

# Remove underlines from everywhere
# It looks ugly with Input font
set fish_pager_color_prefix 'white'  '--bold'
set fish_color_valid_path

# eval (starship init fish)
set SPACEFISH_PROMPT_ORDER time user dir host git exec_time line_sep battery jobs exit_code char
