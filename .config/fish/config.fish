set -x LC_ALL en_US.UTF-8

# Disable fish greeting
set -gx fish_greeting              ''
set -gx FZF_DEFAULT_OPTS           '--height=50% --min-height=15 --reverse'
set -gx FZF_DEFAULT_COMMAND        'rg --files --no-ignore-vcs --hidden'
set -gx FZF_CTRL_T_COMMAND         $FZF_DEFAULT_COMMAND
set -gx EVENT_NOKQUEUE             1
set -gx EDITOR                     nvim
set -gx HOMEBREW_NO_ANALYTICS 1

test -e {$HOME}/.config/fish/aliases.fish; and source {$HOME}/.config/fish/aliases.fish
test -e {$HOME}/.config/fish/functions.fish; and source {$HOME}/.config/fish/functions.fish
test -e {$HOME}/.config/fish/paths.fish; and source {$HOME}/.config/fish/paths.fish
test -e {$HOME}/.config/fish/chromium.fish; and source {$HOME}/.config/fish/chromium.fish
test -e {$HOME}/.config/fish/gitlab.fish; and source {$HOME}/.config/fish/gitlab.fish

# pkg-config paths
set -gx PKG_CONFIG_PATH "/usr/local/opt/icu4c/lib/pkgconfig"

kitty + complete setup fish | source

eval (starship init fish)
