source ~/.config/fish/chromium.fish
status --is-interactive; and source (rbenv init -|psub)

# Disable fish greeting
set -gx fish_greeting ''

set -g fish_user_paths "$HOME/.cargo/bin" $fish_user_paths
# eval (python -m virtualfish compat_aliases auto_activation projects)

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

# Source the aliases in ~/.config/fish/aliases.fish.
test -f ~/.config/fish/aliases.fish; and . ~/.config/fish/aliases.fish

# Source the gruvbox color adjustments.
test -f ~/.config/fish/colors.fish; and source ~/.config/fish/colors.fish
