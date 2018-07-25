source ~/.config/fish/aliases.fish
source ~/.config/fish/chromium.fish
status --is-interactive; and source (rbenv init -|psub)

# Disable fish greeting
set -gx fish_greeting ''

set -g fish_user_paths "$HOME/.cargo/bin" $fish_user_paths
# eval (python -m virtualfish compat_aliases auto_activation projects)
