source ~/.asdf/asdf.fish
source ~/.config/fish/aliases.fish
source ~/.config/fish/chromium.fish

# Disable fish greeting
set -gx fish_greeting ''

set -g fish_user_paths "$HOME/.cargo/bin" $fish_user_paths
