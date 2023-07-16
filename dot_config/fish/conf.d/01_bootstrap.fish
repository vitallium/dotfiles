set fish_greeting ''

fish_add_path --append \
    # Local stuff
    $HOME/.local/bin \
    # https://github.com/GoogleChromeLabs/jsvu
    $HOME/.jsvu \
    # Rust
    $HOME/.cargo/bin \
    # Go
    $HOME/.local/go/bin

set -gx HISTFILE $XDG_DATA_HOME/fish/fish_history

# Set ripgrep configuration file path
set -gx RIPGREP_CONFIG_PATH $XDG_CONFIG_HOME/ripgrep/config
