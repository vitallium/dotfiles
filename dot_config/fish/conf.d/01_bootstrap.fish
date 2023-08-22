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

# Set ripgrep configuration file path
set -gx RIPGREP_CONFIG_PATH $XDG_CONFIG_HOME/ripgrep/config
