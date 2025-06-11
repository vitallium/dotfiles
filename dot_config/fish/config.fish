#
# Bootstrap
#
set fish_greeting ''

#
# Homebrew
#
/opt/homebrew/bin/brew shellenv | source

# Paths
fish_add_path --append \
    # Local stuff
    $HOME/.local/bin \
    # https://github.com/GoogleChromeLabs/jsvu
    $HOME/.jsvu \
    # Go
    $HOME/.local/go/bin

#
# ripgrep
#
# Set configuration file path
set -gx RIPGREP_CONFIG_PATH $XDG_CONFIG_HOME/ripgrep/config

#
# mise
#
mise activate fish | source

#
# Golang
#
set -gx GOPATH $HOME/.local/go

#
# Rust
#
# https://doc.rust-lang.org/cargo/reference/environment-variables.html
set -gx CARGO_NET_GIT_FETCH_WITH_CLI true
if type -q cargo
    source "$HOME/.cargo/env.fish"
end

if type -q sccache
    set -gx RUSTC_WRAPPER sccache
end

#
# Eza
#
if type -q eza
    alias l="eza"
    alias ll="eza --long --header --group --created --modified -a"
    alias ls="eza"
end

#
# bat
#
if type -q bat
    set -x MANPAGER "bat --plain"
    set -x PAGER "bat --plain"

    alias cat="bat --paging=never --plain"
    alias less="bat --plain"
end

#
# EDITOR
#
if not test -z EDITOR
    set -x EDITOR hx
    alias x="hx"
end
