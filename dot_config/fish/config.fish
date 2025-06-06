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
set -gx GO111MODULE on

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

status is-interactive || exit

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
    alias cat="bat --paging=never"
end

#
# EDITOR
#
if not test -z EDITOR
    set -x EDITOR hx
end
