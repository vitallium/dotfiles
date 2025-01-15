#
# Bootstrap
#
set fish_greeting ''
set -g hydro_multiline true

# Paths
fish_add_path --append \
    # Local stuff
    $HOME/.local/bin \
    # https://github.com/GoogleChromeLabs/jsvu
    $HOME/.jsvu \
    # Rust
    $HOME/.cargo/bin \
    # Go
    $HOME/.local/go/bin

# Load Fisher plugins
for file in $fisher_path/conf.d/*.fish
    builtin source $file 2>/dev/null
end

#
# ripgrep
#
# Set configuration file path
set -gx RIPGREP_CONFIG_PATH $XDG_CONFIG_HOME/ripgrep/config

#
# Homebrew
if not set -q HOMEBREW_PREFIX
    if test -d /opt/homebrew
        set -Ux HOMEBREW_PREFIX /opt/homebrew
    else if test -d /usr/local/Homebrew
        set -Ux HOMEBREW_PREFIX /usr/local
    end
end

if set -q HOMEBREW_PREFIX
    # https://github.com/Homebrew/homebrew-cask/blob/master/USAGE.md
    set -gx HOMEBREW_BAT 1
    set -gx HOMEBREW_BUNDLE_NO_LOCK 1
    set -gx HOMEBREW_CACHE $XDG_CACHE_HOME/brew
    set -gx HOMEBREW_CASK_OPTS --no-quarantine
    set -gx HOMEBREW_LOGS $XDG_CACHE_HOME/brew/logs
    set -gx HOMEBREW_NO_ANALYTICS 1
    set -gx HOMEBREW_NO_COMPAT 1
    set -gx HOMEBREW_NO_ENV_HINTS 1

    # https://github.com/Homebrew/homebrew-command-not-found
    builtin source $HOMEBREW_PREFIX/Library/Taps/homebrew/homebrew-command-not-found/handler.fish 2>/dev/null

    fish_add_path --append -g --move $HOMEBREW_PREFIX/{,s}bin
    fish_add_path -g --move $HOMEBREW_PREFIX/opt/curl/bin
    fish_add_path -g --move $HOMEBREW_PREFIX/opt/icu4c/{,s}bin

    if not contains $HOMEBREW_PREFIX/share/info $INFOPATH
        set -gx INFOPATH $HOMEBREW_PREFIX/share/info $INFOPATH
    end
end

#
# mise
#
set -gx MISE_FISH_AUTO_ACTIVATE 0
if command -qa mise
    if status is-interactive
        mise activate fish | source
    else
        mise activate fish --shims | source
    end
end

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

if type -q sccache
    set -gx RUSTC_WRAPPER sccache
end

if status is-interactive
    if not test -z EDITOR
        if type -q nvim
            alias vi='nvim'
            alias vim='nvim'
            alias view='nvim -R'

            set -f editor nvim
        else if type -q vim
            set -f editor vim
        else
            set -f editor vi
        end

        set -gx EDITOR $editor
        set -gx VISUAL $editor
    end
end
