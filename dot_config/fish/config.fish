#
# Bootstrap
#
set fish_greeting ''

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
    set -gx HOMEBREW_NO_COMPAT 1
    set -gx HOMEBREW_NO_ENV_HINTS 1

    # https://github.com/Homebrew/homebrew-command-not-found
    builtin source $HOMEBREW_PREFIX/Library/Taps/homebrew/homebrew-command-not-found/handler.fish 2>/dev/null

    fish_add_path --append -g --move $HOMEBREW_PREFIX/{,s}bin
    fish_add_path -g --move $HOMEBREW_PREFIX/opt/curl/bin
end

#
# direnv
#
if command -qa direnv
    direnv hook fish | source
end

#
# mise
#
if command -qa mise
    set -gx MISE_FISH_AUTO_ACTIVATE 0
    if status is-interactive
        mise activate fish | source
    else
        mise activate fish --shims | source
    end
end

status is-interactive || exit
#
# Editor
#
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

#
# Emacs
#
# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
set -gx LSP_USE_PLISTS true

if test -d "/Applications/Emacs.app/Contents/MacOS/bin"
    set -x PATH "/Applications/Emacs.app/Contents/MacOS/bin" $PATH
    alias emacs "emacs -nw" # Always launch "emacs" in terminal mode.
end

#
# Golang
#
set -gx GOPATH $HOME/.local/go
set -gx GO111MODULE on

#
# Gnupg
#
gpgconf --launch gpg-agent

if test -e (gpgconf --list-dirs agent-ssh-socket)
    set -x GPG_TTY (tty)
    set -g -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
else
    echo (gpgconf --list-dirs agent-ssh-socket) "doesn't exist. Is gpg-agent running?"
end

#
# nix
#
# fish_add_path --append $HOME/.nix-profile/bin \
#     /nix/var/nix/profiles/default/bin

if test -f "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish"
    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
end

# Allow unfree packages for nix
set -x NIXPKGS_ALLOW_UNFREE 1

#
# Rust
#
# https://doc.rust-lang.org/cargo/reference/environment-variables.html
set -gx CARGO_NET_GIT_FETCH_WITH_CLI true

# Rust
if type -q sccache
    set -gx RUSTC_WRAPPER sccache
end
