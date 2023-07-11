status is-interactive || exit

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

# rtx (https://github.com/jdxcode/rtx)
if type -q rtx
    command rtx activate fish | source
end

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
    set -gx HOMEBREW_BUNDLE_FILE $XDG_CONFIG_HOME/Brewfile
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
end

# Golang
set -gx GOPATH $HOME/.local/go
set -gx GO111MODULE on

# Rust
if type -q sccache
    set -gx RUSTC_WRAPPER sccache
end

# Setup editor
if not test -z EDITOR
    if type -q nvim
        abbr --add vi nvim
        abbr --add vim nvim
        abbr --add view 'nvim -R'

        set -f editor nvim

    else if type -q vim
        set -f editor vim
    else
        set -f editor vi
    end

    set -gx EDITOR $editor
    set -gx VISUAL $editor
end

# Configure terminfo for wezterm
if test $TERM = wezterm; and not infocmp wezterm &>/dev/null
    set -l TERMINFO_DIR $HOME/.terminfo

    if fish_is_root_user
        set TERMINFO_DIR /usr/share/terminfo
    end

    # Comes in via Chezmoi externals.
    if test -f $XDG_CACHE_HOME/wezterm/terminfo
        /usr/bin/tic -x -o $TERMINFO_DIR $XDG_CACHE_HOME/wezterm/terminfo
    end
end

# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
set -gx LSP_USE_PLISTS true

# Enable integration with direnv
if command -qa direnv
    direnv hook fish | source
end

# GnuPG
gpgconf --launch gpg-agent
set -x GPG_TTY (tty)
set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)

# fzf
set -gx FZF_DEFAULT_OPTS --cycle --filepath-word --height=50% --info=hidden --border=sharp

if functions -q fzf_configure_bindings
    set fzf_preview_file_cmd bat --line-range :100 --color=always --plain
    set fzf_preview_dir_cmd lsd --tree --depth=1

    set fzf_fd_opts --hidden
    set fzf_dir_opts --bind 'ctrl-e:execute(command $EDITOR {} >/dev/tty)'

    # Bind Ctrl-t to use fzf for the current directory.
    fzf_configure_bindings --directory=\ct
end

# Aliases
alias t="tmux"
alias ta="t a -t"
alias tls="t ls"
alias tn="t new -t"
alias t="tmux"
