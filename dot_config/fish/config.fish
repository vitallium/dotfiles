# put anything needed for a non-interactive shell before this
if not status --is-interactive
  exit 0
end

set -gx fish_greeting              ''
set -gx EDITOR nvim
set -gx VISUAL $EDITOR

# GPG
gpgconf --launch gpg-agent
set -x                             GPG_TTY (tty)
set -x                             SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)

set -gx GOPATH                     $HOME/.go
set -gx GOBIN                      $HOME/.go/bin
set -gx GO111MODULE                on

set -gx RIPGREP_CONFIG_PATH       "$HOME/.ripgreprc"

set configs aliases paths
for config in $configs
  if test -e $__fish_config_dir/$config.fish
    . $__fish_config_dir/$config.fish
  end
end
set -e configs

if command -qa direnv
  direnv hook fish | source
end

type -q rtx; and rtx activate fish | source

