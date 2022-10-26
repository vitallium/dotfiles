set -gx fish_greeting              ''

# GPG
set -x                             GPG_TTY (tty)
set -x                             SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

set -gx GOPATH                     $HOME/.go
set -gx GOBIN                      $HOME/.go/bin
set -gx GO111MODULE                on

set -gx RIPGREP_CONFIG_PATH       "$HOME/.ripgreprc"

set -gx _ZO_FZF_OPTS              "--height 40% --reverse $FZF_DEFAULT_OPTS"
set -gx FZF_DEFAULT_COMMAND       'rg --files'
set -gx FZF_CTRL_T_COMMAND        'rg --files'

fzf_configure_bindings --variables=

set configs aliases functions paths
for config in $configs
  if test -e $__fish_config_dir/$config.fish
    . $__fish_config_dir/$config.fish
  end
end
set -e configs

if test -e $HOME/.asdf/asdf.fish
  . $HOME/.asdf/asdf.fish
end

eval (asdf exec direnv hook fish)

