# put anything needed for a non-interactive shell before this
if not status --is-interactive
  exit 0
end

set -gx fish_greeting              ''
set -gx EDITOR nvim
set -gx VISUAL $EDITOR

# GPG
set -x                             GPG_TTY (tty)
set -x                             SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

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

if command -sq direnv
  direnv hook fish | source
end

# if test -e $HOME/.asdf/asdf.fish
#   . $HOME/.asdf/asdf.fish
# end

if command -sq rtx
  rtx activate fish | source
end

if command -sq gdircolors
  eval (gdircolors -c)
end

if command -qa bat
  alias cat=bat
  set -Ux MANPAGER "sh -c 'col -bx | bat -l man -p'"
end

if command -qs fzf
  set -Ux FZF_DEFAULT_OPTS "\
		--color=bg:#000000,bg+:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
		--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
		--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"
end
