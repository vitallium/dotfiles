set -x LC_ALL en_US.UTF-8

source "$__fish_config_dir/xdg.fish"

# environment variables {{{
set -x fish_greeting              ''
set -x GOPATH                     $HOME/.go
set -x GOBIN                      $HOME/.go/bin
set -x GO111MODULE                on
set -x BAT_THEME                  Dracula
set -gx RIPGREP_CONFIG_PATH       "$HOME/.ripgreprc"
set -gx GITLAB_TOKEN              SET_ME
set -Ux CLUTTER_BACKEND wayland
set -Ux SDL_VIDEODRIVER wayland
set -Ux MOZ_ENABLE_WAYLAND 1
set -Ux XDG_CURRENT_DESKTOP sway
set -Ux XDF_SESSION_DESKTOP sway
# }}}

# fzf {{{
set -Ux FZF_DEFAULT_OPTS "--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4"
set -gx _ZO_FZF_OPTS "--height 40% --reverse $FZF_DEFAULT_OPTS"
set -gx FZF_DEFAULT_COMMAND 'rg --files'
set -gx FZF_CTRL_T_COMMAND 'rg --files'
# }}}

# TODO: Refactor to function
test -e $HOME/.config/fish/aliases.fish; and . $HOME/.config/fish/aliases.fish
test -e $HOME/.config/fish/functions.fish; and . $HOME/.config/fish/functions.fish
test -e $HOME/.config/fish/paths.fish; and . $HOME/.config/fish/paths.fish

test -e $HOME/.config/fish/gitlab.fish; and . $HOME/.config/fish/gitlab.fish
test -e $HOME/.config/fish/chromium.fish; and . $HOME/.config/fish/chromium.fish
test -e $HOME/.config/fish/crystal.fish; and . $HOME/.config/fish/crystal.fish

# asdf
. $HOME/.asdf/asdf.fish

eval (direnv hook fish)

