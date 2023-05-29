set -gx fish_greeting              ''

# put anything needed for a non-interactive shell before this
if not status --is-interactive
  exit 0
end

# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
set -gx LSP_USE_PLISTS             true
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

if test -e $HOME/.asdf
  . $HOME/.asdf/asdf.fish
end
