set -gx fish_greeting              ''

# put anything needed for a non-interactive shell before this
if not status --is-interactive
  exit 0
end

# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
set -gx LSP_USE_PLISTS             true
set -gx RIPGREP_CONFIG_PATH       "$HOME/.ripgreprc"

if test -e $HOME/.asdf
  . $HOME/.asdf/asdf.fish
end
