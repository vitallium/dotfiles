# put anything needed for a non-interactive shell before this
status is-interactive || exit

# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
set -gx LSP_USE_PLISTS true
