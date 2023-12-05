status is-interactive || exit

# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
set -gx LSP_USE_PLISTS true

if test -d "/Applications/Emacs.app/Contents/MacOS/bin"
    set -x PATH "/Applications/Emacs.app/Contents/MacOS/bin" $PATH
    alias emacs "emacs -nw" # Always launch "emacs" in terminal mode.
end
