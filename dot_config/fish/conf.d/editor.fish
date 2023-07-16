status is-interactive || exit

alias vim='nvim'
alias em='/usr/bin/emacs -nw'
alias emacs="emacsclient -c -a 'emacs'"

# Setup editor
if not test -z EDITOR
    set EDITOR "emacsclient -t -a ''"
    set VISUAL "emacsclient -c -a emacs"
end
