# put anything needed for a non-interactive shell before this
if not status --is-interactive
    exit 0
end

alias t="tmux"
alias ta="t a -t"
alias tls="t ls"
alias tn="t new -t"
alias t="tmux"
