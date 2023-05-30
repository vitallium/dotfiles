if not status --is-interactive
    exit 0
end

gpgconf --launch gpg-agent
set -x GPG_TTY (tty)
set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
