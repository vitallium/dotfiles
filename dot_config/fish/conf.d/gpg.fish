status is-interactive || exit

# GnuPG
if test -e (gpgconf --list-dirs agent-ssh-socket)
  set -g -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
else
  echo (gpgconf --list-dirs agent-ssh-socket) "doesn't exist. Is gpg-agent running?"
end

