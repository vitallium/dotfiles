# put anything needed for a non-interactive shell before this
if not status --is-interactive
  exit 0
end

set -gx GOPATH                     $HOME/.local/go
set -gx GO111MODULE                on
