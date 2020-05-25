# brew
pathadd /usr/local/sbin

# asdf
pathadd ~/.asdf/shims

# golang
set -gx GOPATH $HOME/go
set -gx GOBIN $GOPATH/bin
pathadd $GOBIN

# rust
pathadd ~/.cargo/bin

# jsvu
pathadd ~/.jsvu

