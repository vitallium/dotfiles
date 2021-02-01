set -x LC_ALL en_US.UTF-8

# fisher {{{
#if not functions -q fisher; and test -z $__FISHER_FLAG
#  set -x __FISHER_FLAG set
#  curl -sL git.io/fisher | source
#  fisher update
#end
# }}}

# environment variables {{{
set -x fish_greeting              ''
set -x EDITOR                     nvim
set -x GOPATH                     $HOME/.go
set -x GOBIN                      $HOME/.go/bin
set -x GO111MODULE                on

# xdg {{{
if test (uname -s) = Linux
  if test ! (set -q XDG_DATA_HOME)
    set -x XDG_DATA_HOME $HOME/.local/share
  end

  if test ! (set -q XDG_CONFIG_HOME)
    set -x XDG_CONFIG_HOME $HOME/.config
  end

  if test ! (set -q XDG_DATA_DIRS)
    set -x XDG_DATA_DIRS /usr/local/share/:/usr/share/
  end

  if test ! (set -q XDG_CONFIG_DIRS)
    set -x XDG_CONFIG_DIRS /etc/xdg
  end

  if test ! (set -q XDG_CACHE_HOME)
    set -x XDG_CACHE_HOME $HOME/.cache
  end

  if test ! (set -q XDG_RUNTIME_DIR)
    set -q UID; set UID (id -u)
    set -x XDG_RUNTIME_DIR "/tmp/$UID-runtime-dir"
    if test ! -d $XDG_RUNTIME_DIR
      mkdir $XDG_RUNTIME_DIR
      chmod 0700 $XDG_RUNTIME_DIR
    end
  end
end
# }}}
# }}}

# TODO: Refactor to function
test -e $HOME/.config/fish/aliases.fish; and . $HOME/.config/fish/aliases.fish
test -e $HOME/.config/fish/functions.fish; and . $HOME/.config/fish/functions.fish
test -e $HOME/.config/fish/paths.fish; and . $HOME/.config/fish/paths.fish

test -e $HOME/.config/fish/gitlab.fish; and . $HOME/.config/fish/gitlab.fish
test -e $HOME/.config/fish/chromium.fish; and . $HOME/.config/fish/chromium.fish
test -e $HOME/.config/fish/crystal.fish; and . $HOME/.config/fish/crystal.fish

# asdf
. $HOME/.asdf/asdf.fish

# GIT is really slow in starship
# eval (starship init fish)

if type -q _pure_prompt_git
  set -g async_prompt_functions _pure_prompt_git
end

# colors {{{
# if test -e $HOME/.config/fish/theme.fish
#  . $HOME/.config/fish/theme.fish
# end
# }}}
