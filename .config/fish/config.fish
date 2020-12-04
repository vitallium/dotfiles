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
