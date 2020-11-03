set -x LC_ALL en_US.UTF-8

set -gx fish_greeting              ''
set -gx EDITOR                     'nvim'

# fzf
set -gx FZF_PREVIEW_COMMAND        'bat {}'

# TODO: Refactor to function
test -e {$HOME}/.config/fish/aliases.fish; and . {$HOME}/.config/fish/aliases.fish
test -e {$HOME}/.config/fish/functions.fish; and . {$HOME}/.config/fish/functions.fish
test -e {$HOME}/.config/fish/paths.fish; and . {$HOME}/.config/fish/paths.fish

test -e {$HOME}/.config/fish/gitlab.fish; and . {$HOME}/.config/fish/gitlab.fish
test -e {$HOME}/.config/fish/chromium.fish; and . {$HOME}/.config/fish/chromium.fish
test -e {$HOME}/.config/fish/crystal.fish; and . {$HOME}/.config/fish/crystal.fish

. {$HOME}/.asdf/asdf.fish

kitty + complete setup fish | source

pathadd ~/.local/bin
# GIT is really slow in starship
# eval (starship init fish)
