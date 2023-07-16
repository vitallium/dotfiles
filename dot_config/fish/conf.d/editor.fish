status is-interactive || exit

alias vim='nvim'

# Setup editor
if not test -z EDITOR
    if type -q nvim
        abbr --add vi nvim
        abbr --add vim nvim
        abbr --add view 'nvim -R'

        set -f editor nvim
    else if type -q vim
        set -f editor vim
    else
        set -f editor vi
    end

    set -gx EDITOR $editor
    set -gx VISUAL $editor
end
