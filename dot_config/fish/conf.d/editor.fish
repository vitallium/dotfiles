status is-interactive || exit

# Setup editor
if not test -z EDITOR
    if type -q nvim
        alias vi='nvim'
        alias vim='nvim'
        alias view='nvim -R'

        set -f editor nvim
    else if type -q vim
        set -f editor vim
    else
        set -f editor vi
    end

    set -gx EDITOR $editor
    set -gx VISUAL $editor
end
