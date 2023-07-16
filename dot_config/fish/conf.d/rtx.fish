status is-interactive || exit

# rtx (https://github.com/jdxcode/rtx)
if type -q rtx
    command rtx activate fish | source
end
