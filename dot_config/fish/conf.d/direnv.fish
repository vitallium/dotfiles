status is-interactive || exit

if command -qa direnv
    direnv hook fish | source
end
