status is-interactive || exit

# Enable integration with direnv
if command -qa direnv
    direnv hook fish | source
end
