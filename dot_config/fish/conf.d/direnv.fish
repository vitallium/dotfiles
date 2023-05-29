# put anything needed for a non-interactive shell before this
if status is-interactive; and command -qa direnv
    direnv hook fish | source
end
