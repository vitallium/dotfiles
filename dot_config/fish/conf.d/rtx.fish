if status is-interactive; and type -q rtx
    set -gx RTX_USE_TOML 1

    command rtx activate fish | source
end
