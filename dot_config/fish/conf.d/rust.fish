if status is-interactive; and type -q sccache
    set -gx RUSTC_WRAPPER sccache
end
