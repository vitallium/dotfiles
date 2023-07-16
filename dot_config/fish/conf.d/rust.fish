status is-interactive || exit

# Rust
if type -q sccache
    set -gx RUSTC_WRAPPER sccache
end
