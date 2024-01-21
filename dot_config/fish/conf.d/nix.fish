status is-interactive || exit

# fish_add_path --append $HOME/.nix-profile/bin \
#     /nix/var/nix/profiles/default/bin

if test -f "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish"
    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
end

# Allow unfree packages for nix
set -x NIXPKGS_ALLOW_UNFREE 1
