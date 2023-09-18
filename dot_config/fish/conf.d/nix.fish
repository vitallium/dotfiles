status is-interactive || exit

fish_add_path --append $HOME/.nix-profile/bin \
    /nix/var/nix/profiles/default/bin

# Allow unfree packages for nix
set -x NIXPKGS_ALLOW_UNFREE 1
