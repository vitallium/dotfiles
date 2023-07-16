status is-interactive || exit

# Configure terminfo for wezterm
if test $TERM = wezterm; and not infocmp wezterm &>/dev/null
    set -l TERMINFO_DIR $HOME/.terminfo

    if fish_is_root_user
        set TERMINFO_DIR /usr/share/terminfo
    end

    # Comes in via Chezmoi externals.
    if test -f $XDG_CACHE_HOME/wezterm/terminfo
        /usr/bin/tic -x -o $TERMINFO_DIR $XDG_CACHE_HOME/wezterm/terminfo
    end
end
