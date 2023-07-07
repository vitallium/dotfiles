# https://github.com/PatrickF1/fzf.fish
if status is-interactive
    set -gx FZF_DEFAULT_OPTS --cycle --filepath-word --height=50% --info=hidden --border=sharp
    if functions -q fzf_configure_bindings
        set fzf_preview_file_cmd bat --line-range :100 --color=always --plain
        set fzf_preview_dir_cmd lsd --tree --depth=1

        set fzf_fd_opts --hidden
        set fzf_dir_opts --bind 'ctrl-e:execute(command $EDITOR {} >/dev/tty)'

        # Bind Ctrl-t to use fzf for the current directory.
        fzf_configure_bindings --directory=\ct
    end
end
