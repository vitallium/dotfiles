# Change fisher path so we don't have stuff installed by fisher in our VCS
set -g fisher_path $HOME/.config/fish/fisher

set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..-1]
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..-1]

# Plugins need to be loaded last so we load them in config.fish
