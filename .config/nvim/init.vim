" General {{{
source $HOME/.config/nvim/vim-plug/plugins.vim
source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/general/functions.vim
source $HOME/.config/nvim/keys/mappings.vim
source $HOME/.config/nvim/keys/which-key.vim
" }}}

" Plugin Configurations {{{
source $HOME/.config/nvim/plug-config/fzf.vim
source $HOME/.config/nvim/plug-config/coc.vim
source $HOME/.config/nvim/plug-config/goyo.vim
source $HOME/.config/nvim/plug-config/polyglot.vim
source $HOME/.config/nvim/plug-config/lightline.vim
source $HOME/.config/nvim/plug-config/xtabline.vim
source $HOME/.config/nvim/plug-config/neoterm.vim
source $HOME/.config/nvim/plug-config/floatterm.vim
source $HOME/.config/nvim/plug-config/startify.vim
source $HOME/.config/nvim/plug-config/vista.vim
source $HOME/.config/nvim/plug-config/netrw.vim
source $HOME/.config/nvim/plug-config/ale.vim
" }}}

" Plugin Configuration {{{

" Pencil {{{
let g:pencil#autoformat = 0
let g:pencil#textwidth = 80
let g:pencil#conceallevel = 0
" }}}

" Table Mode {{{
let g:table_mode_corner = '|'
" }}}

" Merge Tool {{{
" 3-way merge
let g:mergetool_layout = 'bmr'
let g:mergetool_prefer_revision = 'local'
" }}}

" GitLab {{{
" let g:gitlab_api_keys = {'gitlab.com': ''}
" }}}

" }}}

" vim:fdm=marker
