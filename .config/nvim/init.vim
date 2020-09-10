" General {{{
source $HOME/.config/nvim/vim-plug/plugins.vim
source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/general/functions.vim
source $HOME/.config/nvim/keys/mappings.vim
" }}}

" Plugin Configurations {{{
source $HOME/.config/nvim/keys/which-key.vim
source $HOME/.config/nvim/plug-config/fzf.vim
source $HOME/.config/nvim/plug-config/coc.vim
source $HOME/.config/nvim/plug-config/goyo.vim
source $HOME/.config/nvim/plug-config/polyglot.vim
source $HOME/.config/nvim/plug-config/lightline.vim
source $HOME/.config/nvim/plug-config/neoterm.vim
source $HOME/.config/nvim/plug-config/xtabline.vim
source $HOME/.config/nvim/plug-config/floatterm.vim
source $HOME/.config/nvim/plug-config/startify.vim
source $HOME/.config/nvim/plug-config/vista.vim
source $HOME/.config/nvim/plug-config/netrw.vim
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

" Test {{{
let test#strategy = "neovim"

nmap <silent> t<C-n> :TestNearest<CR>
nmap <silent> t<C-f> :TestFile<CR>
nmap <silent> t<C-s> :TestSuite<CR>
nmap <silent> t<C-l> :TestLast<CR>
nmap <silent> t<C-g> :TestVisit<CR>
" }}}
" }}}

" Commands {{{
" CoC Format
command! -nargs=0 Format :call CocAction('format')
" Sets up command for prettier
command! -nargs=0 Prettier :CocCommand prettier.formatFile
" }}}

" vim:fdm=marker
