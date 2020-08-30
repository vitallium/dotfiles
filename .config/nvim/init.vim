" General {{{
source $HOME/.config/nvim/vim-plug/plugins.vim
source $HOME/.config/nvim/general/settings.vim
" }}}

" Mappings {{{

" General {{{
" Save file
nnoremap <silent> <Leader>w :write<CR>
" Save and close
nnoremap <silent> <Leader><S-w> :x<CR>
" Easy align in visual mode
xmap     ga <Plug>(EasyAlign)
" Easy align in normal mode
nmap     ga <Plug>(EasyAlign)
" Make BS/DEL work as expected in visual modes (i.e. delete the selected text)...
xmap <BS> x
" WhichKey
nnoremap <silent> <leader> :WhichKey '\'<CR>
" }}}

" Search {{{
" Open fuzzy files with leader \
nnoremap <silent> <Leader>\ :Files<CR>
" Open fuzzy lines with leader l
nnoremap <silent> <Leader>l :Lines<CR>
" Open fuzzy buffers with leader b
nnoremap <silent> <Leader>b :Buffers<CR>
" Open ripgrep
nnoremap <silent> <Leader>f :Rg<CR>
" Open global ripgrep
nnoremap <silent> <Leader><S-f> :Rgg<CR>
" Open ripgrep agriculture
nmap <Leader>/ <Plug>RgRawSearch
" Open ripgrep agriculture for visual selection
vmap <Leader>/ <Plug>RgRawVisualSelection
" Remap ** to * now that we are using * for other bindings
nnoremap ** *
" Open ripgrep agriculture for cursor word
nmap */ <Plug>RgRawWordUnderCursor
" Open ripgrep for cursor word
nnoremap <silent> *f :Rg <C-R><C-W><CR>
" Open global ripgrep for cursor word
nnoremap <silent> *<S-f> :Rgg <C-R><C-W><CR>
nnoremap <C-p> :GFiles<CR>
" }}}

" Switching {{{
" Next buffer
nnoremap <silent> <Tab> :bnext<CR>
" Previous buffer
nnoremap <silent> <S-Tab> :bprevious<CR>
" Alternate file navigation
nnoremap <silent> <Leader>a :A<CR>
" Alternate file navigation vertical split
nnoremap <silent> <Leader><S-a> :AV<CR>
" 
nnoremap <leader>h :call WinMove('h')<CR>
nnoremap <leader>j :call WinMove('j')<CR>
nnoremap <leader>k :call WinMove('k')<CR>
nnoremap <leader>l :call WinMove('l')<CR>
" }}}

" View Management {{{
" Create vsplit
nnoremap <silent> <leader>\| :call Vsplit()<CR>
" Create hsplit
nnoremap <silent> <Leader>- :call Split()<CR>
" Only window
nnoremap <silent> <Leader>o :only<CR>
" Close the current buffer
nnoremap <silent> <Leader>c :close<CR>
" Close the current buffer
nnoremap <silent> <Leader><S-c> :%close<CR>
" Delete the current buffer
nnoremap <silent> <Leader>x :bdelete<CR>
" Delete the current buffer
nnoremap <silent> <Leader><S-x> :bdelete!<CR>
" Force close all buffers
nnoremap <silent> <Leader>z :%bdelete<CR>
" Close all buffers
nnoremap <silent> <Leader><S-z> :%bdelete!<CR>
" Toggle clean
nnoremap <silent> <F1> :Clean<CR>
" }}}

" Conquer of Completion {{{
" Get outline
nnoremap <silent> <Leader>co :<C-u>CocList outline<CR>
" Get symbols
nnoremap <silent> <Leader>cs :<C-u>CocList -I symbols<CR>
" Get errors
nnoremap <silent> <Leader>cl :<C-u>CocList locationlist<CR>
" Get available commands
nnoremap <silent> <Leader>cc :<C-u>CocList commands<CR>
" Rename
nmap <Leader>$ <Plug>(coc-rename)
" Go to definition
nmap gd <Plug>(coc-definition)
" Go to type definition
nmap <silent> gy <Plug>(coc-type-definition)
" Go to implementation
nmap <silent> gi <Plug>(coc-implementation)
" Go to references
nmap <silent> gr <Plug>(coc-references)
" Go to type
nmap <silent> gy <Plug>(coc-type-definition)
" Go to implementation
nmap <silent> gi <Plug>(coc-implementation)
" Get hint
nnoremap <silent> gh :call CocActionAsync('doHover')<CR>
" Go to previous diagnostic
nmap <leader>g[ <Plug>(coc-diagnostic-prev)
" Go to next diagnostic
nmap <leader>g] <Plug>(coc-diagnostic-next)
" }}}

" Neoterm {{{
" Use gx{text-object} in normal mode
nmap gx <Plug>(neoterm-repl-send)
" Send selected contents in visual mode.
xmap gx <Plug>(neoterm-repl-send)
" }}}

" Fuzzy Finder {{{
" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)
" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)
" }}}

" Custom Tools {{{
" Cycle line number modes
nnoremap <silent> <Leader>r :call CycleLineNumbering()<CR>
" }}}

" }}}

" Search Configuration {{{
" Use agriculture as a global no hidden search
let g:agriculture#rg_options = '--no-ignore --hidden'
if executable('rg')
  let $FZF_DEFAULT_OPTS      = '--reverse'
  let $FZF_DEFAULT_COMMAND   = 'rg --files --no-messages "" .'
  let g:rg_derive_root       = 'true'
endif
" Default FZF options with bindings to match layout and select all + none
let $FZF_DEFAULT_OPTS        = join(
  \ [
    \ '--layout=default',
    \ '--info inline',
    \ '--bind ' . join(
      \ [
        \ 'ctrl-a:select-all',
        \ 'ctrl-d:deselect-all',
        \ 'tab:toggle+up',
        \ 'shift-tab:toggle+down',
        \ 'ctrl-p:toggle-preview'
      \ ],
      \ ',',
    \ )
  \ ],
  \ ' ',
\ )
" Default location for FZF
let g:fzf_layout             = { 'window': { 'width': 0.8, 'height': 0.8 } }


" Ctrl-l populates arglist. Use with :cfdo. Only works in :Files
let g:fzf_action = {
  \ 'ctrl-l': {l -> execute('args ' . join(map(l, {_, v -> fnameescape(v)}), ' '))},
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit'
\ }
" }}}

" Plugin Configuration {{{

" Pencil {{{
let g:pencil#autoformat = 0
let g:pencil#textwidth = 80
let g:pencil#conceallevel = 0
" }}}

" Polyglot {{{
let g:polyglot_disabled = ['latex']
" }}}

" Table Mode {{{
let g:table_mode_corner = '|'
" }}}

" Conquer of Completion {{{

" Plugins {{{
let g:coc_global_extensions = [
  \ 'coc-css',
  \ 'coc-eslint',
  \ 'coc-diagnostic',
  \ 'coc-emmet',
  \ 'coc-highlight',
  \ 'coc-html',
  \ 'coc-json',
  \ 'coc-snippets',
  \ 'coc-yaml',
  \ 'coc-lists',
  \ 'coc-pairs',
  \ 'coc-prettier',
  \ 'coc-python',
  \ 'coc-sh',
  \ 'coc-solargraph',
  \ 'coc-stylelint',
  \ 'coc-svg',
  \ 'coc-tslint',
  \ 'coc-tsserver',
  \ 'coc-vimlsp',
  \ 'coc-webpack',
  \ 'coc-yaml',
\ ]
" }}}

" CoC Related Settings {{{
" See coc-settings.json for more configuration
" Some servers have issues with backup files
set nobackup
set nowritebackup
" Remove messages from in-completion menus
set shortmess+=c
" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Use K to show documentation in preview window
nnoremap <silent>K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Run jest for current project
command! -nargs=0 Jest :call  CocAction('runCommand', 'jest.projectTest')
" Run jest for current file
command! -nargs=0 JestCurrent :call  CocAction('runCommand', 'jest.fileTest', ['%'])
" Run jest for current test
nnoremap <leader>te :call CocAction('runCommand', 'jest.singleTest')<CR>
" Init jest in current cwd, require global jest command exists
command! JestInit :call CocAction('runCommand', 'jest.init')
" }}}

" CoC Colors {{{
highlight CocCodeLens gui=italic,bold guifg=#505050
" }}}

" Use tab for trigger completion
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'
" }}}

" Neoterm {{{

" Sets default location that neoterm opens
let g:neoterm_default_mod = 'botright'
let g:neoterm_autojump = 1
let g:neoterm_direct_open_repl = 1
" }}}

" Merge Tool {{{
" 3-way merge
let g:mergetool_layout = 'bmr'
let g:mergetool_prefer_revision = 'local'
" }}}

" Lightline {{{
let g:lightline = {
      \ 'colorscheme': 'wal',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'cocstatus', 'readonly', 'filename', 'modified' ] ],
      \ },
      \ 'component_function': {
      \   'cocstatus': 'coc#status',
      \   'fileformat': 'LightlineFileformat',
      \   'filetype': 'LightlineFiletype'
      \ },
      \ }
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

" Custom Tools {{{

" Cycle through relativenumber + number, number (only), and no numbering.
function! CycleLineNumbering() abort
  execute {
    \ '00': 'set relativenumber   | set number',
    \ '01': 'set norelativenumber | set number',
    \ '10': 'set norelativenumber | set nonumber',
    \ '11': 'set norelativenumber | set number' }[&number . &relativenumber]
endfunction

" }}}

" Commands {{{

" CoC Format
command! -nargs=0 Format :call CocAction('format')
" Sets up command for prettier
command! -nargs=0 Prettier :CocCommand prettier.formatFile

function! LightlineFileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! WinMove(key)
  let t:curwin = winnr()
  exec "wincmd ".a:key
  if (t:curwin == winnr())
    if (match(a:key,'[jk]'))
      wincmd v
    else
      wincmd s
    endif
    exec "wincmd ".a:key
  endif
endfunction
" }}}

" Auto Commands {{{

" }}}

" Modern Plugin Configuration {{{
source $HOME/.config/nvim/keys/which-key.vim
" }}}

" vim:fdm=marker
