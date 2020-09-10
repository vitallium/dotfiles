" Mappings {{{

let mapleader = " "

" General {{{
" Save file
nnoremap <silent> <leader>w :write<CR>
" Save and close
nnoremap <silent> <leader><S-w> :x<CR>
" Easy align in visual mode
xmap     ga <Plug>(EasyAlign)
" Easy align in normal mode
nmap     ga <Plug>(EasyAlign)
" Make BS/DEL work as expected in visual modes (i.e. delete the selected text)...
xmap <BS> x
" WhichKey
nnoremap <silent> <leader> :WhichKey ' '<CR>
" }}}

" Search {{{
" Open fuzzy files with leader \
nnoremap <silent> <leader>\ :Files<CR>
" Open fuzzy buffers with leader b
nnoremap <silent> <leader>b :Buffers<CR>
" Open ripgrep
nnoremap <silent> <leader>f :Rg<CR>
"
nnoremap <leader>gc :GBranches<CR>

" Open ripgrep agriculture
nmap <leader>/ <Plug>RgRawSearch
" Open ripgrep agriculture for visual selection
vmap <leader>/ <Plug>RgRawVisualSelection
" Remap ** to * now that we are using * for other bindings
nnoremap ** *
" Open ripgrep agriculture for cursor word
nmap */ <Plug>RgRawWordUnderCursor
" Open ripgrep for cursor word
nnoremap <silent> *f :Rg <C-R><C-W><CR>
" Open global ripgrep for cursor word
nnoremap <silent> *<S-f> :Rgg <C-R><C-W><CR>
nnoremap <C-p> :GFiles<CR>
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <leader><CR> :so ~/.config/nvim/init.vim<CR>
" }}}

" Switching {{{
" Next buffer
nnoremap <silent> <Tab> :bnext<CR>
" Previous buffer
nnoremap <silent> <S-Tab> :bprevious<CR>
" Better window navigation
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd j<CR>
nnoremap <leader>l :wincmd l<CR>
" }}}

" View Management {{{
" Create vsplit
nnoremap <silent> <leader>\| :vsplit<CR>
" Create hsplit
nnoremap <silent> <leader>- :split<CR>
" Only window
nnoremap <silent> <leader>o :only<CR>
" Close the current buffer
nnoremap <silent> <leader>c :close<CR>
" Close the current buffer
nnoremap <silent> <leader><S-c> :%close<CR>
" Delete the current buffer
nnoremap <silent> <leader>x :bdelete<CR>
" Delete the current buffer
nnoremap <silent> <leader><S-x> :bdelete!<CR>
" Force close all buffers
nnoremap <silent> <leader>z :%bdelete<CR>
" Close all buffers
nnoremap <silent> <leader><S-z> :%bdelete!<CR>
" Toggle clean
nnoremap <silent> <F1> :Clean<CR>
" }}}

" Conquer of Completion {{{
" Get outline
nnoremap <silent> <leader>co :<C-u>CocList outline<CR>
" Get symbols
nnoremap <silent> <leader>cs :<C-u>CocList -I symbols<CR>
" Get errors
nnoremap <silent> <leader>cl :<C-u>CocList locationlist<CR>
" Get available commands
nnoremap <silent> <leader>cc :<C-u>CocList commands<CR>
" Rename
nmap <leader>$ <Plug>(coc-rename)
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
" }}}

" Custom Tools {{{
" Cycle line number modes
nnoremap <silent> <leader>r :call CycleLineNumbering()<CR>
" }}}

" }}}
