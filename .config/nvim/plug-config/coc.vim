" Plugins {{{
let g:coc_global_extensions = [
  \ 'coc-css',
  \ 'coc-diagnostic',
  \ 'coc-eslint',
  \ 'coc-highlight',
  \ 'coc-html',
  \ 'coc-graphql',
  \ 'coc-json',
  \ 'coc-lists',
  \ 'coc-pairs',
  \ 'coc-prettier',
  \ 'coc-python',
  \ 'coc-sh',
  \ 'coc-snippets',
  \ 'coc-solargraph',
  \ 'coc-stylelint',
  \ 'coc-svg',
  \ 'coc-tslint',
  \ 'coc-tsserver',
  \ 'coc-vetur',
  \ 'coc-vimlsp',
  \ 'coc-webpack',
  \ 'coc-yaml',
  \ 'coc-yaml',
\ ]
" }}}

" Settings {{{
" See coc-settings.json for more configuration

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
" }}}

" CoC Colors {{{
highlight CocCodeLens gui=italic,bold guifg=#505050
" }}}

let g:coc_snippet_next = '<tab>'
