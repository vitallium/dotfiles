let g:lightline = {
\ 'colorscheme': 'gruvbox',
\ 'active': {
\   'left': [ [ 'mode', 'paste' ],
\             [ 'cocstatus', 'readonly', 'filename', 'modified' ] ],
\ },
\ 'component_function': {
\   'cocstatus': 'coc#status',
\ },
\ }

" vim:fdm=marker
