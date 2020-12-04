" Do NOT initiate ALE when doing short-lived diffs.
if &diff
  finish
endif

let g:ale_linters = {
\  'ruby':       ['rubocop'],
\  'python':     ['flake8', 'pylint'],
\  'javascript': ['eslint'],
\  'vue':        ['eslint']
\}

let g:ale_fixers = {
\  'javascript': ['prettier', 'eslint'],
\  'typescript': ['prettier', 'tslint'],
\  'vue':        ['eslint'],
\  'scss':       ['prettier'],
\  'html':       ['prettier'],
\  'ruby':       ['rubocop']
\}

let g:ale_completion_enabled       = 0
let g:ale_fix_on_save              = 1
let g:ale_hover_cursor             = 0
let g:ale_lint_on_enter            = 0
let g:ale_lint_on_filetype_changed = 0
let g:ale_lint_on_insert_leave     = 0
let g:ale_lint_on_save             = 1
let g:ale_lint_on_text_changed     = 'never'
let g:ale_linters_explicit         = 1
let g:ale_open_list                = 0
let g:ale_sign_error               = '✖'
let g:ale_sign_warning             = '✖'
let g:ale_sign_info                = '●'
let g:ale_sign_priority            = 50
let g:ale_echo_cursor              = 0
let g:ale_virtualtext_cursor       = 1
let g:ale_virtualtext_prefix       = ' ■ '
let g:ale_ruby_rubocop_executable  = 'bundle'
let g:ale_sign_column_always       = 1

" ALE fix and toggle mappings.
nmap <Space>f <Plug>(ale_fix)
nmap <Space>l <Plug>(ale_toggle_buffer)
" Navigate errors and warnings using unimpaired-style mappings.
nmap [w <Plug>(ale_previous)zz
nmap ]w <Plug>(ale_next)zz
nmap [W <Plug>(ale_first)zz
nmap ]W <Plug>(ale_last)zz
" Toggle location list.
nnoremap <silent> <Leader>l :call location_list#Toggle()<CR>
