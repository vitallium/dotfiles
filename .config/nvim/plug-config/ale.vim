let g:ale_disable_lsp = 1
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '●'
let g:ale_virtualtext_cursor = 0
let g:ale_echo_msg_format = '[%linter%]: %s'
let g:ale_lint_on_enter = 1
let g:ale_fix_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_linters = {
  \ 'rust': [],
  \ 'ruby': ['ruby', 'rubocop'],
  \ 'python': ['flake8'],
  \ 'markdown': ['vale']
  \ }
