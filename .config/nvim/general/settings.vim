" General {{{
set clipboard=unnamedplus            | " System clipboard
set dictionary=/usr/share/dict/words | " Set up a dictionary
set encoding=UTF-8                   | " Default file encoding
set hidden                           | " Make buffers hidden then abandoned
set noautochdir                      | " Don't change dirs automatically
set noerrorbells                     | " No sound
set signcolumn=yes                   | " Show signcolumns
set splitbelow splitright            | " Split defaults
set undofile                         | " Enable undo persistence across sessions
set nobackup                         | " This is recommended by coc
set nowritebackup                    | " This is recommended by coc
" }}}

" Search {{{
set ignorecase         | " Ignores case in search
set smartcase          | " Overrides ignore when capital exists
if has('nvim')
  set inccommand=split | " Displays incremental replacement
endif
" }}}

" Editor {{{
set expandtab      | " Expand tab to spaces
set shiftwidth=2   | " Number of spaces for indentation
set spelllang=en   | " Spell checking
set tabstop=2      | " Number of spaces a <Tab> is
set timeoutlen=500 | " Wait less time for mapped sequences
set smartindent    | " Makes indenting smart
set autoindent     | " Good auto indent
" }}}

" Visual {{{
colorscheme wal                             | " Sets theme to pywal
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
set background=dark                         | " tell vim what the background color looks like
set noshowmode                              | " Don't show mode changes
set novisualbell                            | " Don't display visual bell
set nowrap                                  | " Don't wrap lines
set number                                  | " Show line numbers
set relativenumber                          | " Make line numbers relative
set showmatch                               | " Show matching braces
" }}}

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" You can't stop me
cmap w!! w !sudo tee %
