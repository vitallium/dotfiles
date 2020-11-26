" General {{{
syntax on
set clipboard=unnamedplus            | " System clipboard
set dictionary=/usr/share/dict/words | " Set up a dictionary
set encoding=UTF-8                   | " Default file encoding
set hidden                           | " Make buffers hidden then abandoned
set noautochdir                      | " Don't change dirs automatically
set noerrorbells                     | " No sound
set signcolumn=yes                   | " Show signcolumns
set splitbelow splitright            | " Split defaults
set undofile                         | " Enable undo persistence across sessions
set noswapfile                       | " Disable swap
set nobackup                         | " This is recommended by coc
set nowritebackup                    | " This is recommended by coc
set cmdheight=2                      | " Give more space for displaying messages
set showtabline=2                    | " Enable tabline
set updatetime=50
" }}}

" Search {{{
set ignorecase         | " Ignores case in search
set smartcase          | " Overrides ignore when capital exists
set inccommand=split   | " Displays incremental replacement
set nohlsearch         | " TODO
" }}}

" Editor {{{
set expandtab      | " Expand tab to spaces
set shiftwidth=2   | " Number of spaces for indentation
set spelllang=en   | " Spell checking
set tabstop=2      | " Number of spaces a <Tab> is
set timeoutlen=500 | " Wait less time for mapped sequences
set smartindent    | " Makes indenting smart
set autoindent     | " Good auto indent
set complete=.,w,b | " Sources for term and line completions
set completeopt=menu,menuone,noinsert,noselect
" }}}

" Visual {{{
if has('termguicolors')
  set termguicolors                         | " Enable TrueColor support
endif

let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_invert_selection='0'
colorscheme gruvbox                         | " Sets theme to gruvbox
set background=dark                         | " Set background to dark 
set noshowmode                              | " Don't show mode changes
set novisualbell                            | " Don't display visual bell
set nowrap                                  | " Don't wrap lines
set number                                  | " Enable line numbers
set relativenumber                          | " Enable relative line numbers
set showmatch                               | " Show matching braces
set guicursor=                              | " Disable GUI cursor
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgray
" }}}

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" You can't stop me
cmap w!! w !sudo tee %

let g:python3_host_prog = '/usr/bin/python3'
let g:loaded_python3_provider = 1
let g:python_host_prog = '/usr/bin/python2'
