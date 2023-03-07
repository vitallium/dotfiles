-- Options.
-- must be first to make mapping correct
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- [[ opts.lua ]]
local opt = vim.opt

-- [[ Misc ]]
opt.timeoutlen = 300 -- num: Timeout, e.g. for which-key
opt.clipboard = "unnamedplus" -- str: Clipboard integration with macOS

-- [[ Context ]]
opt.colorcolumn = "80" -- str: Show col for max line length
opt.number = true -- bool: Show line numbers
opt.scrolloff = 5 -- int: Min num lines of context
opt.signcolumn = "yes" -- str: Show the sign column

-- [[ Filetypes ]]
opt.encoding = "utf8" -- str: String encoding to use
opt.fileencoding = "utf8" -- str: File encoding to use

-- [[ Theme ]]
opt.syntax = "ON" -- str: Allow syntax highlighting
opt.termguicolors = true -- bool: If term supports ui color then enable
opt.cursorline = true -- bool: Highlight current line
opt.listchars = "space:·,tab:>~,trail:~,extends:>,precedes:<" -- str: how to render invisible chars
opt.list = true -- bool: Show some invisible characters (tabs...
opt.relativenumber = true -- bool: Relative line numbers

-- [[ Search ]]
opt.ignorecase = true -- bool: Ignore case in search patterns
opt.smartcase = true -- bool: Override ignorecase if search contains capitals
opt.incsearch = true -- bool: Use incremental search
opt.grepformat = "%f:%l:%c:%m"
opt.grepprg = "rg --vimgrep" -- str: Configure grep to use rg

-- [[ Whitespace ]]
opt.expandtab = true -- bool: Use spaces instead of tabs
opt.shiftwidth = 2 -- num: Size of an indent
opt.softtabstop = 2 -- num: Number of spaces tabs count for in insert mode
opt.tabstop = 2 -- num: Number of spaces tabs count for

-- [[ Splits ]]
opt.splitright = true -- bool: Place new window to right of current one
opt.splitbelow = true -- bool: Place new window to bottom of current one

-- [[ Completion ]]
opt.completeopt = "menu,menuone,noselect" -- str: Better completion experience

-- [[ Undo ]]
opt.undofile = true
opt.undolevels = 10000

opt.wildmode = "longest:full,full" -- Command-line completion mode
opt.winminwidth = 5 -- Minimum window width
opt.wrap = false -- Disable line wrap

if vim.fn.has("nvim-0.9.0") == 1 then
  opt.splitkeep = "screen"
  opt.shortmess:append({ C = true })
end

-- Fix markdown indentation settings
vim.g.markdown_recommended_style = 0
