-- [[ opts.lua ]]
local opt = vim.opt
local g = vim.g

-- must be first to make mapping correct
g.mapleader = " "
g.maplocalleader = ","

-- [[ Misc ]]
opt.timeoutlen = 300 -- num: Timeout, e.g. for which-key
opt.clipboard = "unnamedplus" -- str: Clipboard integration with macOS
opt.updatetime = 1000 -- num: Faster update times.
opt.hidden = true -- bool: This makes vim act like all other editors, buffers can exist in the background without being in a window.
opt.splitkeep = "screen" -- screen:
opt.shortmess:append({ C = true })

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
opt.listchars = {
  eol = "↲",
  conceal = "┊",
  precedes = ">",
  extends = "<",
  trail = "·",
  tab = "» ",
  nbsp = "␣",
}
opt.list = false -- bool: Show some invisible characters (tabs...
opt.relativenumber = true -- bool: Relative line numbers
opt.smartindent = true

-- [[ Search ]]
opt.ignorecase = true -- bool: Ignore case in search patterns
opt.smartcase = true -- bool: Override ignorecase if search contains capitals
opt.incsearch = true -- bool: Use incremental search
opt.grepformat = "%f:%l:%c:%m"
opt.grepprg = "rg --vimgrep --smart-case --follow" -- str: Configure grep to use rg

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
opt.undofile = true -- bool: Enable undo history
opt.undolevels = 10000 -- num: Number of undo changes to keep

-- [[ Misc ]]
opt.wildmode = "longest:full,full" -- Command-line completion mode
opt.winminwidth = 5 -- Minimum window width
opt.wrap = false -- Disable line wrap

-- [[ Spellcheck ]]
opt.spell = true
opt.spelllang = { "en_us" }
opt.spellfile = vim.fn.expand("~/.local/share/nvim/site/spell/spf.%s.add"):format(vim.o.encoding)

-- [[ Global options ]]
g.loaded_perl_provider = 0 -- disable Perl support

-- [[ netrw customization (https://shapeshed.com/vim-netrw/) ]]
g.netrw_liststyle = 1 -- wide view
g.netrw_browse_split = 3
g.netrw_altv = 1
g.netrw_winsize = 25

-- [[ Disable some in built plugins completely ]]
g.loaded_matchparen = 1
g.loaded_matchit = 1
g.loaded_2html_plugin = 1
g.loaded_getscriptPlugin = 1
g.loaded_gzip = 1
g.loaded_logipat = 1
g.loaded_rrhelper = 1
g.loaded_spellfile_plugin = 1
g.loaded_tarPlugin = 1
g.loaded_vimballPlugin = 1
g.loaded_zipPlugin = 1
