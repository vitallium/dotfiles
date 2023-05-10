-- Options.
-- must be first to make mapping correct
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- [[ opts.lua ]]
local opt = vim.opt

-- [[ Misc ]]
opt.timeoutlen = 300 -- num: Timeout, e.g. for which-key
opt.clipboard = "unnamedplus" -- str: Clipboard integration with macOS
opt.updatetime = 1000 -- num: Faster update times.
opt.hidden = true -- bool: This makes vim act like all other editors, buffers can exist in the background without being in a window.
opt.lazyredraw = true -- bool: Make macros render faster (lazy draw)

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
opt.listchars = "eol:↲,tab:» ,trail:·,extends:<,precedes:>,conceal:┊,nbsp:␣" -- str: how to render invisible chars
opt.list = false -- bool: Show some invisible characters (tabs...
opt.relativenumber = true -- bool: Relative line numbers

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
opt.undofile = true
opt.undolevels = 10000

-- [[ Misc ]]
opt.wildmode = "longest:full,full" -- Command-line completion mode
opt.winminwidth = 5 -- Minimum window width
opt.wrap = false -- Disable line wrap

-- [[ Session ]]
opt.sessionoptions = "buffers,curdir,tabpages,winsize"
opt.shada = ""

-- [[ Spellcheck ]]
opt.spell = true
opt.spelllang = { "en_us" }
opt.spellfile = vim.fn.expand("~/.local/share/nvim/site/spell/spf.%s.add"):format(vim.o.encoding)

-- [[ Global options ]]
vim.g.loaded_perl_provider = 0 -- disable Perl support
-- Disable some in built plugins completely
vim.g.loaded_matchparen = 1
vim.g.loaded_matchit = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_getscriptPlugin = 1
vim.g.loaded_gzip = 1
vim.g.loaded_logipat = 1
vim.g.loaded_rrhelper = 1
vim.g.loaded_spellfile_plugin = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_vimballPlugin = 1
vim.g.loaded_zipPlugin = 1
-- Disable virtual text for all diagnostics
vim.diagnostic.config({
  underline = true,
  signs = true,
  virtual_text = false,
  update_in_insert = false,
  severity_sort = true,
  float = {
    header = false,
    source = "always",
    border = "rounded",
    focusable = false,
  },
})

if vim.fn.has("nvim-0.9") == 1 then
  opt.splitkeep = "screen"
  opt.shortmess:append({ C = true })
end
