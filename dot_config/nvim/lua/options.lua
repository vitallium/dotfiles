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
  tab = "→ ",
  eol = "↲",
  nbsp = "·",
  lead = "·",
  space = "·",
  trail = "•",
  extends = "⟩",
  precedes = "⟨",
}
opt.list = false -- bool: Show some invisible characters (tabs...
opt.relativenumber = true -- bool: Relative line numbers
opt.smartindent = true

-- [[ Search ]]
opt.ignorecase = true -- bool: Ignore case in search patterns
opt.smartcase = true -- bool: Override ignorecase if search contains capitals
opt.incsearch = true -- bool: Use incremental search

-- use ':grep' to send resulsts to quickfix
-- use ':lgrep' to send resulsts to loclist
if vim.fn.executable("rg") == 1 then
  opt.grepprg = "rg --vimgrep --no-heading --smart-case --hidden"
  opt.grepformat = "%f:%l:%c:%m"
end

-- [[ Whitespace ]]
opt.expandtab = true -- bool: Use spaces instead of tabs
opt.shiftwidth = 2 -- num: Size of an indent
opt.softtabstop = 2 -- num: Number of spaces tabs count for in insert mode
opt.tabstop = 2 -- num: Number of spaces tabs count for

-- [[ Splits ]]
opt.splitright = true -- bool: Place new window to right of current one
opt.splitbelow = true -- bool: Place new window to bottom of current one

-- [[ Completion ]]
opt.completeopt = { "noinsert", "menuone", "noselect" } -- map: Better completion experience

-- [[ Undo ]]
opt.undofile = true -- bool: Enable undo history
opt.undolevels = 10000 -- num: Number of undo changes to keep

-- [[ Misc ]]
opt.wildmode = "longest:full,full" -- str: Command-line completion mode
opt.wildoptions = "pum" -- str: Show completion items using the pop-up-menu (pum)
opt.winminwidth = 5 -- num: Minimum window width
opt.wrap = false -- bool: Disable line wrap

-- [[ Spellcheck ]]
opt.spell = true
opt.spelllang = { "en_us" }
opt.spellfile = vim.fn.expand("~/.local/share/nvim/site/spell/spf.%s.add"):format(vim.o.encoding)

-- [[ Formatting ]]
-- c: auto-wrap comments using textwidth
-- r: auto-insert the current comment leader after hitting <Enter>
-- o: auto-insert the current comment leader after hitting 'o' or 'O'
-- q: allow formatting comments with 'gq'
-- n: recognize numbered lists
-- 1: don't break a line after a one-letter word
-- j: remove comment leader when it makes sense
-- this gets overwritten by ftplugins (:verb set fo)
-- we use autocmd to remove 'o' in '/lua/autocmd.lua'
-- borrowed from tjdevries
opt.formatoptions = opt.formatoptions
  - "a" -- Auto formatting is BAD.
  - "t" -- Don't auto format my code. I got linters for that.
  + "c" -- In general, I like it when comments respect textwidth
  + "q" -- Allow formatting comments w/ gq
  - "o" -- O and o, don't continue comments
  + "r" -- But do continue when pressing enter.
  + "n" -- Indent past the formatlistpat, not underneath it.
  + "j" -- Auto-remove comments if possible.
  - "2" -- I'm not in gradeschool anymore

--[[
  ShDa (viminfo for vim): session data history
  --------------------------------------------
  ! - Save and restore global variables (their names should be without lowercase letter).
  ' - Specify the maximum number of marked files remembered. It also saves the jump list and the change list.
  < - Maximum of lines saved for each register. All the lines are saved if this is not included, <0 to disable pessistent registers.
  % - Save and restore the buffer list. You can specify the maximum number of buffer stored with a number.
  / or : - Number of search patterns and entries from the command-line history saved. o.history is used if it’s not specified.
  f - Store file (uppercase) marks, use 'f0' to disable.
  s - Specify the maximum size of an item’s content in KiB (kilobyte).
      For the viminfo file, it only applies to register.
      For the shada file, it applies to all items except for the buffer list and header.
  h - Disable the effect of 'hlsearch' when loading the shada file.

  :oldfiles - all files with a mark in the shada file
  :rshada   - read the shada file (:rviminfo for vim)
  :wshada   - write the shada file (:wrviminfo for vim)
]]
opt.shada = [[!,'100,<0,s100,h]]
opt.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize"
opt.diffopt = "internal,filler,algorithm:histogram,indent-heuristic"

-- [[ Global options ]]
g.loaded_perl_provider = 0 -- disable Perl support

-- [[ netrw customization (https://shapeshed.com/vim-netrw/) ]]
g.netrw_liststyle = 1 -- wide view
g.netrw_browse_split = 3
g.netrw_altv = 1
g.netrw_winsize = 25

-- Disable some in built plugins completely
local disabled_built_ins = {
  "gzip",
  "zip",
  "zipPlugin",
  "tar",
  "tarPlugin",
  "getscript",
  "getscriptPlugin",
  "vimball",
  "vimballPlugin",
  "2html_plugin",
  "logipat",
  "rrhelper",
  "spellfile_plugin",
  "fzf",
}
for _, plugin in pairs(disabled_built_ins) do
  vim.g["loaded_" .. plugin] = 1
end
