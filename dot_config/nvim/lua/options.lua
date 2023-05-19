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
opt.splitkeep = "screen" -- string: Keep the text on the same screen line.
opt.shortmess = "filnxtToOFWIcC"

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
opt.spellfile = vim.fn.stdpath("config") .. "/spell/en.utf-8.add"
opt.spelloptions:append({ "camel" })
opt.spellsuggest = "best,9"

-- [[ Formatting ]]
-- This order is the same as the documentation.
opt.formatoptions = {
  t = false, -- Auto-wrap lines using text width value.
  c = true, -- Auto-wrap comments using 'textwidth', inserting the current comment leader automatically.
  r = true, -- Automatically insert the current comment leader after hitting <Enter> in Insert mode.
  o = false, -- Insert the current comment leader after hitting 'o' or 'O' in Normal mode.
  q = true, -- Allow formatting of comments with "gq".
  a = false, -- Automatic formatting of paragraphs. Every time text is inserted or deleted the paragraph will be reformatted.
  n = true, -- When formatting text, recognize numbered lists.
  [2] = true, -- Use the indent of the second line of a paragraph for the rest of the paragraph.
  l = true, -- Long lines are not broken in insert mode.
  [1] = true, -- Don't break a line after a one-letter word.
  j = true, -- Where it makes sense, remove a comment leader when joining lines.
}

vim.opt.sessionoptions = {
  "buffers",
  "curdir",
  "globals",
  "options",
}

-- Preferences
g.border = "single"

-- [[ Global options ]]
g.loaded_perl_provider = 0 -- disable Perl support

-- [[ netrw customization (https://shapeshed.com/vim-netrw/) ]]
g.netrw_liststyle = 1 -- wide view
g.netrw_browse_split = 3
g.netrw_altv = 1
g.netrw_winsize = 25

-- Flag for disabling null-ls and others for large files.
vim.g.large_file = false

-- Load clipboard.vim faster.
if vim.g.os == "Darwin" then
  vim.g.clipboard = {
    name = "macOS",
    copy = {
      ["+"] = "pbcopy",
      ["*"] = "pbcopy",
    },
    paste = {
      ["+"] = "pbpaste",
      ["*"] = "pbpaste",
    },
    cache_enabled = false,
  }

  vim.g.opener = "open"
else
  if vim.fn.executable("xdg-open") == 1 then
    vim.g.opener = "xdg-open"
  else
    vim.g.opener = ""
  end
end

-- [[ Diagnostic ]]
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-Customization#show-source-in-diagnostics
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-Customization#change-prefixcharacter-preceding-the-diagnostics-virtual-text
local icons = {
  error = "󰅚 ",
  warn = "󰀪 ",
  info = " ",
  hint = "󰌶 ",
}

local function sign(opts)
  vim.fn.sign_define(opts.highlight, {
    text = opts.icon,
    texthl = opts.highlight,
    numhl = opts.linehl ~= false and opts.highlight .. "Nr" or nil,
    culhl = opts.linehl ~= false and opts.highlight .. "CursorNr" or nil,
    linehl = opts.linehl ~= false and opts.highlight .. "Line" or nil,
  })
end

sign({ highlight = "DiagnosticSignError", icon = icons.error })
sign({ highlight = "DiagnosticSignWarn", icon = icons.warn })
sign({ highlight = "DiagnosticSignInfo", linehl = false, icon = icons.info })
sign({ highlight = "DiagnosticSignHint", linehl = false, icon = icons.hint })
vim.diagnostic.config({
  float = {
    border = vim.g.border,
    focusable = true,
    header = { " Issues:" },
    max_height = math.min(math.floor(vim.o.lines * 0.3), 30),
    max_width = math.min(math.floor(vim.o.columns * 0.7), 100),
    prefix = function(diag)
      local level = vim.diagnostic.severity[diag.severity]
      local prefix = string.format("%s ", icons[level:lower()])
      return prefix, "Diagnostic" .. level:gsub("^%l", string.upper)
    end,
    source = "if_many",
  },
  underline = true,
  signs = true,
  severity_sort = true,
  update_in_insert = false, -- https://www.reddit.com/r/neovim/comments/pfk209/nvimlsp_too_fast/
  virtual_text = {
    format = function(diagnostic)
      -- https://www.reddit.com/r/neovim/comments/q9dxnp/set_lsp_messages_max_width/
      return string.sub(diagnostic.message, 1, 80)
    end,
    prefix = "❰",
    source = "if_many",
    spacing = 1,
  },
})
