local wk = require("which-key")

local function map(modes, lhs, rhs, opts)
  opts = opts or {}
  opts.noremap = opts.noremap == nil and true or opts.noremap
  if type(modes) == "string" then modes = { modes } end
  for _, mode in ipairs(modes) do
    if type(rhs) == "string" then
      vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
    else
      opts.callback = rhs
      vim.api.nvim_set_keymap(mode, lhs, "", opts)
    end
  end
end

-- Faster scrolling
map("n", "<c-e>", "3<c-e>")
map("n", "<c-y>", "3<c-y>")

-- Ex-mode is weird and not useful so it seems better to repeat the last macro
map("n", "Q", "@@")

wk.setup({
  plugins = {
    marks = true,     -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    spelling = {
      enabled = true,   -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20, -- how many suggestions should be shown in the list?
    },
    presets = {
      operators = false,   -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = false,     -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = false,     -- default bindings on <c-w>
      nav = true,          -- misc bindings to work with windows
      z = true,            -- bindings for folds, spelling and others prefixed with z
      g = true,            -- bindings for prefixed with g
    },
  },
  key_labels = {
    -- override the label used to display some keys. It doesn't effect WK in any other way.
    -- For example:
    ["<space>"] = "SPC",
    ["<cr>"] = "RET",
    ["<tab>"] = "TAB",
  },
  icons = {
    breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
    separator = "➜", -- symbol used between a key and it's label
    group = "+",      -- symbol prepended to a group
  },
})

-- Without prefix:
wk.register({
  ["<c-w>"] = {
    name = "Windows",
    s = { ":split<CR>", "Split window" },
    v = { ":vsplit<CR>", "Split window vertically" },
  },
})

-- With leader prefix:
wk.register({
  b = {
    name = "Buffer",
    n = { ":bn<CR>", "Next buffer" },
    p = { ":bp<CR>", "Previous buffer" },
  },
  c = {
    name = "Code",
    d = { ":Trouble document_diagnostics<CR>", "Show diagnostics" },
    D = {
      ":Trouble workspace_diagnostics<CR>",
      "Show workspace diagnostics",
    },
    e = { vim.diagnostic.open_float, "Floating diagnostic" },
  },
  f = {
    name = "File",
    o = { ":Other<CR>", "Open other file" },
    O = { ":OtherVSplit<CR>", "Open other file in split" },
    s = { ":w<CR>", "Save" },
    S = { ":wa<CR>", "Save all" },
  },
  h = {
    name = "Help",
    t = { ":Telescope colorscheme<CR>", "Change Theme" },
    m = { ":Mason<CR>", "Mason" },
  },
  t = {
    name = "Toggle",
    c = { ":set nolist!<CR>", "Toggle invisible chars" },
  },
  w = {
    name = "Window",
    c = { ":close<CR>", "Close window" },
    s = { ":split<CR>", "Split window" },
    v = { ":vsplit<CR>", "Vertically split window" },
    h = { ":wincmd h<CR>", "Go to window to the left" },
    j = { ":wincmd j<CR>", "Go to window to the bottom" },
    k = { ":wincmd k<CR>", "Go to window to the top" },
    l = { ":wincmd l<CR>", "Go to window to the right" },
  },
  q = { name = "Quit", q = { ":quitall<CR>", "Quit all" } },
}, { prefix = "<leader>" })

-- Terminal:
wk.register({ ["<esc><esc>"] = { "<C-\\><C-n>", "Escape terminal mode." } }, { mode = "t" })
