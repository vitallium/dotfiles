local opts = { noremap = true, silent = true }
-- Shorten function name
local keymap = vim.keymap.set

local wk = require("which-key")

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Normal --
-- disable Ex mode, I always enter in it by mistake
keymap("n", "Q", "<Nop>", opts)

-- move record macro to Q instead of q
keymap("n", "Q", "q", opts)
keymap("n", "q", "<Nop>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

wk.setup({
  plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    spelling = {
      enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20, -- how many suggestions should be shown in the list?
    },
    presets = {
      operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = false, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = false, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true, -- bindings for prefixed with g
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
    group = "+", -- symbol prepended to a group
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
    d = { ":TroubleToggle document_diagnostics<CR>", "Show diagnostics" },
    D = {
      ":TroubleToggle workspace_diagnostics<CR>",
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
    w = { ":set wrap!<CR>", "Toggle word wrap" },
  },
  l = {
    name = "Launch",
  },
  o = {
    name = "Open",
  },
  p = {
    name = "Project",
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

-- With local predix:
wk.register({
  t = {
    name = "Test",
  },
}, { prefix = "<localleader>" })
