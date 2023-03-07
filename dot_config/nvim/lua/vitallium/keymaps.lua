local wk = require("which-key")

-- Without prefix:
wk.register({
  ["<c-w>"] = {
    name = "Windows",
    s = { ":split<CR>", "Split window" },
    v = { ":vsplit<CR>", "Split window vertically" },
  },
  ["["] = {
    name = "Previous",
    d = { vim.diagnostic.goto_prev, "Previous diagnostic" },
    b = { ":bp<CR>", "Previous buffer" },
    t = { ":tabprevious<CR>", "Previous tab" },
  },
  ["]"] = {
    name = "Next",
    d = { vim.diagnostic.goto_next, "Next diagnostic" },
    b = { ":bn<CR>", "Next buffer" },
    t = { ":tabnext<CR>", "Next tab" },
  },
})

-- With leader prefix:
wk.register({
  b = {
    name = "Buffer",
    d = { ":Bdelete<CR>", "Delete buffer" },
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
  g = {
    name = "Git",
    t = { ":Gitsigns toggle_current_line_blame<CR>", "Toggle blame" },
    g = { ":Neogit<CR>", "Neogit" },
    G = { ":LazyGit<CR>", "LazyGit" },
  },
  h = {
    name = "Help",
    t = { ":Telescope colorscheme<CR>", "Change Theme" },
    m = { ":Mason<CR>", "Mason" },
  },
  t = {
    name = "Tab",
    c = { ":tabclose<CR>", "Close tab" },
    n = { ":tabnext<CR>", "Next tab" },
    p = { ":tabprevious<CR>", "Previous tab" },
    t = { ":tabnew<CR>", "New tab" },
  },
  w = {
    name = "Window",
    c = { ":close<CR>", "Close window" },
    s = { ":split<CR>", "Split window" },
    v = { ":vsplit<CR>", "Vertically split window" },
  },
  q = { name = "Quit", q = { ":quitall<CR>", "Quit all" } },
}, { prefix = "<leader>" })

-- Terminal:
wk.register({ ["<esc><esc>"] = { "<C-\\><C-n>", "Escape terminal mode." } }, { mode = "t" })
