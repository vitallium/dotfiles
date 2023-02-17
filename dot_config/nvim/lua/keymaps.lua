-- Which-Key
local wk = require('which-key')
local telescope = require('telescope')
local builtin = require('telescope.builtin')

-- Without prefix:
wk.register({
  ["<c-w>"] = {
    name = "Windows",
    v = { ":vsplit<CR>", "Split window" },
    s = { ":split<CR>", "Split window vertically" },
  },
  ["["] = {
    name = "Previous",
    d = { vim.diagnostic.goto_prev, "Previous diagnostic" },
    b = { ":bp<CR>", "Previous buffer" },
  },
  ["]"] = {
    name = "Next",
    d = { vim.diagnostic.goto_next, "Next diagnostic" },
    b = { ":bn<CR>", "Next buffer" },
  },
  ["<esc>"] = { ":nohlsearch<CR>", "Disable search highlights" },
})

-- With leader prefix:
wk.register({
  ["<leader>"] = { builtin.find_files, "Find file" },
  ["/"] = { builtin.live_grep, "Grep directory" },
  [","] = { builtin.buffers, "Find buffer" },
  b = {
    name = "Buffer",
    b = { builtin.buffers, "Find buffer" },
    d = { ":Bdelete<CR>", "Delete buffer" },
    n = { ":bn<CR>", "Next buffer" },
    p = { ":bp<CR>", "Previous buffer" },
  }, f = {
    name = "File",
    f = {
      function()
        telescope.extensions.file_browser.file_browser({
          path = "%:p:h",
          select_buffer = true,
          hidden = true,
          grouped = true,
        })
      end,
      "Browser directory for current buffer",
    },
    o = { ":Other<CR>", "Open other file" },
    O = { ":OtherVSplit<CR>", "Open other file in split" },
    s = { ":w<CR>", "Save" },
    r = { builtin.oldfiles, "Open recent file" },
  },
  g = {
    name = "Git",
    b = { ":Gitsigns toggle_current_line_blame<CR>", "Toggle blame" },
    g = { ":Neogit<CR>", "Neogit" },
  },
  h = { builtin.help_tags, "Help" },
  w = {
    name = "Window",
    c = { ":close<CR>", "Close window" },
    d = { ":close<CR>", "Close window" },
    v = { ":vsplit<CR>", "Vertically split window" },
    s = { ":split<CR>", "Split window" },
  },
  o = {
    p = { ":NvimTreeToggle<CR>", "Toggle Nvim Tree" },
  },
  q = { name = "Quit", q = { ":quitall<CR>", "Quit all" } },
}, { prefix = "<leader>" })


-- Terminal:
wk.register({ ["<esc><esc>"] = { "<C-\\><C-n>", "Escape terminal mode." } }, { mode = "t" })
