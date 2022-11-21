local wk = require("which-key")
local telescope = require("telescope")
local builtin = require("telescope.builtin")

wk.register({
  ['<leader>'] = {builtin.find_files, 'Find file'},
  ['/'] = {builtin.live_grep, 'Grep directory'},
  b = {
    name = 'Buffer',
    b = {builtin.buffers, 'Find buffer'},
    n = {':bn<CR>', 'Next buffer'},
    p = {':bp<CR>', 'Previous buffer'},
  },
  f = {
    name = 'File',
    f = {
      function()
        telescope.extensions.file_browser.file_browser({
          hidden = true,
          grouped = true,
          initial_mode = 'normal',
        })
      end,
      'Browse files',
    },
    s = {':w<CR>', 'Save'},
    S = {':wa<CR>', 'Save all'},
    t = {':NvimTreeToggle<CR>', 'Toggle file tree'},
  },
  g = {
    name = 'Git',
    b = {':Gitsigns toggle_current_line_blame<CR>', 'Toggle blame'},
    g = {':Neogit<CR>', 'Neogit'},
  },
  h = {builtin.help_tags, 'Help'},
  w = {
    name = 'Window',
    c = {':close<CR>', 'Close'},
    s = {':vsplit<CR>', 'Split'},
    v = {':split<CR>', 'Vertical split'},
  },
  q = {
    name = 'Quit',
    q = {':quitall<CR>', 'Quit all'},
  },
}, {prefix = '<leader>'})
