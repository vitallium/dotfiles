local telescope = require("telescope")
local telescopeConfig = require("telescope.config")

function map(mode, shortcut, command)
  vim.api.nvim_set_keymap(mode, shortcut, command, { noremap = true, silent = true })
end

function nmap(shortcut, command)
  map('n', shortcut, command)
end

-- Find files using Telescope command-line sugar.
nmap("<C-p>", "<cmd>Telescope find_files<cr>")
nmap("<leader>bb", "<cmd>Telescope buffers<cr>")
-- Neogit
vim.keymap.set('n', '<leader>g', ':Neogit<CR>', { noremap = true })
-- Switch meaning of vertical and horizontal split
vim.keymap.set('', '<C-w>s', '<C-w>v', { noremap = true })
vim.keymap.set('', '<C-w>v', '<C-w>s', { noremap = true })

local builtin = require('telescope.builtin')

vim.keymap.set('n', '<leader><leader>', builtin.find_files, { noremap = true })
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
telescope.load_extension 'file_browser'

vim.keymap.set('n', '<leader>ff', function()
  telescope.extensions.file_browser.file_browser({
    hidden = true,
    grouped = true,
    initial_mode = "normal",
  })
end, { noremap = true })
