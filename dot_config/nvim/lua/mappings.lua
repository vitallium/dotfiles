local telescope = require("telescope")
local telescopeConfig = require("telescope.config")
local builtin = require('telescope.builtin')

-- Neogit
vim.keymap.set('n', '<leader>g', ':Neogit<CR>', { noremap = true })
-- Switch meaning of vertical and horizontal split
vim.keymap.set('', '<C-w>s', '<C-w>v', { noremap = true })
vim.keymap.set('', '<C-w>v', '<C-w>s', { noremap = true })


vim.keymap.set('n', '<leader><leader>', builtin.find_files, { noremap = true })
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
