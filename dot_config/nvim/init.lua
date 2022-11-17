-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require('plugins')
require('settings')
require('mappings')
require('lsp')

require('plugins.gitsigns')
require('plugins.neogit')
require('plugins.lualine')
require('plugins.whichkey')
