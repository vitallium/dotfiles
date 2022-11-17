-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require('plugins')
require('settings')
require('mappings')
require('lsp')

require('plugins.gitlinker')
require('plugins.gitsigns')
require('plugins.lualine')
require('plugins.neogit')
require('plugins.prettier')
require('plugins.whichkey')
