-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.cmd [[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]]

local use = require('packer').use
require('packer').startup(function()
  -- Package manager
  use 'wbthomason/packer.nvim'
  -- modus themes
  use 'ishan9299/modus-theme-vim'
  -- Tokyo Night
  use 'folke/tokyonight.nvim'
  -- Fancier statusline
  use 'nvim-lualine/lualine.nvim'
  -- tmux integration
  use 'christoomey/vim-tmux-navigator'
  -- A highly extendable fuzzy finder over lists
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  -- Add git related info in the signs columns and popups
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  -- Highlight, edit, and navigate code using a fast incremental parsing library
  use {
    'nvim-treesitter/nvim-treesitter',
    run = function() require('nvim-treesitter.install').update({ with_sync = true }) end,
  }
  -- Additional textobjects for treesitter
  use 'nvim-treesitter/nvim-treesitter-textobjects'

  -- LSP
  -- Integration with progress notifications
  use 'arkav/lualine-lsp-progress'
  -- Collection of configurations for built-in LSP client
  use 'neovim/nvim-lspconfig'
end)
