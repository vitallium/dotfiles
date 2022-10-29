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

return require('packer').startup(function(use)
  -- Package manager
  use 'nvim-lua/plenary.nvim'
  use 'folke/tokyonight.nvim'
  -- Fancier statusline
  use 'nvim-lualine/lualine.nvim'
  -- tmux integration
  use 'christoomey/vim-tmux-navigator'
  -- A highly extendable fuzzy finder over lists
  use 'nvim-telescope/telescope.nvim'
  -- Add git related info in the signs columns and popups
  use 'lewis6991/gitsigns.nvim'
  -- Highlight, edit, and navigate code using a fast incremental parsing library
  use("nvim-treesitter/nvim-treesitter", {
        run = ":TSUpdate"
    })
  -- LSP
  -- Integration with progress notifications
  use 'arkav/lualine-lsp-progress'
  -- Collection of configurations for built-in LSP client
  use 'neovim/nvim-lspconfig'
  -- Magit for neovim
  use 'TimUntersberger/neogit'
end)
