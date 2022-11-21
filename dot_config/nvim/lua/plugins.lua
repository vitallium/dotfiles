-- Install packer
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end
local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  -- Package manager
  use 'wbthomason/packer.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'folke/tokyonight.nvim'
  use 'nvim-tree/nvim-web-devicons' -- Fancy icons in pop-ups
  -- Fancier statusline
  use 'nvim-lualine/lualine.nvim'
  -- tmux integration
  use 'christoomey/vim-tmux-navigator'
  -- A highly extendable fuzzy finder over lists
  use 'nvim-telescope/telescope.nvim'
  -- Think Emacs directory browser
  use 'nvim-telescope/telescope-file-browser.nvim'
  -- Add git related info in the signs columns and popups
  use 'lewis6991/gitsigns.nvim'
  -- Highlight, edit, and navigate code using a fast incremental parsing library
  use({"nvim-treesitter/nvim-treesitter",
    run = function()
      local ts_update = require('nvim-treesitter.install').update({
        with_sync = true,
      })
      ts_update()
    end,
  })

  -- LSP
  -- Integration with progress notifications
  use 'arkav/lualine-lsp-progress'
  -- Collection of configurations for built-in LSP client
  use 'neovim/nvim-lspconfig'
  -- Manage language servers, linters, etc.
  use 'williamboman/mason.nvim'
  -- Integration mason/lsp
  use 'williamboman/mason-lspconfig.nvim'
  use({
    "glepnir/lspsaga.nvim", -- UI Improvements for LSP
    branch = "main",
  })
  use 'L3MON4D3/LuaSnip' -- Snippets plugin
  use 'hrsh7th/cmp-nvim-lsp' -- LSP source for nvim-cmp
  use 'hrsh7th/cmp-buffer' -- LSP source for nvim-cmp
  use 'hrsh7th/cmp-path' -- LSP source for nvim-cmp
  use 'hrsh7th/cmp-cmdline' -- LSP source for nvim-cmp
  use 'saadparwaiz1/cmp_luasnip' -- Snippets source for nvim-cmp
  use 'onsails/lspkind.nvim' -- Icons in completion dialogue
  use 'hrsh7th/nvim-cmp' -- Autocompletion plugin
  use 'jose-elias-alvarez/null-ls.nvim' -- NeoVim as LSP server

  use 'MunifTanjim/prettier.nvim' -- Prettier for TS/JS formatting
  -- Magit for neovim
  use 'TimUntersberger/neogit'
  -- Generate shareable file permalinks to GIT hosts
  use 'ruifm/gitlinker.nvim'
  -- Like Emacs which key
  use 'folke/which-key.nvim'
end)
