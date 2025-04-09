return {
  {
    'ibhagwan/fzf-lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    keys = {
      { '<leader><leader>', '<cmd>FzfLua files<CR>', desc = 'Find file' },
      { '<leader>pf', '<cmd>FzfLua files<CR>', desc = 'Find file' },
      { '<leader>bb', '<cmd>FzfLua buffers<CR>', desc = 'List buffers' },
      { '<leader>,', '<cmd>FzfLua buffers<CR>', desc = 'List buffers' },
      { '<leader>br', '<cmd>e!<CR>', desc = 'Reload buffer' },
      { '<leader>bp', '<cmd>bprevious<CR>', desc = 'Previous buffer' },
      { '<leader>bn', '<cmd>bnext<CR>', desc = 'Next buffer' },
      {
        '<leader>ff',
        function()
          require('fzf-lua').files { cwd = vim.fn.expand '%:p:h' }
        end,
        desc = 'Find file in current dir',
      },
      { "<leader>'", '<cmd>FzfLua resume<CR>', desc = 'Resume last FzfLua command' },
      { '<leader>ht', '<cmd>FzfLua colorschemes<CR>', desc = 'Change colorscheme' },
      { '<leader>fr', '<cmd>FzfLua oldfiles<CR>', desc = 'Recent files' },
      { '<leader>sp', '<cmd>FzfLua live_grep_glob<CR>', desc = 'Live grep' },
      { '<leader>gb', '<cmd>FzfLua git_branches<CR>', desc = 'List GIT branches' },
    },
    config = function()
      local fzf_lua = require 'fzf-lua'
      fzf_lua.setup {
        winopts = {
          backdrop = 100,
          split = 'belowright new',
          preview = {
            hidden = 'hidden',
          },
        },
        fzf_opts = {
          ['--layout'] = 'default',
        },
      }
      fzf_lua.register_ui_select()
    end,
  },
  {
    'stevearc/dressing.nvim',
    config = function()
      require('dressing').setup {
        select = {
          backend = { 'fzf_lua', 'nui', 'telescope', 'builtin' },
        },
      }
    end,
  },
}
