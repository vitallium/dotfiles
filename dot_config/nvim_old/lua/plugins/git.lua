return {
  {
    'NeogitOrg/neogit',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'sindrets/diffview.nvim',
    },
    keys = {
      { '<leader>gg', '<Esc>:Neogit<CR>', silent = true, desc = 'Neogit' },
    },
    opts = {
      graph_style = 'unicode',
      use_telescope = false,
    },
    config = true,
  },
  { -- Shareable permalinks to git hosts
    'ruifm/gitlinker.nvim',
    keys = {
      { '<leader>gy', '', mode = { 'n', 'v' }, desc = 'Copy sharable git URL' },
    },
    config = true,
  },
}
