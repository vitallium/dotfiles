return {
  {
    'folke/which-key.nvim',
    event = 'VimEnter',
    config = function()
      local which_key = require 'which-key'

      which_key.setup {
        plugins = {
          marks = true, -- shows a list of your marks on ' and `
        },
        presets = {
          windows = true,
          nav = true,
        },
      }

      -- Document existing key chains
      which_key.add {
        { '<leader>f', group = 'File' },
        { '<leader>c', group = 'Code' },
        { '<leader>b', group = 'Buffer' },
        { '<leader>d', group = 'Document' },
        { '<leader>s', group = 'Search' },
        -- Toggle
        { '<leader>t', group = 'Toggle' },
        { '<leader>tc', ':set nolist!<CR>', desc = 'Toggle invisible chars' },
        { '<leader>tw', ':set wrap!<CR>', desc = 'Toggle word wrap' },
        { '<leader>g', group = 'Git' },
        -- Window
        { '<leader>w', group = 'Window' },
        { '<leader>wc', ':close<CR>', desc = 'Close window' },
        { '<leader>ws', ':split<CR>', desc = 'Split window' },
        { '<leader>wv', ':vsplit<CR>', desc = 'Vertically split window' },
        { '<leader>wh', ':wincmd h<CR>', desc = 'Go to window to the left' },
        { '<leader>wj', ':wincmd j<CR>', desc = 'Go to window to the bottom' },
        { '<leader>wk', ':wincmd k<CR>', desc = 'Go to window to the top' },
        { '<leader>wl', ':wincmd l<CR>', desc = 'Go to window to the right' },

        -- { '<localleader>t', group = 'Test' },
      }
    end,
  },
}
