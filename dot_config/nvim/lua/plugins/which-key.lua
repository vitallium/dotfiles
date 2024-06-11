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
          windows = false,
          nav = true,
        },
        triggers = 'auto', -- automatically setup triggers
      }

      -- Document existing key chains
      which_key.register({
        c = { name = 'Code' },
        b = { name = 'Buffer' },
        d = { name = 'Document' },
        -- r = { name = 'Rename' },
        s = { name = 'Search' },
        -- w = { name = 'Workspace', _ = 'which_key_ignore' },
        t = {
          name = 'Toggle',
          c = { ':set nolist!<CR>', 'Toggle invisible chars' },
          w = { ':set wrap!<CR>', 'Toggle word wrap' },
        },
        g = { name = 'Git' },
        w = {
          name = 'Window',
          c = { ':close<CR>', 'Close window' },
          d = { ':close<CR>', 'Close window' },
          s = { ':split<CR>', 'Split window' },
          v = { ':vsplit<CR>', 'Vertically split window' },
          h = { ':wincmd h<CR>', 'Go to window to the left' },
          j = { ':wincmd j<CR>', 'Go to window to the bottom' },
          k = { ':wincmd k<CR>', 'Go to window to the top' },
          l = { ':wincmd l<CR>', 'Go to window to the right' },
        },
      }, { prefix = '<leader>' })

      -- With local prefix
      which_key.register({
        t = {
          name = 'Test',
        },
      }, { prefix = '<localleader>' })
    end,
  },
}
