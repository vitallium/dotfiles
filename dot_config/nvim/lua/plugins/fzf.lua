return {
  {
    'ibhagwan/fzf-lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      vim.api.nvim_set_keymap('n', '<C-\\>', [[<Cmd>lua require"fzf-lua".buffers()<CR>]], {})
      vim.api.nvim_set_keymap('n', '<C-k>', [[<Cmd>lua require"fzf-lua".builtin()<CR>]], {})
      vim.api.nvim_set_keymap('n', '<C-p>', [[<Cmd>lua require"fzf-lua".files()<CR>]], {})
      vim.api.nvim_set_keymap('n', '<C-l>', [[<Cmd>lua require"fzf-lua".live_grep_glob()<CR>]], {})
      vim.api.nvim_set_keymap('n', '<C-g>', [[<Cmd>lua require"fzf-lua".grep_project()<CR>]], {})
      vim.api.nvim_set_keymap('n', '<F1>', [[<Cmd>lua require"fzf-lua".help_tags()<CR>]], {})

      local fzf_lua = require 'fzf-lua'
      fzf_lua.setup {
        winopts = {
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
}
