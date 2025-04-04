return {
  {
    'mfussenegger/nvim-lint',
    dependencies = {
      'williamboman/mason.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',
    },
    config = function()
      -- Configure Mason to install linters
      require('mason').setup()
      require('mason-tool-installer').setup {
        ensure_installed = {
          -- Linters
          'vale',
          'markdownlint-cli2',
          'shellcheck',
          'eslint_d',
          'rubocop',
          'yamllint',
          'hadolint',
        },
        run_on_start = true,
        start_delay = 3000,
        debounce_hours = 5,
      }

      require('lint').linters_by_ft = {
        markdown = {
          'vale',
          'markdownlint-cli2',
        },
        sh = { 'shellcheck' },
        dockerfile = { 'hadolint' },
      }

      -- Lint on save
      vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
        callback = function()
          require('lint').try_lint()
        end,
      })
    end,
  },
}
