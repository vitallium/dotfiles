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
          require('lint').linters.vale,
          require('lint').linters.markdownlint,
        },
        sh = {
          require('lint').linters.shellcheck,
        },
        javascript = {
          require('lint').linters.eslint_d,
        },
        typescript = {
          require('lint').linters.eslint_d,
        },
        ruby = {
          require('lint').linters.rubocop,
        },
        yaml = {
          require('lint').linters.yamllint,
        },
        dockerfile = {
          require('lint').linters.hadolint,
        },
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
