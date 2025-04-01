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
      require('mason-tool-installer').setup({
        ensure_installed = {
          -- Linters
          'vale',
          'markdownlint',
          'shellcheck',
          'selene',
          'eslint_d',
          'flake8',
          'rubocop',
          'yamllint',
          'hadolint',
        },
        run_on_start = true,
        start_delay = 3000,
        debounce_hours = 5,
      })

      require('lint').linters_by_ft = {
        markdown = {
          require('lint').linters.vale,
          require('lint').linters.markdownlint,
        },
        sh = {
          require('lint').linters.shellcheck,
        },
        lua = {
          require('lint').linters.selene,
        },
        javascript = {
          require('lint').linters.eslint_d,
        },
        typescript = {
          require('lint').linters.eslint_d,
        },
        python = {
          require('lint').linters.flake8,
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
      vim.api.nvim_create_autocmd({ "BufWritePost" }, {
        callback = function()
          require("lint").try_lint()
        end,
      })

      -- Doom Emacs style keymaps
      vim.keymap.set('n', '<space>h d', function()
        vim.diagnostic.open_float()
      end, { desc = '[H]elp [D]iagnostics' })
      
      vim.keymap.set('n', '<space>h n', function()
        vim.diagnostic.goto_next()
      end, { desc = '[H]elp [N]ext error' })
      
      vim.keymap.set('n', '<space>h p', function()
        vim.diagnostic.goto_prev()
      end, { desc = '[H]elp [P]revious error' })

      vim.keymap.set('n', '<space>h l', function()
        vim.diagnostic.setloclist()
      end, { desc = '[H]elp [L]ist errors' })
      
      vim.keymap.set('n', '<space>h c', function()
        vim.diagnostic.clear()
      end, { desc = '[H]elp [C]lear diagnostics' })

      -- Add manual lint trigger
      vim.keymap.set('n', '<space>h r', function()
        require('lint').try_lint()
      end, { desc = '[H]elp [R]un linter' })
    end,
  },
} 