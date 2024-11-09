return {
  {
    'nvim-neotest/neotest',
    dependencies = {
      'nvim-neotest/nvim-nio',
      'nvim-lua/plenary.nvim',
      'antoinemadec/FixCursorHold.nvim',
      'nvim-treesitter/nvim-treesitter',
      -- Adapters
      'nvim-neotest/neotest-jest',
      'olimorris/neotest-rspec',
      'zidhuss/neotest-minitest',
    },
    keys = {
      {
        '<leader>ts',
        function()
          require('neotest').summary.toggle()
        end,
        desc = 'Toggle summary',
      },
      {
        '<leader>ta',
        function()
          require('neotest').run.run(vim.fn.getcwd())
        end,
        desc = 'Run tests in project',
      },
      {
        '<leader>td',
        function()
          ---@diagnostic disable-next-line: missing-fields
          require('neotest').run.run { strategy = 'dap' }
        end,
        desc = 'Run nearest test',
      },
      {
        '<leader>tr',
        function()
          require('neotest').run.run(vim.fn.expand '%')
        end,
        desc = 'Run current test file',
      },
      {
        '<leader>tn',
        function()
          require('neotest').run.run()
        end,
        desc = 'Run nearest test',
      },
      {
        '<leader>to',
        function()
          require('neotest').output.open()
        end,
        desc = 'View test output',
      },
      {
        '<leader>tO',
        function()
          require('neotest').output.open { auto_close = true, enter = true }
        end,
        desc = 'View test output',
      },
      {
        '<leader>tw',
        function()
          require('neotest').watch.toggle(vim.fn.getcwd())
        end,
        desc = 'Run tests in project in watch mode',
      },
    },
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require('neotest').setup {
        adapters = {
          require 'neotest-jest' {
            jestCommand = 'npm test --',
            jestConfigFile = 'custom.jest.config.ts',
            env = { CI = true },
            cwd = function(path)
              return vim.fn.getcwd()
            end,
          },
          require 'neotest-rspec' {
            filter_dirs = { 'vendor' },
            rspec_cmd = function(position_type)
              if position_type == 'test' then
                ---No viable alternative exists yet
                ---@diagnostic disable-next-line: deprecated
                return vim.tbl_flatten {
                  'bin/rspec',
                  '--fail-fast',
                }
              else
                ---No viable alternative exists yet
                ---@diagnostic disable-next-line: deprecated
                return vim.tbl_flatten {
                  'bin/rspec',
                }
              end
            end,
          },
          require 'neotest-minitest' {
            test_cmd = function()
              ---No viable alternative exists yet
              ---@diagnostic disable-next-line: deprecated
              return vim.tbl_flatten {
                'bundle',
                'exec',
                'rails',
                'test',
              }
            end,
          },
        },
      }
    end,
  },
}
