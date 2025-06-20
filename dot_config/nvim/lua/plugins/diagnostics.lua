return {
  {
    "mfussenegger/nvim-lint", -- Non LSP code linting
    event = {
      "BufReadPre",
      "BufNewFile",
    },
    opts = {
      events = { "BufWritePost", "BufReadPost", "InsertLeave" },
      linters_by_ft = {
        fish = { "fish" },
        eruby = { "erb_lint" },
        javascript = { "eslint_d" },
        javascriptreact = { "eslint_d" },
        markdown = { "vale", "markdownlint-cli2" },
        ruby = { "rubocop" },
        sh = { "shellcheck" },
        svelte = { "eslint_d" },
        typescript = { "eslint_d" },
      },
    },
    config = function(_, opts)
      local lint = require("lint")

      lint.linters_by_ft = opts.linters_by_ft

      local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

      vim.api.nvim_create_autocmd(opts.events, {
        group = lint_augroup,
        callback = function()
          lint.try_lint()
        end,
      })
    end,
  },
}
