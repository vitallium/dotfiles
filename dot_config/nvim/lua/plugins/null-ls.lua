return {
  "jose-elias-alvarez/null-ls.nvim", -- NeoVim as LSP server
  event = "VeryLazy",
  config = function()
    local handlers = require("plugins.lsp.handlers")

    local null_ls = require("null-ls")
    local null_ls_sources = {
      -- code actions
      null_ls.builtins.code_actions.gitrebase,

      -- diagnostics
      null_ls.builtins.diagnostics.gitlint.with({
        filetypes = { "gitcommit", "NeogitCommitMessage" },
      }),
      null_ls.builtins.diagnostics.hadolint, -- Docker best practices
      null_ls.builtins.diagnostics.markdownlint,
      null_ls.builtins.diagnostics.shellcheck,
      null_ls.builtins.diagnostics.stylelint,
      -- Ruby
      null_ls.builtins.diagnostics.haml_lint.with({
        command = "bundle",
        args = vim.list_extend({ "exec", "haml-lint" }, null_ls.builtins.diagnostics.haml_lint._opts.args),
      }),
      null_ls.builtins.diagnostics.rubocop.with({
        command = "bundle",
        args = vim.list_extend({ "exec", "rubocop" }, null_ls.builtins.diagnostics.rubocop._opts.args),
      }),
      null_ls.builtins.diagnostics.reek,
      null_ls.builtins.diagnostics.yamllint,

      -- formatting
      null_ls.builtins.formatting.stylua,
      null_ls.builtins.formatting.gofumpt,
      null_ls.builtins.formatting.goimports,
      null_ls.builtins.formatting.golines,
      null_ls.builtins.formatting.fish_indent,
    }

    -- NeoVim LSP server capabilities
    null_ls.setup({
      debug = false,
      sources = null_ls_sources,
      on_attach = handlers.on_attach,
      update_in_insert = false,
      -- Let the LSP client set the root directory.
      root_dir = require("null-ls.utils").root_pattern(),
    })
  end,
}
