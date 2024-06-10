return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      { "williamboman/mason.nvim", config = true },
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { "j-hui/fidget.nvim", opts = {} },
      -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
      -- used for completion, annotations and signatures of Neovim apis
      { "folke/neodev.nvim", opts = {} },
      -- Use "bundler" for Ruby stuff
      "mihyaeru21/nvim-lspconfig-bundler",
    },
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("lspconfig.ui.windows").default_options.border = vim.g.border

      vim.lsp.set_log_level(vim.log.levels.ERROR)

      require("vim.lsp.log").set_format_func(vim.inspect)

      vim.keymap.set("n", "<leader>li", vim.cmd.LspInfo, { desc = "LSP Info" })
      vim.keymap.set("n", "<leader>ll", vim.cmd.LspLog, { desc = "LSP Log" })
      vim.keymap.set("n", "<leader>lr", vim.cmd.LspRestart, { desc = "LSP Restart" })

      local lsp_servers = require("plugins.lsp.servers")
      lsp_servers.setup()
    end,
  },
  {
    "aznhe21/actions-preview.nvim",
    dependencies = { "MunifTanjim/nui.nvim" },
    config = function()
      require("actions-preview").setup({
        diff = {
          algorithm = "patience",
          ignore_whitespace = true,
        },
      })
    end,
  },
  {
    "SmiteshP/nvim-navic",
    event = "LspAttach",
    config = function()
      vim.g.navic_silence = true

      require("nvim-navic").setup({
        highlight = true,
      })
    end,
  },
  { "b0o/schemastore.nvim", version = false },
  {
    "crispgm/nvim-go",
    cmd = {
      "GoFormat",
      "GoGet",
      "GoInstall",
      "GoLint",
    },
    event = "VeryLazy",
  },
  { "ray-x/lsp_signature.nvim" },
  { "VidocqH/lsp-lens.nvim", config = true },
  { "smjonas/inc-rename.nvim", config = true },
}