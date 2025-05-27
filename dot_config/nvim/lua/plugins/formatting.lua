return {
  {
    "stevearc/conform.nvim", -- Non LSP code formatting
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    keys = {
      {
        "<leader>bf",
        function()
          require("conform").format({ async = true, lsp_format = "last" })
        end,
        mode = "",
        desc = "[F]ormat Current Buffer",
      },
    },
    init = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    end,
    ---@module "conform"
    ---@type conform.setupOpts
    opts = {
      notify_on_error = false,
      formatters = {
        injected = {
          options = { ignore_errors = true },
        },
      },
      -- Set up format-on-save
      format_on_save = { timeout_ms = 500 },
      formatters_by_ft = {
        css = { "prettier" },
        fish = { "fish_indent" },
        html = { "prettier" },
        javascript = { "prettierd", "prettier", stop_after_first = true },
        json = { "prettier" },
        lua = { "stylua" },
        markdown = { "prettier", "injected" },
        scss = { "prettier" },
        sh = { "shfmt" },
        typescript = { "prettier" },
        xml = { "prettier" },
        yaml = { "prettier" },
        ["*"] = { "injected" },
        -- Use the "_" filetype to run formatters on filetypes that don't
        -- have other formatters configured.
        ["_"] = { "trim_whitespace" },
      },
    },
  },
}
