return {
  -- Prettier for TS/JS formatting (for null-ls)
  "MunifTanjim/prettier.nvim",
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    bin = "prettierd", -- or `"prettier"`
    filetypes = {
      "css",
      "graphql",
      "html",
      "javascript",
      "javascriptreact",
      "json",
      "less",
      "markdown",
      "scss",
      "typescript",
      "typescriptreact",
      "vue",
      "yaml",
    },
  },
}
