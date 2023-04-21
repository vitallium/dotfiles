return {
  -- Prettier for TS/JS formatting (for null-ls)
  "MunifTanjim/prettier.nvim",
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    bin = "prettierd", -- or `"prettier"`
    filetypes = {
      "html",
      "javascript",
      "javascriptreact",
      "typescript",
      "typescriptreact",
      "vue",
    },
  },
}
