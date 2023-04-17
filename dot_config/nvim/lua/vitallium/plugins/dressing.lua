return {
  -- dressing.nvim (https://github.com/stevearc/dressing.nvim)
  --  Neovim plugin to improve the default vim.ui interfaces
  "stevearc/dressing.nvim",
  event = "VeryLazy",
  init = function()
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.ui.select = function(...)
      require("lazy").load({ plugins = { "dressing.nvim" } })
      return vim.ui.select(...)
    end
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.ui.input = function(...)
      require("lazy").load({ plugins = { "dressing.nvim" } })
      return vim.ui.input(...)
    end
  end,
}
