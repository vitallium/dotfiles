return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  keys = {
    { "<leader>od", ":Oil", desc = "Open parent directory" },
  },
  opts = {
    default_file_explorer = false,
  },
}
