return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  keys = {
    {
      "<leader>o-",
      function()
        if vim.api.nvim_buf_get_option(vim.api.nvim_win_get_buf(0), "filetype") == "oil" then
          return require("oil").close()
        else
          require("oil").open()
        end
      end,
      mode = "",
      desc = "Open oil",
    },
  },
  opts = {
    default_file_explorer = true,
  },
}
