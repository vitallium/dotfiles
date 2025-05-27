return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "sindrets/diffview.nvim",
    },
    keys = {
      { "<leader>gg", "<Esc>:Neogit<CR>", silent = true, desc = "Neogit" },
    },
    opts = {
      graph_style = "unicode",
      use_telescope = false,
    },
    config = true,
  },
  {
    "ruifm/gitlinker.nvim",
    keys = {
      { "<leader>gy", "", mode = { "n", "v" }, desc = "Copy sharable git URL" },
    },
    config = true,
  },
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
    },
  },
}
