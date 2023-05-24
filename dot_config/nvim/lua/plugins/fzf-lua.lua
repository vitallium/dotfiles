return {
  "ibhagwan/fzf-lua",
  lazy = false,
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  keys = {
    { "<leader><leader>", "<cmd>FzfLua files<CR>", desc = "Find file" },
    { "<leader>bb", "<cmd>FzfLua buffers<CR>", desc = "List buffers" },
    {
      "<leader>ff",
      function()
        require("fzf-lua").files({ cwd = vim.fn.expand("%:p:h") })
      end,
      desc = "Find file in current dir",
    },
    { "<leader>'", "<cmd>FzfLua resume<CR>", desc = "Resume last FzfLua command" },
    { "<leader>ht", "<cmd>FzfLua help_tags<CR>", desc = "Help tags" },
    { "<leader>fr", "<cmd>FzfLua oldfiles<CR>", desc = "Recent files" },
    { "<leader>sp", "<cmd>FzfLua live_grep_blob<CR>", desc = "Live grep" },
    { "<leader>gb", "<cmd>FzfLua git_branches<CR>", desc = "List GIT branches" },
  },
  opts = {
    winopts = {
      split = "belowright 15new",
      border = "single",
      preview = {
        default = "bat",
        hidden = "hidden",
        border = "border",
        title = false,
        layout = "horizontal",
        horizontal = "right:50%",
      },
    },
    files = {
      previewer = false,
    },
    buffers = {
      previewer = false,
    },
  },
}
