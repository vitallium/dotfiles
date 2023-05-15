return {
  "ibhagwan/fzf-lua",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  keys = {
    { "<leader><leader>", "<cmd>FzfLua files<CR>", desc = "Find file" },
    { "<leader>b", "<cmd>FzfLua buffers<CR>", desc = "List buffers" },
    {
      "<leader>ff",
      function()
        require("fzf-lua").files({ cwd = vim.fn.expand("%:p:h") })
      end,
      desc = "Find file in current dir",
    },
    { "<leader>'", "<cmd>FzfLua resume<CR>", desc = "Resume last FzfLua command" },
    { "<leader>h", "<cmd>FzfLua help_tags<CR>" },
    { "<leader>fr", "<cmd>FzfLua oldfiles<CR>" },
    { "<leader>sp", "<cmd>FzfLua live_grep<CR>", desc = "Live grep" },
    { "<leader>gb", "<cmd>FzfLua git_branches<CR>" },
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
  config = true,
}
