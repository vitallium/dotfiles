return {
  "ibhagwan/fzf-lua",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  keys = {
    { "<leader><leader>", "<cmd>FzfLua files<CR>", desc = "Find file" },
    { "<leader>b", "<cmd>FzfLua buffers<CR>", desc = "List buffers" },
    { "<leader>ff", "<cmd>FzfLua files<CR>" },
    { "<leader>'", "<cmd>FzfLua resume<CR>" },
    { "<leader>h", "<cmd>FzfLua help_tags<CR>" },
    { "<leader>fr", "<cmd>FzfLua oldfiles<CR>" },
    { "<leader>sp", "<cmd>FzfLua live_grep<CR>" },
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
  setup = true,
}
