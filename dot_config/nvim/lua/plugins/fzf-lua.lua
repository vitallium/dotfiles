return {
  "ibhagwan/fzf-lua",
  lazy = false,
  keys = {
    { "<leader><leader>", "<cmd>FzfLua files<CR>", desc = "Find file" },
    { "<leader>bb", "<cmd>FzfLua buffers<CR>", desc = "List buffers" },
    { "<leader>br", "<cmd>e!<CR>", desc = "Reload buffer" },
    { "<leader>bp", "<cmd>bprevious<CR>", desc = "Previous buffer" },
    { "<leader>bn", "<cmd>bnext<CR>", desc = "Next buffer" },
    {
      "<leader>ff",
      function()
        require("fzf-lua").files({ cwd = vim.fn.expand("%:p:h") })
      end,
      desc = "Find file in current dir",
    },
    { "<leader>'", "<cmd>FzfLua resume<CR>", desc = "Resume last FzfLua command" },
    { "<leader>ht", "<cmd>FzfLua colorschemes<CR>", desc = "Change colorscheme" },
    { "<leader>fr", "<cmd>FzfLua oldfiles<CR>", desc = "Recent files" },
    { "<leader>sp", "<cmd>FzfLua live_grep_glob<CR>", desc = "Live grep" },
    { "<leader>gb", "<cmd>FzfLua git_branches<CR>", desc = "List GIT branches" },
  },
  opts = {
    winopts = {
      hl = {
        border = "FloatBorder",
      },
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
      fzf_opts = {
        ["--ansi"] = false,
        ["--layout"] = "reverse-list",
        ["--info"] = "default",
      },
    },
    manpages = { previewer = "man_native" },
    helptags = { previewer = "help_native" },
    tags = { previewer = "bat" },
    btags = { previewer = "bat" },
    global_git_icons = false,
    global_file_icons = false,
  },
}
