return {
  {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<leader><leader>", "<cmd>FzfLua files<CR>", desc = "Find file" },
      { "<leader>pf", "<cmd>FzfLua files<CR>", desc = "Find file" },
      { "<leader>bb", "<cmd>FzfLua buffers<CR>", desc = "List buffers" },
      { "<leader>,", "<cmd>FzfLua buffers<CR>", desc = "List buffers" },
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
      { "<leader>fr", "<cmd>FzfLua oldfiles<CR>", desc = "Recent files" },
      { "<leader>sp", "<cmd>FzfLua live_grep_glob<CR>", desc = "Live grep" },
    },
    config = function()
      local fzf_lua = require("fzf-lua")
      fzf_lua.setup({
        fzf_colors = {
          ["fg"] = { "fg", "CursorLine" },
          ["bg"] = { "bg", "Normal" },
          ["hl"] = { "fg", "Comment" },
          ["fg+"] = { "fg", "Normal" },
          ["bg+"] = { "bg", "CursorLine" },
          ["hl+"] = { "fg", "Statement" },
          ["info"] = { "fg", "PreProc" },
          ["prompt"] = { "fg", "Conditional" },
          ["pointer"] = { "fg", "Exception" },
          ["marker"] = { "fg", "Keyword" },
          ["spinner"] = { "fg", "Label" },
          ["header"] = { "fg", "Comment" },
          ["gutter"] = "-1",
        },
        winopts = {
          backdrop = 100,
          split = "belowright new",
          preview = {
            hidden = "hidden",
          },
        },
        fzf_opts = {
          ["--layout"] = "default",
        },
      })
      fzf_lua.register_ui_select()
    end,
  },
  {
    "stevearc/oil.nvim",
    ---@module 'oil'
    ---@type oil.SetupOpts
    opts = {},
    dependencies = { "echasnovski/mini.icons" },
    -- Lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
    lazy = false,
    keys = {
      { "-", "<cmd>Oil<cr>", desc = "Open parent directory" },
    },
  },
}
