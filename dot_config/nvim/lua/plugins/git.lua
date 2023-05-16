return {
  {
    "lewis6991/gitsigns.nvim", -- Git gutter
    event = { "BufReadPost", "BufNewFile" },

    config = function()
      require("gitsigns").setup({
        current_line_blame = false,
      })
    end,
  },
  {
    "tpope/vim-fugitive", -- For :Git
    cmd = { "Git", "Gedit", "Gdiffsplit", "Gvdiffsplit" },
    event = "VeryLazy",
    keys = {
      { "<leader>gs", "<Esc>:Git<CR>", silent = true, desc = "Open Git" },
    },
  },
  {
    "sindrets/diffview.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    config = function()
      local actions = require("diffview.actions")

      require("diffview").setup({
        keymaps = {
          view = {
            { "n", "<leader>ft", actions.toggle_files },
          },
          file_panel = {
            { "n", "j", actions.select_next_entry },
            { "n", "k", actions.select_prev_entry },
            { "n", "q", ":tabclose<CR>" },
            { "n", "<cr>", "<c-w>k" },
            { "n", "o", actions.close },
            { "n", "<leader>ft", actions.toggle_files },
          },
          file_history_panel = {
            { "n", "j", actions.select_next_entry },
            { "n", "k", actions.select_prev_entry },
            { "n", "q", ":tabclose<CR>" },
            { "n", "<cr>", "<c-w>k" },
            { "n", "o", actions.close },
            { "n", "<leader>ft", actions.toggle_files },
          },
        },
      })
    end,
  },
  {
    "ruifm/gitlinker.nvim", -- Shareable permalinks to git hosts
    keys = {
      { "<leader>gy", "", mode = { "n", "v" }, desc = "Copy sharable git URL" },
    },
    config = true,
  },
  -- Git client in your neovim
  { "kdheepak/lazygit.nvim", cmd = "LazyGit" },
}
