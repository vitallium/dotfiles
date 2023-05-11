local wk = require("which-key")

-- Normal mode
wk.register({
  g = {
    name = "Git",
    b = { ":Gitsigns toggle_current_line_blame<CR>", "Toggle blame" },
    g = { ":Neogit<CR>", "Neogit" },
    G = { ":LazyGit<CR>", "LazyGit" },
    d = { ":DiffviewOpen<CR>", "Diff view" },
    f = { ":DiffviewFileHistory -f %<CR>", "File history" },
    h = {
      name = "Highlighting",
      b = { ":Gitsigns toggle_current_line_blame<CR>", "Toggle blame" },
      l = { ":Gitsigns toggle_linehl<CR>", "Toggle line highlighting" },
      n = { ":Gitsigns toggle_numhl<CR>", "Toggle number highlighting" },
      s = { ":Gitsigns toggle_signs<CR>", "Toggle signs" },
      w = { ":Gitsigns toggle_current_word_diff<CR>", "Toggle word diff" },
    },
    l = { "V:'<,'>DiffviewFileHistory -f<CR>", "Line history" },
  },
}, { prefix = "<leader>" })

-- Visual mode
wk.register({
  g = {
    name = "Git",
    l = { ":'<,'>DiffviewFileHistory -f<CR>", "Line history" },
  },
}, { prefix = "<leader>", mode = "v" })

return {
  {
    "lewis6991/gitsigns.nvim", -- Git gutter
    event = "BufEnter",
    config = function()
      require("gitsigns").setup({
        signs = {
          add = { text = "┃" },
          change = { text = "┃" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
          untracked = { text = "┆" },
        },
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
