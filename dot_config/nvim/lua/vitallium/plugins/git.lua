local wk = require("which-key")

-- Normal mode
wk.register({
  g = {
    name = "Git",
    t = { ":Gitsigns toggle_current_line_blame<CR>", "Toggle blame" },
    g = { ":Neogit<CR>", "Neogit" },
    G = { ":LazyGit<CR>", "LazyGit" },
    b = {
      function() require("telescope.builtin").git_branches() end,
      "Branches",
    },
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
    keys = {
      { "<leader>gs", vim.cmd.Git, noremap = true, silent = true, desc = "Open Git" },
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
    "TimUntersberger/neogit", -- Think magit
    dependencies = { "sindrets/diffview.nvim" },
    cmd = "Neogit",
    config = function()
      require("neogit").setup({
        disable_builtin_notifications = true,
        disable_commit_confirmation = true,
        disable_insert_on_commit = false,
        integrations = {
          diffview = true,
        },
        mappings = {
          status = {
            ["zM"] = "Depth1",
            ["zR"] = "Depth4",
          },
        },
      })
    end,
  },
  {
    "ruifm/gitlinker.nvim", -- Shareable permalinks to git hosts
    keys = {
      { "<leader>gy", "", mode = { "n", "v" }, desc = "copy sharable git URL" },
    },
    config = true,
  },
  -- Git client in your neovim
  { "kdheepak/lazygit.nvim", cmd = "LazyGit" },
}
