return {
  {
    "lewis6991/gitsigns.nvim", -- Git gutter
    event = "VeryLazy",
    opts = {
      on_attach = function(buffer)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, desc)
          vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
        end

        -- stylua: ignore start
        map("n", "]h", gs.next_hunk, "Next Hunk")
        map("n", "[h", gs.prev_hunk, "Prev Hunk")
        map({ "n", "v" }, "<leader>ghs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
        map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
        map("n", "<leader>ghS", gs.stage_buffer, "Stage Buffer")
        map("n", "<leader>ghu", gs.undo_stage_hunk, "Undo Stage Hunk")
        map("n", "<leader>ghR", gs.reset_buffer, "Reset Buffer")
        map("n", "<leader>ghp", gs.preview_hunk, "Preview Hunk")
        map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame Line")
        map("n", "<leader>ghd", gs.diffthis, "Diff This")
        map("n", "<leader>ghD", function() gs.diffthis("~") end, "Diff This ~")
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
      end,
      preview_config = {
        border = vim.g.border,
      },
      trouble = true,
      current_line_blame = false,
    },
  },
  {
    "tpope/vim-fugitive", -- For :Git
    cmd = { "Git", "Gedit", "Gdiffsplit", "Gvdiffsplit" },
    event = "VeryLazy",
    keys = {
      { "<leader>gs", "<Esc>:Git<CR>", silent = true, desc = "Open Git" },
      { "<leader>gb", "<Esc>:Git blame<CR>", silent = true, desc = "Blame" },
    },
  },
  {
    "sindrets/diffview.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = "VeryLazy",
    config = function()
      local actions = require("diffview.actions")

      require("diffview").setup({
        use_icons = false,
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
  {
    "NeogitOrg/neogit",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Neogit",
    keys = {
      { "<leader>gg", "<Esc>:Neogit<CR>", silent = true, desc = "Open Git" },
    },
    opts = {
      use_telescope = false,
      disable_commit_confirmation = true,
      disable_builtin_notifications = true,
      integrations = {
        diffview = true,
      },
    },
    config = function()
      require("neogit").setup({})
    end,
  },
}
