local wk = require("which-key")

-- Normal mode
wk.register({
  g = {
    name = "Git",
    b = {
      function()
        require("telescope.builtin").git_branches()
      end,
      "Branches",
    },
    d = { ":DiffviewOpen<CR>", "Diff view" },
    f = { ":DiffviewFileHistory -f %<CR>", "File history" },
    g = { ":Neogit<CR>", "Neogit" },
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
      -- This is for diagnostic signs on the line number column.
      -- Use this to beautify the plain E W signs.
      local signs = require("vitallium.icons").diagnostics
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
      end
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
    dependencies = { "nvim-tree/nvim-web-devicons", "nvim-lua/plenary.nvim" },
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    config = function()
      local icons = require("vitallium.icons")
      local actions = require("diffview.actions")

      require("diffview").setup({
        icons = {
          -- Only applies when use_icons is true.
          folder_closed = icons.Folder,
          folder_open = icons.FolderOpen,
        },
        signs = {
          fold_closed = icons.ui.ChevronShortRight,
          fold_open = icons.ui.ChevronShortDown,
          done = icons.BoxChecked,
        },
        keymaps = {
          view = {
            { "n", "<leader>ft", actions.toggle_files },
          },
          file_panel = {
            { "n", "j",          actions.select_next_entry },
            { "n", "k",          actions.select_prev_entry },
            { "n", "q",          ":tabclose<CR>" },
            { "n", "<cr>",       "<c-w>k" },
            { "n", "o",          actions.close },
            { "n", "<leader>ft", actions.toggle_files },
          },
          file_history_panel = {
            { "n", "j",          actions.select_next_entry },
            { "n", "k",          actions.select_prev_entry },
            { "n", "q",          ":tabclose<CR>" },
            { "n", "<cr>",       "<c-w>k" },
            { "n", "o",          actions.close },
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
      local icons = require("vitallium.icons")

      require("neogit").setup({
        disable_builtin_notifications = true,
        disable_commit_confirmation = true,
        disable_insert_on_commit = false,
        integrations = {
          diffview = true,
        },
        signs = {
          section = { icons.ui.ChevronShortRight, icons.ui.ChevronShortDown },
          item = { icons.ui.ChevronShortRight, icons.ui.ChevronShortDown },
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
    "pwntester/octo.nvim", -- GitHub inside Neovim
    cmd = "Octo",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "kyazdani42/nvim-web-devicons",
    },
    config = function()
      require("octo").setup()
    end,
  },
}
