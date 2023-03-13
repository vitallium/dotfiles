return {
  -- Plugin with util functions required by other plugins
  { "nvim-lua/plenary.nvim", lazy = true },
  -- EditorConfig
  "gpanders/editorconfig.nvim",
  -- [[ Theming ]]
  {
    "folke/todo-comments.nvim", -- Highlight and list TODOs, etc.
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      highlight = {
        pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlightng (vim regex)
        -- Was [[.*<(KEYWORDS)\s*:]] including colon.
      },
      search = {
        pattern = [[\b(KEYWORDS)]], -- ripgrep regex
        -- Was [[\b(KEYWORDS):]] including colon.
      },
    },
  },
  {
    "norcalli/nvim-colorizer.lua", -- Show color-codes colored
    event = { "BufReadPost", "BufNewFile" },
    config = true,
  },
  {
    "danilamihailov/beacon.nvim", -- Highlight cursor on jump
    event = { "BufReadPost", "BufNewFile" },
  },

  -- [[ Interface ]]
  {
    -- Switch windows/panes vim/tmux
    "christoomey/vim-tmux-navigator",
    event = { "VeryLazy" },
  },
  {
    -- Like Emacs which key
    "folke/which-key.nvim",
    event = { "VeryLazy" },
    config = true,
  },
  {
    -- Alt-h/j/k/l to move line
    "booperlv/nvim-gomove",
    event = { "BufReadPost", "BufNewFile" },
    config = true,
  },
  {
    -- Keep windows around when deleting buffers
    "famiu/bufdelete.nvim",
    cmd = "Bdelete",
    keys = {
      { "<leader>bd", ":Bdelete<CR>", desc = "Delete buffer" },
    },
  },
  {
    -- Auto-pair tags, etc.
    "windwp/nvim-autopairs",
    event = { "BufReadPost", "BufNewFile" },
    config = true,
  },
}
