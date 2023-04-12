return {
  -- Plugin with util functions required by other plugins
  { "nvim-lua/plenary.nvim", lazy = true },
  {
    "direnv/direnv.vim",
    event = "BufRead",
  },
  {
    "johmsalas/text-case.nvim",
    event = "BufRead",
    opts = {},
  },
  {
    "gpanders/editorconfig.nvim",
    event = "BufRead",
  },
  -- [[ Theming ]]
  {
    "folke/todo-comments.nvim", -- Highlight and list TODOs, etc.
    event = "BufReadPost",
    opts = {
      highlight = {
        keyword = "bg",
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
