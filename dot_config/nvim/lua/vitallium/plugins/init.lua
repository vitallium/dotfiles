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
    lazy = true,
  },
  {
    -- Alt-h/j/k/l to move line
    "booperlv/nvim-gomove",
    event = { "BufReadPost", "BufNewFile" },
    config = true,
  },
  {
    -- Auto-pair tags, etc.
    "windwp/nvim-autopairs",
    event = { "BufReadPost", "BufNewFile" },
    config = true,
  },
  -- Ruby improvement
  {
    "jlcrochet/vim-ruby",
    event = { "BufReadPost", "BufNewFile" },
  },
  -- Dash.app integration
  {
    "mrjones2014/dash.nvim",
    event = { "BufReadPost", "BufNewFile" },
    build = "make install",
  },
  {
    "dstein64/vim-startuptime",
    cmd = "StartupTime",
    init = function()
      vim.g.startuptime_tries = 10
    end,
  },
  {
    "mhanberg/output-panel.nvim",
    event = "VeryLazy",
    config = function()
      require("output_panel").setup()
    end,
  },
}
