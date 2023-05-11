return {
  {
    "bluz71/vim-nightfly-guicolors",
    lazy = false,
    priority = 1000,
  },
  {
    "dstein64/vim-startuptime",
    cmd = "StartupTime",
  },
  -- Plugin with util functions required by other plugins
  { "nvim-lua/plenary.nvim", lazy = true },
  {
    "direnv/direnv.vim",
    event = "BufRead",
  },
  {
    "nvchad/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
    cmd = { "ColorizerAttachToBuffer", "ColorizerDetachFromBuffer" },
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
        pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlightng (vim regex)
        keyword = "bg",
      },
      search = {
        pattern = [[\b(KEYWORDS)]], -- ripgrep regex
        -- Was [[\b(KEYWORDS):]] including colon.
      },
    },
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
  {
    "mhanberg/output-panel.nvim",
    event = "VeryLazy",
    config = function()
      require("output_panel").setup()
    end,
  },
}
