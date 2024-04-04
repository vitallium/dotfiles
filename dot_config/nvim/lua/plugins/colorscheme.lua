return {
  {
    "folke/tokyonight.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      style = "night",
      on_colors = function(colors)
        colors.border = colors.cyan
      end,
    },
    config = function(_, opts)
      require("tokyonight").setup(opts)
    end,
  },
  {
    "miikanissi/modus-themes.nvim",
    priority = 1000,
    lazy = false,
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    lazy = false,
    opts = {
      flavour = "mocha",
      integrations = {
        cmp = true,
        dap = {
          enabled = true,
          enable_ui = true,
        },
        gitsigns = true,
        indent_blankline = {
          enabled = true,
          colored_indent_levels = true,
        },
        markdown = true,
        native_lsp = {
          enabled = true,
        },
        neogit = true,
        neotest = false,
        notify = true,
        telescope = false,
        harpoon = false,
        treesitter = true,
        treesitter_context = true,
      },
    },
  },
}
