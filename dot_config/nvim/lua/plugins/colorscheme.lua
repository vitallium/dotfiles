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
}
