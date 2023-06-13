return {
  {
    "folke/tokyonight.nvim",
    priority = 1000,
    opts = {
      style = "night",
      on_colors = function(colors)
        colors.border = colors.cyan
      end,
    },
    config = function(_, opts)
      local tokyonight = require("tokyonight")
      tokyonight.setup(opts)
      tokyonight.load()
    end,
  },
}
