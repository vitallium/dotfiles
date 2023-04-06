return {
  "nvim-tree/nvim-web-devicons", -- Fancy icons in pop-ups
  lazy = true,
  opts = {
    -- globally enable different highlight colors per icon (default to true)
    -- if set to false all icons will have the default icon's color
    color_icons = true,
    -- globally enable default icons (default to false)
    default = true,
  },
  config = function(_, opts)
    require("nvim-web-devicons").setup(opts)
  end,
}
