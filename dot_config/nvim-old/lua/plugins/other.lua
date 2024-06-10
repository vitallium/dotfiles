return {
  "rgroli/other.nvim",
  opts = {
    mappings = {
      "rails",
      "golang",
    },
  },
  config = function(opts)
    require("other-nvim").setup(opts)
  end
}
