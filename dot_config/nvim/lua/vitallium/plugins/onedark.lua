return {
  "navarasu/onedark.nvim",
  priority = 1000,
  opts = {
    style = "light",
  },
  config = function(_, opts)
    vim.opt.background = "light"
    require("onedark").load(opts)
  end
}
