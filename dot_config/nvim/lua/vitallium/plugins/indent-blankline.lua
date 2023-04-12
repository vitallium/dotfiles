return {
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      char = "│",
      filetype_exclude = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy" },
      show_trailing_blankline_indent = false,
      show_current_context = false,
    },
    {
      "echasnovski/mini.indentscope",
      version = false, -- wait till new 0.7.0 release to put it back on semver
      event = "BufReadPre",
      config = function()
        local indent = require("mini.indentscope")
        indent.setup({
          symbol = "│",
          draw = {
            animation = indent.gen_animation.none(),
          },
          options = { try_as_border = true },
        })
      end,
    },
  },
}
