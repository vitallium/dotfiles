return {
  "lukas-reineke/indent-blankline.nvim",
  ft = { "yaml" },
  opts = {
    buftype_exclude = require("ignored").buffer_types,
    filetype = { "yaml" },
    show_current_context = true,
    show_trailing_blankline_indent = true,
    use_treesitter = true,
    use_treesitter_scope = true,
  },
}
