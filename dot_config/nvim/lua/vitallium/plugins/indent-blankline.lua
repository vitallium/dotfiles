return {
  "lukas-reineke/indent-blankline.nvim",
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    char_list = { "│", "¦", "┆", "┊" },
    filetype_exclude = { "help", "alpha", "Trouble", "lazy" },
    show_trailing_blankline_indent = false,
    show_current_context = true,
    show_current_context_start = false, -- If true, underline context start (with treesitter)
    show_end_of_line = false, -- On empty lines
    space_char_blankline = " ",
  },
}
