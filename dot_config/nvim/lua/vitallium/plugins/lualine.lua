return {
  "nvim-lualine/lualine.nvim", -- Modeline
  event = "VeryLazy",
  config = true,
  opts = {
    options = {
      theme = "auto",
      component_separators = "",
      section_separators = "",
      disabled_filetypes = {
        statusline = {},
        winbar = { "NvimTree", "NeogitCommitMessage", "NeogitStatus", "aerial" },
      },
    },
    sections = {
      lualine_c = {
        "%=",
        {
          "filetype",
          icon_only = true,
          icon = { align = "right" },
        },
        {
          "filename",
          file_status = false,
          path = 1,
        },
      },
    },
    extensions = { "aerial", "lazy", "nvim-tree" },
  },
}
