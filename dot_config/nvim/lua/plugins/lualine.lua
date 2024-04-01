return {
  "nvim-lualine/lualine.nvim", -- Modeline
  event = "VeryLazy",
  config = function()
    -- Performance: We don't need this lualine require madness.
    local lualine_require = require("lualine_require")
    lualine_require.require = require

    require("lualine").setup({
      options = {
        theme = "auto",
        section_separators = { left = "", right = "" },
        disabled_filetypes = {
          statusline = {},
          winbar = { "NvimTree", "NeogitCommitMessage", "NeogitStatus", "oil" },
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = true,
        refresh = { statusline = 1000, tabline = 1000, winbar = 1000 },
      },
      sections = {
        lualine_a = { "mode", "searchcount" },
        lualine_b = { "diff" },
        lualine_c = { { "filename", path = 1, shorting_target = 70 } },
        lualine_x = { { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
        lualine_y = { "filetype" },
        lualine_z = { "location", "progress" },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = { "mode", "searchcount", "diff" },
        lualine_c = { { "filename", path = 1, shorting_target = 70 } },
        lualine_x = { { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
        lualine_y = { "filetype", "locally", "progress" },
        lualine_z = {},
      },
      tabline = {},
      extensions = { "fugitive", "quickfix", "lazy", "nvim-tree" },
    })
  end,
}
