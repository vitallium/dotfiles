return {
  "nvim-lualine/lualine.nvim", -- Modeline
  name = "lualine",
  config = function()
    local branch_max_length = 30
    local function get_branch()
      require("lualine.components.branch.git_branch").init()
      local branch = require("lualine.components.branch.git_branch").get_branch()
      return string.sub(branch, math.max(string.len(branch) - branch_max_length, 0), string.len(branch))
    end

    require("lualine").setup({
      options = {
        icons_enabled = true,
        theme = "tokyonight",
        component_separators = "",
        section_separators = "",
        disabled_filetypes = {
          statusline = {},
          winbar = { "NvimTree", "NeogitCommitMessage", "NeogitStatus", "aerial" },
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = true,
        refresh = { statusline = 1000, tabline = 1000, winbar = 1000 },
      },
      sections = {
        lualine_a = { "mode", "searchcount" },
        lualine_b = { get_branch, "diff" },
        lualine_c = { { "filename", path = 1, shorting_target = 70 } },
        lualine_x = { { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
        lualine_y = { "filetype" },
        lualine_z = { "location", "progress", "tabnine" },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = { "mode", "searchcount", get_branch, "diff" },
        lualine_c = { { "filename", path = 1, shorting_target = 70 } },
        lualine_x = { { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
        lualine_y = { "filetype", "locally", "progress", "cmp_tabnine" },
        lualine_z = {},
      },
      tabline = {},
      winbar = {
        lualine_a = {},
        lualine_b = { { "filetype", icon_only = true }, "filename" },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
      inactive_winbar = {
        lualine_a = {},
        lualine_b = { { "filetype", icon_only = true }, "filename" },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
      extensions = { "aerial", "nvim-tree" },
    })
  end,
}
