return {
  "nvim-lualine/lualine.nvim", -- Modeline
  config = function()
    local branch_max_length = 30
    local function get_branch()
      require("lualine.components.branch.git_branch").init()
      local branch = require("lualine.components.branch.git_branch").get_branch()
      return string.sub(branch, math.max(string.len(branch) - branch_max_length, 0), string.len(branch))
    end

    require("lualine").setup({
      options = {
        theme = "auto",
        disabled_filetypes = {
          statusline = {},
          winbar = { "NvimTree", "NeogitCommitMessage", "NeogitStatus" },
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
        lualine_z = { "location", "progress" },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = { "mode", "searchcount", get_branch, "diff" },
        lualine_c = { { "filename", path = 1, shorting_target = 70 } },
        lualine_x = { { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
        lualine_y = { "filetype", "locally", "progress" },
        lualine_z = {},
      },
      tabline = {},
      extensions = { "lazy", "nvim-tree" },
    })
  end,
}
