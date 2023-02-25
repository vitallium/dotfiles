return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  config = function()
    require("catppuccin").setup({
      integrations = {
        aerial = true,
        cmp = true,
        dashboard = true,
        fidget = true,
        gitsigns = true,
        illuminate = true,
        indent_blankline = {
          enabled = true,
          -- To enable to following, check the documentation:
          -- https://github.com/catppuccin/nvim#special-integrations
          colored_indent_levels = false,
        },
        lsp_trouble = true,
        markdown = true,
        mason = true,
        native_lsp = {
          -- To see more options, check the documentation:
          -- https://github.com/catppuccin/nvim#special-integrations
          enabled = true,
        },
        navic = {
          enabled = true,
          custom_bg = "NONE",
        },
        neogit = true,
        notify = true,
        nvimtree = true,
        telescope = true,
        treesitter = true,
        treesitter_context = true,
        ts_rainbow = true,
        which_key = true,
      },
    })
    vim.cmd.colorscheme("catppuccin-latte")
  end,
}
