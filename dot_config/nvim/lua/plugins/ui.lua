return {
  {
    "miikanissi/modus-themes.nvim", -- Modus themes
    priority = 1000,
    config = function()
      require("modus-themes").setup({
        hide_inactive_statusline = true,
        line_nr_column_background = false,
        sign_column_background = false,
      })
      vim.cmd.colorscheme("modus")
    end,
  },
  {
    "nvim-lualine/lualine.nvim", -- Fancier statusline
    opts = {
      options = {
        icons_enabled = true,
        theme = "auto",
        component_separators = "|",
        section_separators = "",
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = {},
        lualine_c = { "filename", "diff", "diagnostics" },
        lualine_x = { "filesize", "encoding", "fileformat", "filetype" },
        lualine_y = {},
      },
    },
  },
  {
    "folke/which-key.nvim",
    config = function()
      local which_key = require("which-key")

      which_key.setup({
        plugins = {
          marks = true, -- shows a list of your marks on ' and `
        },
        presets = {
          windows = true,
          nav = true,
        },
      })

      -- Document existing key chains
      which_key.add({
        { "<leader>f", group = "File" },
        { "<leader>c", group = "Code" },
        { "<leader>b", group = "Buffer" },
        { "<leader>d", group = "Document" },
        { "<leader>s", group = "Search" },
        -- Toggle
        { "<leader>t", group = "Toggle" },
        { "<leader>tc", ":set nolist!<CR>", desc = "Toggle invisible chars" },
        { "<leader>tw", ":set wrap!<CR>", desc = "Toggle word wrap" },
        { "<leader>g", group = "Git" },
        -- Window
        { "<leader>w", group = "Window" },
        { "<leader>wc", ":close<CR>", desc = "Close window" },
        { "<leader>ws", ":split<CR>", desc = "Split window" },
        { "<leader>wv", ":vsplit<CR>", desc = "Vertically split window" },
        { "<leader>wh", ":wincmd h<CR>", desc = "Go to window to the left" },
        { "<leader>wj", ":wincmd j<CR>", desc = "Go to window to the bottom" },
        { "<leader>wk", ":wincmd k<CR>", desc = "Go to window to the top" },
        { "<leader>wl", ":wincmd l<CR>", desc = "Go to window to the right" },
      })
    end,
  },
  {
    "stevearc/quicker.nvim",
    event = "FileType qf",
    ---@module "quicker"
    ---@type quicker.SetupOptions
    config = function()
      require("quicker").setup({
        keys = {
          {
            ">",
            function()
              require("quicker").expand({
                before = 2,
                after = 2,
                add_to_existing = true,
              })
            end,
            desc = "Expand quickfix context",
          },
          {
            "<",
            function()
              require("quicker").collapse()
            end,
            desc = "Collapse quickfix context",
          },
        },
      })

      vim.keymap.set("n", "<leader>q", function()
        require("quicker").toggle()
      end, {
        desc = "Toggle quickfix",
      })
    end,
  },
}
