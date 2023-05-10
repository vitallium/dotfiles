return {
  "folke/noice.nvim",
  event = "VeryLazy",
  dependencies = {
    { "MunifTanjim/nui.nvim", lazy = true },
  },
  opts = {
    presets = {
      command_palette = true,
      long_message_to_split = true,
      lsp_doc_border = true,
    },
    cmdline = {
      format = {
        cmdline = { icon = "❯" },
        search_down = { icon = "🔍 ↓" },
        search_up = { icon = "🔍 ↑" },
        IncRename = {
          pattern = "^:%s*IncRename%s+",
          icon = " ",
          conceal = true,
        },
      }
    },
    popupmenu = {
      kind_icons = false
    },
    lsp = {
      progress = {
        enabled = false
      },
      hover = {
        silent = true,
      },
      override = {
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
        ["vim.lsp.util.stylize_markdown"] = true,
        ["cmp.entry.get_documentation"] = true,
      },
    },
    views = {
      cmdline_popup = {
        position = {
          row = "100%",
        },
        size = {
          height = "auto",
          width = "99%",
        },
        border = {
          style = "none",
          padding = { 1, 2 },
        },
        filter_options = {},
      },
      popupmenu = {
        position = {
          row = -5
        },
        size = {
          height = "auto",
          width = "100%"
        },
        border = {
          style = "none",
          padding = { 2, 2 },
        },
        filter_options = {},
      }
    },
    routes = {
      {
        filter = {
          event = "msg_show",
          kind = "",
          find = "written",
        },
        opts = { skip = true },
      },
    },
  },
}
