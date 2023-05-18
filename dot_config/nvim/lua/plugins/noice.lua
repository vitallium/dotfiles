return {
  "folke/noice.nvim",
  dependencies = { "MunifTanjim/nui.nvim" },
  event = "VeryLazy",
  opts = {
    cmdline = {
      enabled = false,
    },
    lsp = {
      documentation = {
        enabled = true,
        view = "hover",
      },
      hover = {
        enabled = true,
        opts = {
          border = {
            style = vim.g.border,
          },
        },
      },
      override = {
        -- override the default lsp markdown formatter with Noice
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
        -- override the lsp markdown formatter with Noice
        ["vim.lsp.util.stylize_markdown"] = true,
        -- override cmp documentation with Noice (needs the other options to work)
        ["cmp.entry.get_documentation"] = true,
      },
      progress = {
        enabled = false,
      },
      signature = {
        enabled = true,
        auto_open = {
          enabled = true,
          trigger = true, -- Automatically show signature help when typing a trigger character from the LSP
          luasnip = true, -- Will open signature help when jumping to Luasnip insert nodes
          throttle = 50, -- Debounce lsp signature help request by 50ms
        },
      },
    },
    messages = {
      enabled = false,
    },
    notify = {
      enabled = true,
    },
    presets = {
      bottom_search = true, -- use a classic bottom cmdline for search
      command_palette = false, -- position the cmdline and popupmenu together
      long_message_to_split = true, -- long messages will be sent to a split
      inc_rename = true, -- enables an input dialog for inc-rename.nvim
      lsp_doc_border = true, -- add a border to hover docs and signature help
    },
    routes = {
      {
        filter = {
          any = {
            { find = "No active Snippet" },
            { find = "No information available" },
            { find = "No signature help available" },
            { find = "Running provider" },
            { find = "The coroutine failed with this message" },
          },
        },
        opts = {
          skip = true,
        },
      },
      {
        filter = {
          event = "msg_show",
          kind = "",
          find = "written",
        },
        opts = { skip = true },
      },
    },
    views = {
      split = {
        win_options = {
          winhighlight = { Normal = "Normal" },
        },
      },
      popupmenu = {
        relative = "editor",
        position = {
          row = 9,
          col = "50%",
        },
        size = {
          width = 60,
          height = 10,
        },
        border = {
          style = vim.g.border,
          padding = { 0, 1 },
        },
        win_options = {
          winhighlight = {
            Normal = "NormalFloat",
            FloatBorder = "FloatBorder",
          },
        },
      },
    },
  },
}
