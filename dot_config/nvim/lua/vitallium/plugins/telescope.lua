return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-ui-select.nvim", -- optional, for using telescope in more places
      "nvim-telescope/telescope-file-browser.nvim",
      "nvim-tree/nvim-web-devicons", -- optional, for icons
      "debugloop/telescope-undo.nvim",
    },
    cmd = { "Telescope" },
    keys = {
      {
        "<leader><leader>",
        function()
          require("telescope.builtin").find_files()
        end,
        desc = "Find file",
      },
      {
        "<leader>/",
        function()
          require("telescope.builtin").live_grep()
        end,
        desc = "Grep directory",
      },
      {
        "<leader>?",
        function()
          require("telescope.builtin").current_buffer_fuzzy_find()
        end,
        desc = "Grep current buffer",
      },
      {
        "<leader>;",
        function()
          require("telescope.builtin").command_history()
        end,
        desc = "Command history",
      },
      {
        "<leader>:",
        function()
          require("telescope.builtin").commands()
        end,
        desc = "Commands",
      },
      {
        "<leader>r",
        function()
          require("telescope.builtin").resume()
        end,
        desc = "Resume telescope",
      },
      {
        "<leader>bb",
        function()
          require("telescope.builtin").buffers()
        end,
        desc = "Find buffer",
      },
      {
        "<leader>cs",
        function()
          require("telescope.builtin").lsp_document_symbols()
        end,
        desc = "Symbols in document",
      },
      {
        "<leader>fr",
        function()
          require("telescope.builtin").oldfiles()
        end,
        desc = "Find previously opened file",
      },
      {
        "<leader>ff",
        function()
          require("telescope").extensions.file_browser.file_browser({
            path = "%:p:h",
            hidden = true,
            grouped = true,
          })
        end,
        desc = "Browse files",
      },
      {
        "<leader>h",
        function()
          require("telescope.builtin").help_tags()
        end,
        desc = "NeoVim help tags",
      },
      {
        "<leader>pf",
        function()
          require("telescope.builtin").git_files()
        end,
        desc = "Find files",
      },
    },
    config = function(_, opts)
      local telescope = require("telescope")
      telescope.load_extension("undo")
      telescope.load_extension("file_browser")
      telescope.load_extension("fzf")
      telescope.load_extension("frecency")
      telescope.load_extension("ui-select")
      telescope.load_extension("textcase")
    end,
    opts = function(_, opts)
      local telescope = require("telescope")
      local actions = require("telescope.actions")
      local trouble = require("trouble.providers.telescope")
      local actionlayout = require("telescope.actions.layout")

      telescope.setup({
        defaults = {
          sorting_strategy = "ascending",
          vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
            "--hidden",
            "--glob=!.git",
          },
          color_devicons = true,
          layout_strategy = "horizontal",
          winblend = 5,
          layout_config = {
            prompt_position = "top",
            horizontal = {
              width = 0.75,
              height = 0.85,
              width_padding = 0.04,
              height_padding = 0.1,
              preview_width = 0.6,
            },
            vertical = {
              width_padding = 0.05,
              height_padding = 1,
              preview_height = 0.5,
            },
          },
          mappings = {
            i = {
              ["<C-u>"] = actions.preview_scrolling_up,
              ["<C-d>"] = actions.preview_scrolling_down,
              ["<C-j>"] = actions.move_selection_next,
              ["<C-k>"] = actions.move_selection_previous,
              ["<C-Space>"] = actionlayout.toggle_preview,
              ["<esc>"] = actions.close,
              ["<C-x>"] = actions.cycle_previewers_next,
              ["<C-a>"] = actions.cycle_previewers_prev,
            },
            n = { ["<c-t>"] = trouble.open_with_trouble },
          },
        },
        pickers = {
          find_files = {
            previewer = false,
            find_command = { "rg", "--hidden", "--files", "--smart-case", "--glob=!.git" },
          },
          git_files = {
            previewer = false,
          },
        },
        extensions = {
          file_browser = {
            hijack_netrw = true,
            previewer = false,
          },
          fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
          },
        },
      })
    end,
  },
  {
    -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make",
    cond = vim.fn.executable("make") == 1,
    dependencies = {
      "nvim-telescope/telescope.nvim",
    },
  },
  {
    -- Order fuzzy list by frequency
    "nvim-telescope/telescope-frecency.nvim",
    dependencies = {
      "kkharji/sqlite.lua",
    },
  },
}
