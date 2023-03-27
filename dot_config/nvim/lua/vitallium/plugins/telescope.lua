return {
  {
    "nvim-telescope/telescope.nvim", -- Fancy picker (think fzf on steroids)
    version = "*",
    dependencies = {
      "nvim-telescope/telescope-file-browser.nvim", -- Think Emacs directory browser
      {
        "nvim-telescope/telescope-fzf-native.nvim", -- FZF algorithm for telescope
        build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
      },
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
    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")

      local trouble = require("trouble.providers.telescope")
      telescope.setup({
        defaults = {
          -- `hidden = true` is not supported in text grep commands.
          vimgrep_arguments = {
            "rg",
            "--hidden",
            "--no-ignore-vcs",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
            "--trim",
          },
          sorting_strategy = "ascending",
          mappings = {
            i = {
              ["<c-j>"] = actions.move_selection_next,
              ["<c-k>"] = actions.move_selection_previous,
              ["<c-t>"] = trouble.open_with_trouble,
              ["<esc>"] = actions.close, -- Close on first press of esc. No "normal" mode.
            },
            n = { ["<c-t>"] = trouble.open_with_trouble },
          },
          layout_strategy = "bottom_pane",
        },
        pickers = {
          find_files = {
            previewer = false,
            find_command = {
              "rg",
              "--files",
              "--no-ignore-vcs",
              "--hidden",
              "--no-heading",
              "--with-filename",
              "--column",
              "--smart-case",
              "--iglob",
              "!.git",
            },
            -- `hidden = true` will still show the inside of `.git/` as it's not `.gitignore`d.
            -- find_command = { "rg", "--files", "--hidden", "--glob", "!.git/*" },
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
      telescope.load_extension("file_browser")
      telescope.load_extension("fzf")
    end,
  },
}
