-- Remap space as leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- SETTINGS
vim.opt.termguicolors = true -- Use full colors
vim.o.hlsearch = true -- Highlight on search
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>") -- Clear search on escape

-- Sync clipboard between OS and Neovim.
--  Schedule the setting after `UiEnter` because it can increase startup-time.
vim.schedule(function()
  vim.o.clipboard = "unnamedplus"
end)

vim.opt.cursorline = true -- Highlight cursor line
vim.opt.matchpairs:append({ "<:>" }) -- Highlights matching brackets - '%' to jump between them
vim.opt.showmode = false -- -- INSERT -- is not shown
vim.opt.signcolumn = "yes" -- Always show signcolumn
vim.opt.relativenumber = true -- Hybrid line numbers
vim.opt.number = true -- Hybrid line numbers
-- Menus, search and completion
vim.opt.ignorecase = true -- Ignores case in search
vim.opt.smartcase = true -- Uses smartcase in search
vim.opt.incsearch = true -- Search incrementally
vim.opt.magic = true -- Magic for regex
vim.opt.wildmenu = true -- Use wildmenu
vim.opt.wildmode = "longest:full,full" -- First tab completes longest common string
vim.opt.completeopt = "menuone,noselect,noinsert,popup" -- Completion menu options
-- Misc
vim.opt.foldmethod = "expr" -- Use folding expression
vim.opt.foldlevel = 99 -- Never fold by default
vim.opt.mouse = "a" -- Enable mouse support
vim.opt.eb = false -- No error bells
vim.opt.swapfile = false -- No swap file
vim.opt.backup = false -- No backup file
vim.opt.undofile = true -- Makes undofile for history
vim.opt.lazyredraw = true -- No redraw
vim.opt.updatetime = 200 -- Default is 4000 - more updates
vim.opt.timeoutlen = 300 -- Default is 1000 - faster
vim.opt.splitbelow = true -- Split opened below instead of above
vim.opt.splitright = true -- Split opened to the right instead of left

-- Highlight on yank
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

-- lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  change_detection = {
    enabled = true,
    notify = false,
  },
  spec = {
    "nvim-lua/plenary.nvim",
    {
      "miikanissi/modus-themes.nvim", -- Modus themes
      priority = 1000,
      config = function()
        require("modus-themes").setup({
          hide_inactive_statusline = true,
          line_nr_column_background = false,
          sign_column_background = false,
        })
        -- vim.cmd.colorscheme("modus")
      end,
    },

    {
      "folke/tokyonight.nvim",
      lazy = false,
      priority = 1000,
      opts = {},
      config = function()
        vim.cmd.colorscheme("tokyonight-night")
      end,
    },

    {
      "ellisonleao/gruvbox.nvim",
      priority = 1000,
      config = function()
        require("gruvbox").setup({
          contrast = "dark",
        })
        -- vim.cmd.colorscheme("gruvbox")
      end,
    },

    {
      "nvim-lualine/lualine.nvim", -- Fancier statusline
      event = "VimEnter",
      opts = {
        options = {
          icons_enabled = true,
          theme = "auto",
          component_separators = "|",
          section_separators = "",
        },
        sections = {
          lualine_c = {
            {
              "filename",
              file_status = false,
              path = 3,
            },
          },
        },
        inactive_sections = {
          lualine_c = {
            {
              "filename",
              file_status = false,
              path = 1,
            },
          },
        },
      },
    },

    {
      "ibhagwan/fzf-lua", -- Fuzzy finder / search
      event = "VimEnter",
      config = function()
        require("fzf-lua").setup({
          fzf_colors = {
            ["fg"] = { "fg", "CursorLine" },
            ["bg"] = { "bg", "Normal" },
            ["hl"] = { "fg", "Comment" },
            ["fg+"] = { "fg", "Normal" },
            ["bg+"] = { "bg", "CursorLine" },
            ["hl+"] = { "fg", "Statement" },
            ["info"] = { "fg", "PreProc" },
            ["prompt"] = { "fg", "Conditional" },
            ["pointer"] = { "fg", "Exception" },
            ["marker"] = { "fg", "Keyword" },
            ["spinner"] = { "fg", "Label" },
            ["header"] = { "fg", "Comment" },
            ["gutter"] = "-1",
          },
        })
        -- Files and buffers
        vim.keymap.set("n", "<leader>,", require("fzf-lua").buffers, { desc = "Fzf: [B]uffers" })
        vim.keymap.set("n", "<leader>bb", require("fzf-lua").buffers, { desc = "Fzf: [B]uffers" })
        vim.keymap.set("n", "<leader>pf", require("fzf-lua").files, { desc = "Fzf: [F]iles" })
        vim.keymap.set("n", "<leader><leader>", require("fzf-lua").files, { desc = "Fzf: [F]iles" })
        vim.keymap.set("n", "<leader>ff", function()
          require("fzf-lua").files({ cwd = vim.fn.expand("%:p:h") })
        end, { desc = "Fzf: F[i]nf file in current dir" })
        vim.keymap.set("n", "<leader>fr", require("fzf-lua").oldfiles, { desc = "Fzf: [O]ld Files" })

        -- Search
        vim.keymap.set("n", "<leader>sp", require("fzf-lua").live_grep, { desc = "Fzf: [L]ive Grep" })
        vim.keymap.set("n", "<leader>'", require("fzf-lua").resume, { desc = "Fzf: Resume" })
      end,
    },
    {
      "nvim-treesitter/nvim-treesitter",
      dependencies = {
        "nvim-treesitter/nvim-treesitter-context",
        "windwp/nvim-ts-autotag",
      },
      branch = "main",
      lazy = false,
      build = ":TSUpdate",
      config = function()
        require("nvim-treesitter").setup()

        -- fold
        vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
        vim.wo[0][0].foldmethod = "expr"
        vim.wo[0][0].foldenable = false

        require("nvim-ts-autotag").setup({})
        require("treesitter-context").setup({
          enable = true,
          multiline_threshold = 1,
        })
      end,
    },

    {
      "mfussenegger/nvim-lint", -- Non LSP code linting
      event = {
        "BufReadPre",
        "BufNewFile",
      },
      opts = {
        events = { "BufWritePost", "BufReadPost", "InsertLeave" },
        linters_by_ft = {
          fish = { "fish" },
          eruby = { "erb_lint" },
          javascript = { "eslint_d" },
          javascriptreact = { "eslint_d" },
          markdown = { "vale", "markdownlint-cli2" },
          ruby = { "rubocop" },
          sh = { "shellcheck" },
          svelte = { "eslint_d" },
          typescript = { "eslint_d" },
        },
      },
      config = function(_, opts)
        local lint = require("lint")

        lint.linters_by_ft = opts.linters_by_ft

        local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

        vim.api.nvim_create_autocmd(opts.events, {
          group = lint_augroup,
          callback = function()
            lint.try_lint()
          end,
        })
      end,
    },

    {
      "stevearc/conform.nvim", -- Non LSP code formatting
      event = { "BufWritePre" },
      cmd = { "ConformInfo" },
      keys = {
        {
          "<leader>bf",
          function()
            require("conform").format({ async = true, lsp_format = "last" })
          end,
          mode = "",
          desc = "[F]ormat Current Buffer",
        },
      },
      init = function()
        vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
      end,
      ---@module "conform"
      ---@type conform.setupOpts
      opts = {
        notify_on_error = false,
        formatters = {
          injected = {
            options = { ignore_errors = true },
          },
        },
        -- Set up format-on-save
        format_on_save = { timeout_ms = 500 },
        formatters_by_ft = {
          css = { "prettier" },
          fish = { "fish_indent" },
          html = { "prettier" },
          javascript = { "prettierd", "prettier", stop_after_first = true },
          json = { "prettier" },
          lua = { "stylua" },
          markdown = { "prettier", "injected" },
          scss = { "prettier" },
          sh = { "shfmt" },
          typescript = { "prettier" },
          xml = { "prettier" },
          yaml = { "prettier" },
          ["*"] = { "injected" },
          -- Use the "_" filetype to run formatters on filetypes that don't
          -- have other formatters configured.
          ["_"] = { "trim_whitespace" },
        },
      },
    },

    {
      "neovim/nvim-lspconfig",
      dependencies = {
        { "mason-org/mason.nvim", opts = {} },
        "mason-org/mason-lspconfig.nvim",
        "WhoIsSethDaniel/mason-tool-installer.nvim",
        "mihyaeru21/nvim-lspconfig-bundler",
        { "j-hui/fidget.nvim", opts = {} },
        "saghen/blink.cmp",
      },
      config = function()
        vim.api.nvim_create_autocmd("LspAttach", {
          group = vim.api.nvim_create_augroup("vs-lsp-attach", { clear = true }),
          callback = function(event)
            local map = function(keys, func, desc, mode)
              mode = mode or "n"
              vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
            end

            map("grn", vim.lsp.buf.rename, "[R]e[n]ame")
            map("gra", vim.lsp.buf.code_action, "[G]oto Code [A]ction", { "n", "x" })
            map("grr", require("fzf-lua").lsp_references, "[G]oto [R]eferences")
            map("gri", require("fzf-lua").lsp_implementations, "[G]oto [I]mplementation")
            map("grd", require("fzf-lua").lsp_definitions, "[G]oto [D]efinition")
            map("grD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
            map("gO", require("fzf-lua").lsp_document_symbols, "Open Document Symbols")
            map("gW", require("fzf-lua").lsp_live_workspace_symbols, "Open Workspace Symbols")
            map("grt", require("fzf-lua").lsp_typedefs, "[G]oto [T]ype Definition")

            local client = vim.lsp.get_client_by_id(event.data.client_id)
            if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf) then
              local highlight_augroup = vim.api.nvim_create_augroup("vs-lsp-highlight", { clear = false })
              vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
                buffer = event.buf,
                group = highlight_augroup,
                callback = vim.lsp.buf.document_highlight,
              })

              vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
                buffer = event.buf,
                group = highlight_augroup,
                callback = vim.lsp.buf.clear_references,
              })

              vim.api.nvim_create_autocmd("LspDetach", {
                group = vim.api.nvim_create_augroup("vs-lsp-detach", { clear = true }),
                callback = function(event2)
                  vim.lsp.buf.clear_references()
                  vim.api.nvim_clear_autocmds({ group = "vs-lsp-highlight", buffer = event2.buf })
                end,
              })
            end

            if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
              map("<leader>th", function()
                vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
              end, "[T]oggle Inlay [H]ints")
            end
          end,
        })

        -- Diagnostic Config
        -- See :help vim.diagnostic.Opts
        vim.diagnostic.config({
          severity_sort = true,
          float = { border = "rounded", source = "if_many" },
          underline = { severity = vim.diagnostic.severity.ERROR },
          signs = {
            text = {
              [vim.diagnostic.severity.ERROR] = "󰅚 ",
              [vim.diagnostic.severity.WARN] = "󰀪 ",
              [vim.diagnostic.severity.INFO] = "󰋽 ",
              [vim.diagnostic.severity.HINT] = "󰌶 ",
            },
          },
          virtual_text = {
            source = "if_many",
            spacing = 2,
            format = function(diagnostic)
              local diagnostic_message = {
                [vim.diagnostic.severity.ERROR] = diagnostic.message,
                [vim.diagnostic.severity.WARN] = diagnostic.message,
                [vim.diagnostic.severity.INFO] = diagnostic.message,
                [vim.diagnostic.severity.HINT] = diagnostic.message,
              }
              return diagnostic_message[diagnostic.severity]
            end,
          },
        })

        local capabilities = require("blink.cmp").get_lsp_capabilities()
        local servers = {
          lua_ls = {
            settings = {
              Lua = {
                completion = {
                  callSnippet = "Replace",
                },
              },
            },
          },
        }

        local ensure_installed = vim.tbl_keys(servers or {})
        vim.list_extend(ensure_installed, {
          "stylua",
          "ruby-lsp",
          "rubocop",
        })
        require("lspconfig-bundler").setup()
        require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

        require("mason-lspconfig").setup({
          ensure_installed = {},
          automatic_installation = false,
          handlers = {
            function(server_name)
              local server = servers[server_name] or {}
              server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
              require("lspconfig")[server_name].setup(server)
            end,
          },
        })
      end,
    },

    {
      "saghen/blink.cmp",
      dependencies = { "rafamadriz/friendly-snippets" },
      version = "1.*",
      ---@module 'blink.cmp'
      ---@type blink.cmp.Config
      opts = {
        -- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
        -- 'super-tab' for mappings similar to vscode (tab to accept)
        -- 'enter' for enter to accept
        -- 'none' for no mappings
        --
        -- All presets have the following mappings:
        -- C-space: Open menu or open docs if already open
        -- C-n/C-p or Up/Down: Select next/previous item
        -- C-e: Hide menu
        -- C-k: Toggle signature help (if signature.enabled = true)
        --
        -- See :h blink-cmp-config-keymap for defining your own keymap
        keymap = { preset = "super-tab" },

        appearance = {
          -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
          -- Adjusts spacing to ensure icons are aligned
          nerd_font_variant = "mono",
        },

        -- (Default) Only show the documentation popup when manually triggered
        completion = { documentation = { auto_show = false } },

        -- Default list of enabled providers defined so that you can extend it
        -- elsewhere in your config, without redefining it, due to `opts_extend`
        sources = {
          default = { "lsp", "path", "snippets", "buffer" },
        },

        -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
        -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
        -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
        --
        -- See the fuzzy documentation for more information
        fuzzy = { implementation = "prefer_rust_with_warning" },
      },
      opts_extend = { "sources.default" },
    },

    {
      "stevearc/oil.nvim",
      ---@module 'oil'
      ---@type oil.SetupOpts
      opts = {},
      dependencies = { "echasnovski/mini.icons" },
      -- Lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
      lazy = false,
      keys = {
        { "-", "<cmd>Oil<cr>", desc = "Open parent directory" },
      },
    },

    -- VCS
    {
      "NeogitOrg/neogit",
      dependencies = {
        "sindrets/diffview.nvim",
      },
      keys = {
        { "<leader>gg", "<Esc>:Neogit<CR>", silent = true, desc = "Neogit" },
      },
      opts = {
        graph_style = "unicode",
        use_telescope = false,
      },
      config = true,
    },
    {
      "ruifm/gitlinker.nvim",
      keys = {
        {
          "<leader>gb",
          '<cmd>lua require"gitlinker".get_buf_range_url("n", {action_callback = require"gitlinker.actions".open_in_browser})<cr>',
          mode = "n",
          desc = "GitLinker: Open in browser",
        },
        {
          "<leader>gb",
          '<cmd>lua require"gitlinker".get_buf_range_url("v", {action_callback = require"gitlinker.actions".open_in_browser})<cr>',
          mode = "v",
          desc = "GitLinker: Open in browser",
        },
        {
          "<leader>gy",
          '<cmd>lua require"gitlinker".get_buf_range_url("n")<cr>',
          mode = "n",
          desc = "GitLinker: Copy URL",
        },
        {
          "<leader>gy",
          '<cmd>lua require"gitlinker".get_buf_range_url("v")<cr>',
          mode = "v",
          desc = "GitLinker: Copy URL",
        },
      },
      config = true,
    },
    {
      "lewis6991/gitsigns.nvim",
      opts = {
        -- See `:help gitsigns.txt`
        signs = {
          add = { text = "+" },
          change = { text = "~" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
        },
      },
    },
  },
})
