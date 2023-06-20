local function setup_lsp_servers()
  local handlers = require("plugins.lsp.handlers")
  local capabilities = require("cmp_nvim_lsp").default_capabilities()

  local lspconfig = require("lspconfig")
  local lsp_defaults = lspconfig.util.default_config

  require("lspconfig-bundler").setup()

  lsp_defaults.capabilities = vim.tbl_deep_extend("force", lsp_defaults.capabilities, capabilities)

  lspconfig.solargraph.setup({})
  lspconfig.bashls.setup({})
  lspconfig.cmake.setup({})
  lspconfig.cssls.setup({})
  lspconfig.dockerls.setup({})
  lspconfig.graphql.setup({})
  lspconfig.html.setup({})
  lspconfig.marksman.setup({})
  lspconfig.vuels.setup({})

  lspconfig.gopls.setup({
    init_options = {
      usePlaceholders = true,
    },
    on_attach = function(client, ...)
      client.server_capabilities.semanticTokensProvider = {
        full = true,
        legend = {
          tokenModifiers = client.config.capabilities.textDocument.semanticTokens.tokenModifiers,
          tokenTypes = client.config.capabilities.textDocument.semanticTokens.tokenTypes,
        },
      }
      handlers.on_attach(client, ...)
    end,
    settings = {
      gopls = {
        hints = {
          assignVariableTypes = true,
          compositeLiteralFields = true,
          constantValues = true,
          functionTypeParameters = true,
          parameterNames = true,
          rangeVariableTypes = true,
        },
        experimentalPostfixCompletions = true,
        analyses = {
          unusedparams = true,
          shadow = true,
        },
        staticcheck = true,
      },
    },
  })

  lspconfig.jsonls.setup({
    before_init = function(_, config)
      config.settings.json.schemas = require("schemastore").json.schemas()
    end,
    filetypes = { "json", "json5", "jsonc" },
  })

  lspconfig.lua_ls.setup({
    settings = {
      Lua = {
        completion = {
          callSnippet = "Both",
          keywordSnippet = "Replace",
          workspaceWord = true,
        },
        diagnostics = {
          -- Have the language server to recognize the `vim` global.
          globals = { "bit", "vim" },
        },
        format = {
          enable = false,
        },
        hint = {
          enable = true,
          arrayIndex = "Disable",
          setType = true,
          paramName = "Disable",
        },
        runtime = {
          version = "Lua 5.1",
        },
        telemetry = {
          enable = false,
        },
        workspace = {
          checkThirdParty = false,
        },
      },
    },
  })

  lspconfig.yamlls.setup({
    before_init = function(_, config)
      config.settings.yaml.schemas = require("schemastore").json.schemas()
    end,
    settings = {
      -- https://github.com/redhat-developer/vscode-redhat-telemetry#how-to-disable-telemetry-reporting
      redhat = { telemetry = { enabled = false } },
      yaml = {
        completion = true,
        format = {
          enable = true,
          singleQuote = false,
        },
        hover = true,
        validate = true,
      },
    },
  })
end

return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      {
        "lukas-reineke/lsp-format.nvim",
        dependencies = {
          "folke/neodev.nvim",
          opts = {
            library = {
              plugins = false,
            },
            setup_jsonls = false,
          },
        },
      },
      "mihyaeru21/nvim-lspconfig-bundler",
    },
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("lspconfig.ui.windows").default_options.border = vim.g.border

      vim.lsp.set_log_level(vim.log.levels.ERROR)

      require("vim.lsp.log").set_format_func(vim.inspect)

      vim.keymap.set("n", "<leader>li", vim.cmd.LspInfo, { desc = "LSP Info" })
      vim.keymap.set("n", "<leader>ll", vim.cmd.LspLog, { desc = "LSP Log" })
      vim.keymap.set("n", "<leader>lr", vim.cmd.LspRestart, { desc = "LSP Restart" })

      setup_lsp_servers()
    end,
  },
  {
    "aznhe21/actions-preview.nvim",
    config = function()
      require("actions-preview").setup({
        diff = {
          algorithm = "patience",
          ignore_whitespace = true,
        },
      })
    end,
  },
  {
    "SmiteshP/nvim-navic",
    event = "LspAttach",
    config = function()
      vim.g.navic_silence = true

      require("nvim-navic").setup({
        highlight = true,
      })
    end,
  },
  { "b0o/schemastore.nvim", version = false },
  {
    "jose-elias-alvarez/typescript.nvim",
    config = function()
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      local handlers = require("plugins.lsp.handlers")

      require("typescript").setup({
        debug = false,
        disable_commands = false,
        disable_formatting = true,
        go_to_source_definition = {
          fallback = true, -- Fall back to standard LSP definition on failure.
        },
        server = {
          capabilities = capabilities,
          filetypes = { "javascript", "javascript.jsx", "typescript", "typescript.tsx" },
          on_attach = handlers.on_attach,
          settings = {
            completions = {
              completeFunctionCalls = true,
            },
          },
        },
      })

      require("null-ls").register(require("typescript.extensions.null-ls.code-actions"))
    end,
  },
  {
    "crispgm/nvim-go",
    cmd = {
      "GoFormat",
      "GoGet",
      "GoInstall",
      "GoLint",
    },
    event = "VeryLazy",
  },
  { "VidocqH/lsp-lens.nvim", config = true, lazy = false },
  { "smjonas/inc-rename.nvim", config = true },
  { "zbirenbaum/neodim", branch = "v2", config = true, event = "LspAttach" },
  { "simrat39/rust-tools.nvim" },
  { "anuvyklack/hydra.nvim" },
}
