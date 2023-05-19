local handlers = require("plugins.lsp.handlers")

local servers = {
  bashls = {},
  cmake = {},
  cssls = {},
  dockerls = {},
  graphql = {},
  html = {},
  marksman = {},
  solargraph = {},
  gopls = {
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
  },
  jsonls = {
    before_init = function(_, config)
      config.settings.json.schemas = require("schemastore").json.schemas()
    end,
    filetypes = { "json", "json5", "jsonc" },
  },
  lua_ls = {
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
  },
  tsserver = function()
    require("typescript").setup({
      debug = false,
      disable_commands = false,
      disable_formatting = true,
      go_to_source_definition = {
        fallback = true, -- Fall back to standard LSP definition on failure.
      },
      server = {
        capabilities = handlers.capabilities(),
        filetypes = { "javascript", "javascript.jsx", "typescript", "typescript.tsx" },
        on_attach = handlers.on_attach,
        settings = {
          completions = {
            completeFunctionCalls = true,
          },
        },
      },
    })

    -- Inject code actions to null-ls.
    require("null-ls").register(require("typescript.extensions.null-ls.code-actions"))
  end,
  yamlls = {
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
  },
}

return {
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("lspconfig.ui.windows").default_options.border = vim.g.border

      vim.lsp.set_log_level(vim.log.levels.ERROR)

      require("vim.lsp.log").set_format_func(vim.inspect)

      vim.keymap.set("n", "<leader>li", vim.cmd.LspInfo, { desc = "  LSP Info" })
      vim.keymap.set("n", "<leader>ll", vim.cmd.LspLog, { desc = "LSP Log" })
      vim.keymap.set("n", "<leader>lr", vim.cmd.LspRestart, { desc = "  LSP Restart" })
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
  -- Mason related packages follow.
  {
    "williamboman/mason.nvim",
    build = ":MasonUpdate",
    cmd = { "Mason", "MasonInstall", "MasonUninstall" },
    config = function()
      require("mason").setup({
        ensure_installed = { "glow" },
        ui = {
          border = vim.g.border,
        },
      })
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      local hhandlers = {}

      for name, handler in pairs(servers) do
        if type(handler) == "function" then
          hhandlers[name] = handler
        else
          hhandlers[name] = function()
            require("lspconfig")[name].setup(vim.tbl_deep_extend("force", {
              capabilities = handlers.capabilities(),
              on_attach = handlers.on_attach,
            }, handler))
          end
        end
      end

      require("mason-lspconfig").setup({
        automatic_installation = true,
        ensure_installed = vim.tbl_keys(hhandlers),
        handlers = hhandlers,
      })
    end,
    event = { "BufReadPre", "BufNewFile" },
  },
  {
    "jay-babu/mason-null-ls.nvim",
    dependencies = { "mason.nvim", "null-ls.nvim" },
    event = "VeryLazy",
    opts = {
      automatic_installation = true,
    },
  },
  { "b0o/schemastore.nvim", version = false },
  {
    "folke/neodev.nvim",
    ft = "lua",
    opts = {
      library = {
        plugins = false,
      },
      setup_jsonls = false,
    },
  },
  { "jose-elias-alvarez/typescript.nvim" },
  { "smjonas/inc-rename.nvim", config = true },
  { "yioneko/nvim-type-fmt", lazy = false }, -- LSP handler of textDocument/onTypeFormatting for nvim. Sets itself up via an LspAttach autocmd.
  { "zbirenbaum/neodim", branch = "v2", config = true, event = "LspAttach" },
}
