local M = {}

M.setup = function()
  local handlers = require("plugins.lsp.handlers")
  local capabilities = require("cmp_nvim_lsp").default_capabilities()

  local lspconfig = require("lspconfig")
  local lsp_defaults = lspconfig.util.default_config

  require("lspconfig-bundler").setup()

  lsp_defaults.capabilities = vim.tbl_deep_extend("force", lsp_defaults.capabilities, capabilities)
  lsp_defaults.on_attach = handlers.on_attach

  lspconfig.solargraph.setup({})
  lspconfig.bashls.setup({})
  lspconfig.clangd.setup({})
  lspconfig.cmake.setup({})
  lspconfig.cssls.setup({})
  lspconfig.dockerls.setup({})
  lspconfig.docker_compose_language_service.setup({})
  lspconfig.graphql.setup({})
  lspconfig.html.setup({})
  lspconfig.marksman.setup({})
  lspconfig.tsserver.setup({})
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
          globals = { "vim" },
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

return M
