local M = {}

M.setup = function()
  -- neodev must be set up before lspconfig
  require("neodev").setup({})

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
  lspconfig.tsserver.setup({
    settings = {
      javascript = {
        inlayHints = {
          includeInlayEnumMemberValueHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayParameterNameHints = "all",
          includeInlayParameterNameHintsWhenArgumentMatchesName = true,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayVariableTypeHints = true,
        },
      },
      typescript = {
        inlayHints = {
          includeInlayEnumMemberValueHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayParameterNameHints = "all",
          includeInlayParameterNameHintsWhenArgumentMatchesName = true,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayVariableTypeHints = true,
        },
      },
    },
  })
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
        gofumpt = true,
        codelenses = {
          gc_details = false,
          generate = true,
          regenerate_cgo = true,
          run_govulncheck = true,
          test = true,
          tidy = true,
          upgrade_dependency = true,
          vendor = true,
        },
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
          version = "LuaJIT",
        },
        telemetry = {
          enable = false,
        },
        workspace = {
          checkThirdParty = false,
          -- Make the server aware of neovim runtime files
          library = vim.api.nvim_get_runtime_file("", true),
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
