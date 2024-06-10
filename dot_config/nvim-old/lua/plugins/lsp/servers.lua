local M = {}

M.setup = function()
  local handlers = require("plugins.lsp.handlers")

  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

  require("lspconfig-bundler").setup()

  local lsp_servers = {
    ruby_lsp = {},
    bashls = {},
    clangd = {},
    cmake = {},
    cssls = {},
    dockerls = {},
    docker_compose_language_service = {},
    graphql = {},
    html = {},
    marksman = {},
    tsserver = {
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
    },
    vuels = {},
    gopls = {
      init_options = {
        usePlaceholders = true,
      },
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
    },
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
    rust_analyzer = {},
  }
  require("mason").setup()

  local ensure_installed = vim.tbl_keys(lsp_servers or {})
  vim.list_extend(ensure_installed, {
    "stylua", -- Used to format Lua code
  })
  require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

  require("mason-lspconfig").setup({
    handlers = {
      function(server_name)
        local server = lsp_servers[server_name] or {}
        -- This handles overriding only values explicitly passed
        -- by the server configuration above. Useful when disabling
        -- certain features of an LSP (for example, turning off formatting for tsserver)
        server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
        server.on_attach = handlers.on_attach
        require("lspconfig")[server_name].setup(server)
      end,
    },
  })
end

return M
