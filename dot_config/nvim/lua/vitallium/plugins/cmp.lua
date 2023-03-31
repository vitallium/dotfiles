return {
  {
    "hrsh7th/nvim-cmp", -- Autocompletion plugin
    event = { "BufReadPost", "BufNewFile" },
    dependencies = {
      "hrsh7th/cmp-nvim-lsp", -- LSP source for nvim-cmp
      "hrsh7th/cmp-nvim-lsp-signature-help", -- Function signature source for nvim-cmp
      "hrsh7th/cmp-buffer", -- Buffer source for nvim-cmp
      "hrsh7th/cmp-path", -- Path source for nvim-cmp
      "hrsh7th/cmp-cmdline", -- Command line source for nvim-cmp
      {
        "L3MON4D3/LuaSnip", -- Snippets plugin
        dependencies = {
          "rafamadriz/friendly-snippets",
        },
        config = function()
          -- Load "friendly-snippets" (dependency):
          require("luasnip.loaders.from_vscode").lazy_load()
          -- Extend filetypes:
          require("luasnip").filetype_extend("typescript", { "javascript", "jsdoc" })
        end,
      },
      "saadparwaiz1/cmp_luasnip", -- Snippets source for nvim-cmp
      "ray-x/cmp-treesitter",
      {
        "tzachar/cmp-tabnine",
        build = "./install.sh",
        config = function()
          local tabnine = require("cmp_tabnine.config")

          tabnine:setup({
            max_lines = 1000,
            max_num_results = 20,
            sort = true,
            run_on_every_keystroke = true,
            snippet_placeholder = "..",
            ignored_file_types = {
              -- default is not to ignore
              -- uncomment to ignore in lua:
              -- lua = true
            },
            show_prediction_strength = false,
          })
        end,
      },
      -- working with neovim config/plugins
      "folke/neodev.nvim",
      -- hints
      "simrat39/inlay-hints.nvim",
    },
    config = function()
      local source_mapping = {
        buffer = "[Buffer]",
        nvim_lsp = "[LSP]",
        nvim_lua = "[Lua]",
        cmp_tabnine = "[TN]",
        path = "[Path]",
      }

      require("neodev").setup({ library = { plugins = { "neotest" }, types = true } })
      -- nvim-cmp setup
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          -- Use <C-j/k> to select candidates:
          ["<C-j>"] = cmp.mapping(function()
            cmp.select_next_item()
          end),
          ["<C-k>"] = cmp.mapping(function()
            cmp.select_prev_item()
          end),
          -- Other mappings:
          ["<C-d>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<CR>"] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = false, -- Selection required to complete on "Enter".
          }),
          -- For (S-)Tab, prefer snippet over completion.
          -- (Prefer C-j/k for completion anyway)
          ["<Tab>"] = cmp.mapping(function(fallback)
            if luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            elseif cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if luasnip.jumpable(-1) then
              luasnip.jump(-1)
            elseif cmp.visible() then
              cmp.select_prev_item()
            else
              fallback()
            end
          end, { "i", "s" }),
        }),
        sources = {
          -- Output will be prioritized according to order.
          { name = "nvim_lsp" },
          { name = "path" },
          { name = "buffer" },
          { name = "luasnip" },
          { name = "nvim_lsp_signature_help" },
          { name = "cmp_tabnine" },
          { name = "treesitter" },
        },
        formatting = {
          format = function(entry, vim_item)
            -- if you have lspkind installed, you can use it like
            -- in the following line:
            -- vim_item.kind = lspkind.symbolic(vim_item.kind, { mode = "symbol" })
            vim_item.menu = source_mapping[entry.source.name]
            if entry.source.name == "cmp_tabnine" then
              local detail = (entry.completion_item.data or {}).detail
              vim_item.kind = ""
              if detail and detail:find(".*%%.*") then
                vim_item.kind = vim_item.kind .. " " .. detail
              end

              if (entry.completion_item.data or {}).multiline then
                vim_item.kind = vim_item.kind .. " " .. "[ML]"
              end
            end
            local maxwidth = 80
            vim_item.abbr = string.sub(vim_item.abbr, 1, maxwidth)
            return vim_item
          end,
        },
      })

      -- `/` cmdline setup.
      cmp.setup.cmdline("/", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = { { name = "buffer" } },
      })

      -- `:` cmdline setup.
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({ { name = "path" } }, {
          { name = "cmdline", option = { ignore_cmds = { "Man", "!" } } },
        }),
      })

      -- Auto-pair setup.
      local cmp_autopairs = require("nvim-autopairs.completion.cmp")
      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    end,
  },
}
