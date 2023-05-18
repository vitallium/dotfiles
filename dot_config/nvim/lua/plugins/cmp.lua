return {
  {
    "hrsh7th/nvim-cmp", -- Autocompletion plugin
    cmd = "CmpStatus",
    event = "VeryLazy",
    dependencies = {
      "hrsh7th/cmp-buffer", -- Buffer source for nvim-cmp
      "hrsh7th/cmp-path", -- Path source for nvim-cmp
      "hrsh7th/cmp-nvim-lsp", -- LSP source for nvim-cmp
      "hrsh7th/cmp-nvim-lua", -- Lua Completions
      "hrsh7th/cmp-nvim-lsp-signature-help", -- Function signature source for nvim-cmp
      "hrsh7th/cmp-cmdline", -- Command line source for nvim-cmp
      "hrsh7th/cmp-emoji",
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
      "onsails/lspkind.nvim",
      "saadparwaiz1/cmp_luasnip", -- Snippets source for nvim-cmp
      {
        "tzachar/cmp-tabnine", -- TabNine
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
              lua = true,
            },
            show_prediction_strength = false,
          })
        end,
      },
    },
    config = function()
      -- nvim-cmp setup
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local lspkind = require("lspkind")
      lspkind.init({
        mode = "symbol_text",
      })

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        view = {
          entries = {
            name = "custom",
            selection_order = "near_cursor",
          },
        },
        completion = {
          keyword_length = 3,
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
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<CR>"] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = false, -- Selection required to complete on "Enter".
          }),
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expandable() then
              luasnip.expand()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, {
            "i",
            "s",
          }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, {
            "i",
            "s",
          }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "nvim_lsp_signature_help" },
          { name = "cmp_tabnine" },
          { name = "luasnip" },
        }, {
          { name = "buffer" },
          { name = "path" },
          { name = "emoji" },
        }),
        confirm_opts = {
          behavior = cmp.ConfirmBehavior.Select,
        },
        window = {
          documentation = cmp.config.window.bordered(),
          completion = {
            col_offset = 3,
            side_padding = 0,
          },
        },
        experimental = {
          native_menu = false,
          ghost_text = false,
        },
        formatting = {
          fields = { "kind", "abbr", "menu" },
          format = function(entry, vim_item)
            local kind = lspkind.cmp_format({ maxwidth = 50 })(entry, vim_item)

            local strings = vim.split(kind.kind, "%s", { trimempty = true })
            kind.kind = " " .. strings[1] .. " "
            if #strings > 1 then
              kind.menu = "    (" .. strings[2] .. ")"
            end

            return kind
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
        sources = cmp.config.sources({
          { name = "cmdline" },
        }, {
          { name = "path" },
        }),
      })

      -- Auto-pair setup.
      local cmp_autopairs = require("nvim-autopairs.completion.cmp")
      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    end,
  },
}
