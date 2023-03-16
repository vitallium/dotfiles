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
      -- Icons in completion dialogue
      "onsails/lspkind.nvim",
      "ray-x/cmp-treesitter",
      { "tzachar/cmp-tabnine", build = "./install.sh" },
      -- working with neovim config/plugins
      "folke/neodev.nvim",
      -- hints
      "simrat39/inlay-hints.nvim",
    },
    config = function()
      require("neodev").setup({ library = { plugins = { "neotest" }, types = true } })
      -- nvim-cmp setup
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local lspkind = require("lspkind")
      local icons = require("vitallium.icons").kind
      lspkind.init({
        mode = "symbol_text",
        symbol_map = {
          Class = icons.Class,
          Color = icons.Color,
          Constant = icons.Constant,
          Constructor = icons.Constructor,
          Enum = icons.Enum,
          EnumMember = icons.Enum,
          Event = icons.Event,
          Field = icons.Field,
          File = icons.File,
          Folder = icons.Folder,
          Function = icons.Function,
          Interface = icons.Interface,
          Keyword = icons.Keyword,
          Method = icons.Method,
          Module = icons.Module,
          Operator = icons.Operator,
          Property = icons.Property,
          Reference = icons.Reference,
          Snippet = icons.Snippet,
          Struct = icons.Struct,
          Text = icons.Text,
          TypeParameter = icons.TypeParameter,
          Unit = icons.Unit,
          Value = icons.Value,
          Variable = icons.Variable,
        },
      })
      cmp.setup({
        snippet = {
          expand = function(args) luasnip.lsp_expand(args.body) end,
        },
        window = {
          -- Style completion window to have icons on the left.
          -- In combination with `formatting` below.
          completion = {
            col_offset = -3,
            side_padding = 0,
          },
          documentation = cmp.config.window.bordered(),
        },
        formatting = {
          fields = { "kind", "abbr", "menu" },
          format = function(entry, vim_item)
            local kind = lspkind.cmp_format({ maxwidth = 50 })(entry, vim_item)

            local strings = vim.split(kind.kind, "%s", { trimempty = true })
            kind.kind = " " .. strings[1] .. " "
            kind.menu = "    (" .. strings[2] .. ")"

            return kind
          end,
        },
        mapping = cmp.mapping.preset.insert({
          -- Use <C-j/k> to select candidates:
          ["<C-j>"] = cmp.mapping(function() cmp.select_next_item() end),
          ["<C-k>"] = cmp.mapping(function() cmp.select_prev_item() end),
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
