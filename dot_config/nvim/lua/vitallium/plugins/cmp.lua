return {
  {
    "hrsh7th/nvim-cmp", -- Autocompletion plugin
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      "hrsh7th/cmp-buffer",   -- Buffer source for nvim-cmp
      "hrsh7th/cmp-path",     -- Path source for nvim-cmp
      "hrsh7th/cmp-nvim-lsp", -- LSP source for nvim-cmp
      "hrsh7th/cmp-nvim-lua", -- Lua Completions
      "hrsh7th/cmp-cmdline",  -- Command line source for nvim-cmp
      "hrsh7th/cmp-emoji",
      'ray-x/cmp-treesitter',
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
      {
        "tzachar/cmp-tabnine",    -- TabNine
        build = "./install.sh",
      },
    },
    config = function()
      -- nvim-cmp setup
      local cmp = require("cmp")

      cmp.setup({
        completion = {
          completeopt = "menu,menuone,noinsert",
        },
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete({}),
          ["<C-e>"] = cmp.mapping.close(),
          ["<CR>"] = cmp.mapping.confirm({
            select = true,
            behavior = cmp.ConfirmBehavior.Replace
          }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "cmp_tabnine" },
          { name = "luasnip" },
          { name = "treesitter" },
        }, {
          { name = "path" },
          { name = "emoji" },
        }),
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
    end,
  },
}
