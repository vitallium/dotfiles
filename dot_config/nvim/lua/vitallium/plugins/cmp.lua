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
      "hrsh7th/cmp-emoji",
      "hrsh7th/cmp-calc",
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
        "tzachar/cmp-tabnine", -- TabNine
        build = "./install.sh",
      },
      -- working with neovim config/plugins
      "folke/neodev.nvim",
    },
    config = function()
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
        sources = cmp.config.sources({
          { name = "nvim_lsp_signature_help" },
          { name = "nvim_lsp" },
          { name = "cmp_tabnine" },
          { name = "luasnip" },
        }, {
          { name = "path" },
          { name = "buffer" },
          { name = "emoji" },
          { name = "calc" },
        }),
        confirm_opts = {
          behavior = cmp.ConfirmBehavior.Select,
        },
        experimental = {
          native_menu = false,
          ghost_text = false,
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
