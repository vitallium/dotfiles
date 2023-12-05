return {
  "L3MON4D3/LuaSnip",
  dependencies = { "rafamadriz/friendly-snippets" },
  lazy = true,
  config = function()
    local luasnip = require("luasnip")
    local types = require("luasnip.util.types")

    -- documentation for snippet format inside examples:
    -- https://github.com/L3MON4D3/LuaSnip/blob/master/Examples/snippets.lua

    luasnip.config.set_config({
      history = true,
      -- Do not jump to snippet if I'm outside of it
      -- https://github.com/L3MON4D3/LuaSnip/issues/78
      region_check_events = "CursorMoved",
      delete_check_events = "TextChanged",
      enable_autosnippets = true,
      ext_opts = {
        [types.choiceNode] = {
          active = {
            virt_text = { { "", "Operator" } },
            hl_mode = "combine",
          },
        },
        [types.insertNode] = {
          active = {
            virt_text = { { "", "Type" } },
            hl_mode = "combine",
          },
        },
      },
      -- Use treesitter for getting the current filetype. This allows correctly resolving
      -- the current filetype in eg. a markdown-code block or `vim.cmd()`.
      ft_func = require("luasnip.extras.filetype_functions").from_cursor,
    })

    -- Snippets are stored in separate files.
    require("luasnip.loaders.from_lua").load({ paths = vim.api.nvim_get_runtime_file("lua/snippets", false)[1] })

    -- Load Rails snippets for ruby filetype
    luasnip.filetype_extend('ruby', { 'rails' })

    -- Load Ruby snippets for haml filetype
    luasnip.filetype_extend('haml', { 'ruby' })

    -- Create a command to edit the snippet file associated with the current
    vim.api.nvim_create_user_command("LuaSnipEdit", require("luasnip.loaders").edit_snippet_files, {})

    vim.api.nvim_create_autocmd("InsertLeave", {
      desc = "Clear luasnip on mode change.",
      callback = function()
        local ls = require("luasnip")

        if ls.in_snippet() then
          ls.unlink_current()
        end
      end,
    })

    -- Load "friendly-snippets" (dependency):
    require("luasnip.loaders.from_vscode").lazy_load()
    -- Extend filetypes:
    require("luasnip").filetype_extend("typescript", { "javascript", "jsdoc" })
  end,
}
