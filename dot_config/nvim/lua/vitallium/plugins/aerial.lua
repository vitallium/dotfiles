return {
  "stevearc/aerial.nvim", -- Code file overview
  keys = {
    { "<leader>co", ":AerialToggle<CR>", desc = "Toggle overview" },
  },
  opts = {
    -- Priority list of preferred backends for aerial.
    -- This can be a filetype map (see :help aerial-filetype-map)
    -- Current setup is not ideal. Neither treesitter nor lsp give good info
    -- for component files with lots of `computed`, for example.
    backends = { "treesitter", "lsp", "markdown", "man" },
    icons = {
      Collapsed = require("vitallium.icons").ui.ChevronShortRight,
    },
    -- Keymaps in aerial window. Can be any value that `vim.keymap.set` accepts OR a table of keymap
    -- options with a `callback` (e.g. { callback = function() ... end, desc = "", nowait = true })
    -- Additionally, if it is a string that matches "actions.<name>",
    -- it will use the mapping at require("aerial.actions").<name>
    -- Set to `false` to remove a keymap
    keymaps = {
      ["<TAB>"] = "actions.scroll",
      ["o"] = {
        callback = function()
          require("aerial").select()
          require("aerial").close()
        end,
        desc = "Jump and quit",
        nowait = true,
      },
    },
  },
}
