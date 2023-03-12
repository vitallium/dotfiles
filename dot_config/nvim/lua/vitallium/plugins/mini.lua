return {
  "echasnovski/mini.nvim",
  version = false,
  event = "VeryLazy",
  config = function()
    -- [[ai]]
    local spec_treesitter = require("mini.ai").gen_spec.treesitter
    require("mini.ai").setup({
      -- Better selection inside/around things.
      custom_textobjects = {
        c = spec_treesitter({ a = "@class.outer", i = "@class.inner" }),
        f = spec_treesitter({ a = "@function.outer", i = "@function.inner" }),
        o = spec_treesitter({
          a = { "@conditional.outer", "@loop.outer" },
          i = { "@conditional.inner", "@loop.inner" },
        }),
      },
    })

    -- [[sane defaults]]
    require("mini.basics").setup({
      options = {
        basic = true,
        extra_ui = true,
      },
      autocommands = {
        -- When enabled, Neogit starts in insert mode due to background terminal
        -- See also https://github.com/TimUntersberger/neogit/issues/426#issuecomment-1374832606
        basic = false,
      },
    })

    -- [[next/previous]]
    require("mini.bracketed").setup({
      -- Disable some mappings:
      location = { suffix = "", options = {} },
      quickfix = { suffix = "", options = {} },
    })

    -- [[comment]]
    require("mini.comment").setup({
      -- Easier (un)commenting.
      hooks = {
        pre = function()
          require("ts_context_commentstring.internal").update_commentstring()
        end,
      },
    })

    -- [[jumping]]
    -- Use f/F/t/T again to continue jumping
    require("mini.jump").setup()

    -- [[surround]]
    require("mini.surround").setup({
      -- Manipulate "surrounding" characters.
      mappings = {
        replace = "sc",
      },
    })

    -- [[highlight trailing spaces]]
    require("mini.trailspace").setup()
  end,
}
