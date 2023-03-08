return {
  "echasnovski/mini.nvim",
  version = false,
  event = "VeryLazy",
  config = function()
    -- [[ai]]
    local spec_treesitter = require("mini.ai").gen_spec.treesitter
    require("mini.ai").setup({ -- Better selection inside/around things.
      custom_textobjects = {
        c = spec_treesitter({ a = "@class.outer", i = "@class.inner" }),
        f = spec_treesitter({ a = "@function.outer", i = "@function.inner" }),
        o = spec_treesitter({
          a = { "@conditional.outer", "@loop.outer" },
          i = { "@conditional.inner", "@loop.inner" },
        }),
      },
    })

    -- [[comment]]
    require("mini.comment").setup({ -- Easier (un)commenting.
      hooks = {
        pre = function()
          require("ts_context_commentstring.internal").update_commentstring()
        end,
      },
    })

    -- [[surround]]
    require("mini.surround").setup({ -- Manipulate "surrounding" characters.
      mappings = {
        replace = "sc",
      },
    })
  end,
}
