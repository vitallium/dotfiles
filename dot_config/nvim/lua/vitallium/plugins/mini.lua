return {
  "echasnovski/mini.nvim",
  version = false,
  event = "VeryLazy",
  keys = {
    {
      "<leader>bd",
      function()
        require("mini.bufremove").delete(0, true)
      end,
      desc = "Delete buffer",
    },
  },
  config = function()
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

    require("mini.bufremove").setup()
  end,
}
