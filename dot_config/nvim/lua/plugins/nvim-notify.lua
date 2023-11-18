return {
  "rcarriga/nvim-notify",
  config = function()
    require("notify").setup({
      background_colour = "#2e3440",
      fps = 60,
      max_height = function()
        return math.floor(vim.o.lines * 0.75)
      end,
      max_width = function()
        return math.floor(vim.o.columns * 0.75)
      end,
      minimum_width = 50,
      timeout = 3000,
    })

    -- Set as the default notify function.
    vim.notify = require("notify")
  end,
  keys = {
    {
      "<leader>nd",
      function()
        require("notify").dismiss({ silent = true, pending = true })
      end,
      desc = "Delete all Notifications",
    },
  },
  init = function()
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.notify = function(...)
      require("lazy").load({ plugins = { "nvim-notify" } })
      vim.notify(...)
    end
  end,
}
