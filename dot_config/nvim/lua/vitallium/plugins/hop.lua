return {
  "phaazon/hop.nvim", -- Snipe words/letters on screen
  branch = "v2", -- optional but strongly recommended
  event = "VeryLazy",
  config = function()
    require("hop").setup({ keys = "asdfghjkl;qwerpoiuzxcv.,mn" })

    local wk = require("which-key")
    wk.register({
      ["<leader>s"] = { ":HopWord<CR>", "Move cursor to word" },
    })
  end,
}
