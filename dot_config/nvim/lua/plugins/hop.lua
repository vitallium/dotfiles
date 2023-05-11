return {
  "phaazon/hop.nvim", -- Snipe words/letters on screen
  branch = "v2", -- optional but strongly recommended
  event = { "BufReadPost", "BufNewFile" },
  keys = {
    { "<leader>gw", ":HopWord<CR>", desc = "Move cursor to word" },
  },
  opts = {
    keys = "asdfghjkl;qwerpoiuzxcv.,mn",
  },
  config = true,
}
