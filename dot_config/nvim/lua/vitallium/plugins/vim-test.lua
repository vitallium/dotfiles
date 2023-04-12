return {
  "vim-test/vim-test",
  event = "BufEnter",
  keys = {
    { "<leader>ttn", ":TestNearest -v<CR>g", noremap = true, silent = true, desc = "Test Nearest" },
    { "<leader>ttf", ":TestFile -v<CR>g", noremap = true, silent = true, desc = "Test File" },
    { "<leader>tts", ":TestSuite -v<CR>g", noremap = true, silent = true, desc = "Test Suite" },
    { "<leader>ttl", ":TestLast -v<CR>g", noremap = true, silent = true, desc = "Test Last" },
    { "<leader>ttv", ":TestVisit -v<CR>g", noremap = true, silent = true, desc = "Test Visit" },
  },
  config = function()
    vim.api.nvim_set_var("test#strategy", "neovim")
    vim.api.nvim_set_var("test#neovim#term_position", "vert")
  end,
}
