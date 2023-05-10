return {
  "vim-test/vim-test",
  event = "BufEnter",
  cmd = { "TestNearest", "TestFile" },
  keys = {
    { "<localleader>ts", ":TestNearest -v<CR>g", noremap = true, silent = true, desc = "Test Nearest" },
    { "<localleader>tv", ":TestFile -v<CR>g", noremap = true, silent = true, desc = "Test File" },
  },
  config = function()
    vim.api.nvim_set_var("test#strategy", "neovim")
    vim.api.nvim_set_var("test#neovim#term_position", "vert")
    vim.api.nvim_set_var("test#ruby#use_binstubs", "1")
  end,
}
