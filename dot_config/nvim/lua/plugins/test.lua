return {
  {
    "vim-test/vim-test",
    cmd = { "TestNearest", "TestFile" },
    keys = {
      { "<localleader>ts", ":TestNearest<CR>", noremap = true, silent = true, desc = "Test Nearest" },
      { "<localleader>tv", ":TestFile<CR>", noremap = true, silent = true, desc = "Test File" },
    },
    config = function()
      vim.api.nvim_set_var("test#strategy", "neovim")
      vim.api.nvim_set_var("test#neovim#term_position", "hor")
      vim.api.nvim_set_var("test#ruby#use_binstubs", "1")
    end,
  },
}
