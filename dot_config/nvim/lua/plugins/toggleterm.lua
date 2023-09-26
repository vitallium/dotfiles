return {
  "akinsho/toggleterm.nvim", -- Toggle terminals like Doom Emacs
  version = "*",
  cmd = { "ToggleTerm" },
  keys = {
    { "<leader>ot", ":ToggleTerm<CR>", desc = "Toggle Term" },
  },
  opts = {
    direction = "horizontal",
    -- size can be a number or function which is passed the current terminal
    size = function(term)
      if term.direction == "horizontal" then
        return 15
      elseif term.direction == "vertical" then
        return vim.o.columns * 0.4
      end
    end,
    open_mapping = [[<C-\>]],
    start_in_insert = true,
    close_on_exit = false,
  },
}
