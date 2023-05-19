return {
  "AckslD/messages.nvim",
  cmd = "Messages",
  keys = {
    {
      "<leader>om",
      function()
        vim.cmd.Messages()
      end,
      desc = "Open messages window.",
    },
    {
      "<leader>ol",
      function()
        require("messages.api").capture_thing(vim.lsp)
      end,
      desc = "Open LSP Debug window.",
    },
  },
  opts = {
    border = vim.g.border,
    post_open_float = function(winnr)
      vim.keymap.set("n", "q", vim.cmd.close, { buffer = vim.api.nvim_win_get_buf(winnr), silent = true })
    end,
  },
}
