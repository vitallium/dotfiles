return {
  "linty-org/key-menu.nvim",
  enabled = false,
  config = function()
    local keymenu = require("key-menu")

    -- Create labels for key-menu.
    for _, menu in pairs({
      { key = "g" },
      { key = "<leader>" },
      { key = "<space>" },
      { key = "]", opts = { desc = " 󰒭Next ..." } },
      { key = "[", opts = { desc = " 󰒮Previous ..." } },
      { key = "<leader>b", opts = { desc = " Buffers" } },
      { key = "<leader>c", opts = { desc = " Code" }, mode = { "n", "t" } },
      { key = "<leader>f", opts = { desc = " Find" } },
      { key = "<leader>g", opts = { desc = " Git" } },
      { key = "<leader>l", opts = { desc = " LSP" } },
      { key = "<leader>m", opts = { desc = " Map View" } },
      { key = "<leader>n", opts = { desc = " Notifications" } },
      { key = "<leader>p", opts = { desc = " Plugins" } },
      { key = "<leader>q", opts = { desc = "󰗼 Quit" } },
      { key = "<leader>r", opts = { desc = " Refactor" }, mode = { "n", "t" } },
      { key = "<leader>t", opts = { desc = " Test" } },
      { key = "<leader>v", opts = { desc = " View" } },
      { key = "<leader>x", opts = { desc = " Diagnostics" } },
    }) do
      keymenu.set(menu.mode or "n", menu.key, menu.opts or {})
    end
  end,
  event = "CursorMoved",
}
