return {
  "stevearc/overseer.nvim",
  opts = { strategy = "toggleterm", task_list = { direction = "right" } },
  keys = {
    { "<leader>lor", "<cmd>OverseerRun<cr>", desc = "Run task" },
    { "<leader>lol", "<cmd>OverseerToggle<cr>", desc = "Toggle tasklist" },
  },
}
