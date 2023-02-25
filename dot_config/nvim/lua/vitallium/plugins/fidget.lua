return {
  -- Spinner to show when LSP Server starts
  "j-hui/fidget.nvim",
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    text = {
      spinner = "arc",
    },
    window = {
      blend = 0,
    },
  },
}
