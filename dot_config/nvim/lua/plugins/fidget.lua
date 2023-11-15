return {
  -- Spinner to show when LSP Server starts
  "j-hui/fidget.nvim",
  event = "LspAttach",
  opts = {
    text = { spinner = "dots" },
  },
}
