return {
  -- Spinner to show when LSP Server starts
  "j-hui/fidget.nvim",
  tag = "legacy",
  event = "LspAttach",
  opts = {
    sources = {
      ["null-ls"] = { ignore = true },
    },
    text = { spinner = "dots" },
  },
}
