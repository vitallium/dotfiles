return {
  "folke/trouble.nvim", -- Better looking quicklist, diagnostics, etc.
  cmd = { "TroubleToggle", "Trouble" },
  opts = {
    use_diagnostic_signs = true,
  },
  config = function(_, opts)
    require("trouble").setup(opts)
    -- Disable width ruler in trouble window:
    vim.api.nvim_create_autocmd("Filetype", {
      pattern = { "Trouble" },
      command = "set colorcolumn=0",
    })
  end,
}
