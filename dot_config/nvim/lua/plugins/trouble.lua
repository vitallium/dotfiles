return {
  "folke/trouble.nvim", -- Better looking quicklist, diagnostics, etc.
  cmd = { "TroubleToggle", "Trouble" },
  keys = {
    {
      "<leader>xx",
      function()
        vim.cmd.TroubleToggle()
      end,
      desc = "Trouble",
    },
  },
  opts = {
    use_diagnostic_signs = true,
    icons = false,
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
