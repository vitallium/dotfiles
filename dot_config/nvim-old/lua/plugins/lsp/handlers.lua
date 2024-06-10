local M = {}

M.on_attach = function(client, buffer)
  -- General diagnostics.
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next Diagnostic" })
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous Diagnostic" })
  vim.keymap.set("n", "<leader>xr", vim.diagnostic.reset, { desc = "Reset" })
  vim.keymap.set("n", "<leader>xs", vim.diagnostic.open_float, { desc = "Show" })

  if client.server_capabilities.signatureHelpProvider then
    vim.keymap.set("n", "<leader>ch", vim.lsp.buf.signature_help, { desc = "Signature Help" })
    vim.keymap.set("i", "<C-k>", vim.lsp.buf.signature_help, { desc = "Signature Help" })
  end

  if client.server_capabilities.declarationProvider then
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "Go To Declaration" })
  end

  if client.server_capabilities.codeActionProvider then
    vim.keymap.set({ "n", "x" }, "<leader>ca", require("actions-preview").code_actions, { desc = "Actions" })
  end

  -- https://github.com/smjonas/inc-rename.nvim
  if client.server_capabilities.renameProvider then
    vim.keymap.set("n", "<leader>cr", function()
      --
      return ":" .. require("inc_rename").config.cmd_name .. " " .. vim.fn.expand("<cword>")
    end, { desc = "Rename", expr = true })
  end

  if client.server_capabilities.codeLensProvider then
    vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "CursorHoldI", "InsertLeave" }, {
      desc = "LSP Code Lens Refresh",
      buffer = buffer,
      callback = vim.lsp.codelens.refresh,
    })
  end

  if client.server_capabilities.documentSymbolProvider then
    require("nvim-navic").attach(client, buffer)
  end

  if client.server_capabilities.definitionProvider then
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Go To Definition(s)" })
  end

  if client.server_capabilities.implementationProvider then
    vim.keymap.set("n", "gi", function()
      require("fzf-lua").lsp_implementations()
    end, { desc = "Go To Implementations(s)" })
  end
end

return M
