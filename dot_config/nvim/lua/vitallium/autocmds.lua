local group = vim.api.nvim_create_augroup("Setup", {})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = group,
  callback = function() vim.highlight.on_yank({ higroup = "IncSearch", timeout = 100 }) end,
  desc = "Highlights the yanked text",
})

-- Hide cursorline in insert mode
vim.api.nvim_create_autocmd({ "InsertLeave", "WinEnter" }, { command = "set cursorline", group = group })
vim.api.nvim_create_autocmd({ "InsertEnter", "WinLeave" }, { command = "set nocursorline", group = group })

-- Automatically close Vim if the quickfix window is the only one open
vim.api.nvim_create_autocmd("WinEnter", {
  group = group,
  callback = function()
    if vim.fn.winnr("$") == 1 and vim.fn.win_gettype() == "quickfix" then vim.cmd.q() end
  end,
})
