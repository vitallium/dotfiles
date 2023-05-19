local opts = { noremap = true, silent = true }
-- Shorten function name
local keymap = vim.keymap.set

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Normal --
-- Disable recording / annoying exmode.
-- https://stackoverflow.com/questions/1527784/what-is-vim-recording-and-how-can-it-be-disabled
keymap("n", "q", "<Nop>", { desc = "hidden" })
keymap("n", "Q", "<Nop>", { desc = "hidden" })
keymap("n", "q:", "<Nop>", { desc = "hidden" })

keymap("n", "]d", vim.diagnostic.goto_next, { desc = "Next Diagnostic" })
keymap("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous Diagnostic" })

-- Quitting / Sessions
keymap("n", "qq", vim.cmd.quitall, { desc = "Quit" })
keymap("n", "qw", vim.cmd.wqall, { desc = "Quit & Write All" })
keymap("n", "q!", function()
  vim.cmd.quitall("!")
end, { desc = "Quit without saving" })

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
