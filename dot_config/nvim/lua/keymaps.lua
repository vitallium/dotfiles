local opts = { noremap = true, silent = true }
-- Shorten function name
local keymap = vim.keymap.set

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Normal --
-- disable Ex mode, I always enter in it by mistake
keymap("n", "Q", "<Nop>", opts)

-- move record macro to Q instead of q
keymap("n", "Q", "q", opts)
keymap("n", "q", "<Nop>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
