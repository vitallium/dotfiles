-- Disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- [[ bootstrap package manager ]]
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- must be first to make mapping correct
vim.g.mapleader = " "
vim.g.localleader = ","

require("vitallium.options")
require("lazy").setup("vitallium.plugins", {
	checker = {
		enabled = true,
		notify = false,
	},
	performance = {
		cache = {
			enabled = true,
		},
	},
	change_detection = {
		notify = false,
	},
})
require("vitallium.keymaps")
