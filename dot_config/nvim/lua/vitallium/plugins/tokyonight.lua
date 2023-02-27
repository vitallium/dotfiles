return {
	"folke/tokyonight.nvim",
	lazy = false,
	opts = {
		style = "storm",
	},
	config = function()
		vim.cmd("colorscheme tokyonight")
	end,
}
