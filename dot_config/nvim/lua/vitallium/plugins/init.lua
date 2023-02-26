return {
	-- Plugin with util functions required by other plugins
	"nvim-lua/plenary.nvim",
	-- EditorConfig
	"gpanders/editorconfig.nvim",
	-- [[ Theming ]]
	{
		"lewis6991/gitsigns.nvim", -- Git gutter
		event = { "BufReadPost", "BufNewFile" },
		config = function(_, opts)
			require("gitsigns").setup(opts)
			-- This is for diagnostic signs on the line number column.
			-- Use this to beautify the plain E W signs.
			local signs = require("vitallium.icons").diagnostics
			for type, icon in pairs(signs) do
				local hl = "DiagnosticSign" .. type
				vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
			end
		end,
	},

	{
		"folke/todo-comments.nvim", -- Highlight and list TODOs, etc.
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			highlight = {
				pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlightng (vim regex)
				-- Was [[.*<(KEYWORDS)\s*:]] including colon.
			},
			search = {
				pattern = [[\b(KEYWORDS)]], -- ripgrep regex
				-- Was [[\b(KEYWORDS):]] including colon.
			},
		},
	},
	{
		"norcalli/nvim-colorizer.lua", -- Show color-codes colored
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
	{
		"danilamihailov/beacon.nvim", -- Highlight cursor on jump
		event = { "BufReadPost", "BufNewFile" },
	},

	-- [[ Interface ]]
	{
		-- Switch windows/panes vim/tmux
		"christoomey/vim-tmux-navigator",
		event = { "VeryLazy" },
	},
	{
		-- Like Emacs which key
		"folke/which-key.nvim",
		event = { "VeryLazy" },
		config = true,
	},
	{
		-- Alt-h/j/k/l to move line
		"booperlv/nvim-gomove",
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
	-- Keep windows around when deleting buffers
	"famiu/bufdelete.nvim",
	{
		-- Auto-pair tags, etc.
		"windwp/nvim-autopairs",
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
}
