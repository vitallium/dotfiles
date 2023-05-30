if vim.loader then
  vim.loader.enable()
end

require("options")
require("autocmds")
require("keymaps")

require("lazyplug")

pcall(vim.cmd, [[colorscheme tokyonight]])
