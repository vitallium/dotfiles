if vim.loader then
  vim.loader.enable()
end

require("options")
require("autocmds")
require("keymaps")

require("lazyplug")

vim.cmd.colorscheme("base16-tomorrow-night")
