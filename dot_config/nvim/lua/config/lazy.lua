-- lazy.nvim
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

require("lazy").setup({
  change_detection = {
    enabled = true,
    notify = false,
  },
  spec = {
    "nvim-lua/plenary.nvim",
    { import = "plugins.ui" },
    { import = "plugins.fzf" },
    { import = "plugins.treesitter" },
    { import = "plugins.git" },
    { import = "plugins.completion" },
    { import = "plugins.test" },
    { import = "plugins.formatting" },
    { import = "plugins.diagnostics" },
    { import = "plugins.lsp.init" },
  },
})
