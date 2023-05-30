-- [[ bootstrap package manager ]]
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  print("Downloading folke/lazy.nvim...")
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--single-branch",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
end
vim.opt.runtimepath:prepend(lazypath)

local ok, lazy = pcall(require, "lazy")
if not ok then
  return
end

lazy.setup("plugins", {
  change_detection = {
    enabled = false,
    notify = false,
  },
  checker = {
    enabled = true,
    notify = false,
  },
  defaults = {
    lazy = true,
  },
  install = { colorscheme = { "tokyonight" }, missing = true },
  performance = {
    rtp = {
      disabled_plugins = {
        "2html_plugin",
        "getscript",
        "getscriptPlugin",
        "gzip",
        "health",
        "logipat",
        "man",
        "matchit",
        "matchparen",
        "rplugin",
        "rrhelper",
        "spellfile",
        "spellfile_plugin",
        "tar",
        "tarPlugin",
        "tohtml",
        "tutor",
        "vimball",
        "vimballPlugin",
        "zip",
        "zipPlugin",
        "nvim-treesitter-textobjects",
        "plenary",
      },
    },
  },
  ui = {
    border = vim.g.border,
  },
})

vim.keymap.set("n", "<leader>pi", require("lazy").show, { desc = "Plugin Info" })
vim.keymap.set("n", "<leader>pp", require("lazy").profile, { desc = "Profile Plugins" })
vim.keymap.set("n", "<leader>ps", require("lazy").sync, { desc = "Sync Plugins" })
