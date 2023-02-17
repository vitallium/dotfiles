local status_ok_treesitter, treesitter = pcall(require, "nvim-treesitter.configs")
if not status_ok_treesitter then
  return
end

treesitter.setup {
  -- Add languages to be installed here that you want installed for treesitter
  ensure_installed = {
    'c',
    'cpp',
    'go',
    'lua',
    'python',
    'rust',
    'typescript',
    'help',
    'vim',
    'ruby',
    'yaml',
    'javascript'
  },

  highlight = { enable = true },
}

