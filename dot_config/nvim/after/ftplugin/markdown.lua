vim.opt_local.spell = true -- Enable spellchecking
vim.opt_local.spelllang = { "en_us" }

-- Automatically wrap at 100 characters
vim.opt_local.textwidth = 120
vim.opt_local.colorcolumn = "120"

-- allow syntax highlight inside code blocks for these languages
vim.g.markdown_fenced_languages = {
  "vim",
  "python",
  "javascript",
  "js=javascript",
  "jsx=javascriptreact",
  "typescript",
  "ts=typescript",
  "tsx=typescriptreact",
}
