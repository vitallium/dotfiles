-- Custom filetype detection logic with the new Lua filetype plugin
vim.filetype.add({
  extension = {
    png = "image",
    jpg = "image",
    jpeg = "image",
    gif = "image",
    es6 = "javascript",
    mts = "typescript",
    cts = "typescript",
  },
  filename = {
    [".eslintrc"] = "json",
    [".prettierrc"] = "json",
    [".babelrc"] = "json",
    [".stylelintrc"] = "json",
    ["NEOGIT_COMMIT_EDITMSG"] = "NeogitCommitMessage",
  },
  pattern = {
    [".*config/git/config"] = "gitconfig",
    [".*%.conf"] = "conf",
    [".*%.theme"] = "conf",
    [".*ignore"] = "conf",
    [".nvimrc"] = "lua",
    ["default-*%-packages"] = "conf",
    [".*%.gradle"] = "groovy",
    [".*%.env%..*"] = "env",
    [".*%.prettierrc%..*"] = "jsonc",
    [".*%.eslintrc%..*"] = "jsonc",
    [".*%.jst.eco"] = "jst",
    [".*%.html.en"] = "html",
  },
})
