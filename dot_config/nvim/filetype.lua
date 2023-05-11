-- Custom filetype detection logic with the new Lua filetype plugin
vim.filetype.add({
  extension = {
    png = "image",
    jpg = "image",
    jpeg = "image",
    gif = "image",
  },
  filename = {
    [".eslintrc"] = "json",
    [".prettierrc"] = "json",
    [".babelrc"] = "json",
    [".stylelintrc"] = "json",
  },
  pattern = {
    [".*config/git/config"] = "gitconfig",
    [".*%.conf"] = "conf",
    [".*%.theme"] = "conf",
    [".*ignore"] = "conf",
    ["default-*%-packages"] = "conf",
    [".*%.env%..*"] = "env",
    [".*%.prettierrc%..*"] = "jsonc",
    [".*%.eslintrc%..*"] = "jsonc",
  },
})
