vim.keymap.set("n", "<leader>ce", function()
  require("rust-tools").open_cargo_toml.open_cargo_toml()
end, { desc = "Open Cargo.toml" })
