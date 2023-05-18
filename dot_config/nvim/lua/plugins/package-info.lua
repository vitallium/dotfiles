return {
  "vuki656/package-info.nvim",
  dependencies = { "MunifTanjim/nui.nvim" },
  event = "BufRead package.json",
  config = function()
    local package = require("package-info")

    package.setup()

    vim.keymap.set("n", "<localleader>pu", package.update, { desc = "Package: Update package on line" })
    vim.keymap.set("n", "<localleader>pd", package.delete, { desc = "Package: Delete package on line" })
    vim.keymap.set("n", "<localleader>pi", package.install, { desc = "Package: Install new package" })
    vim.keymap.set(
      "n",
      "<localleader>pv",
      package.change_version,
      { desc = "Package: Change version of package on line" }
    )
  end,
}
