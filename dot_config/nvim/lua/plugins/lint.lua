return {
  "mfussenegger/nvim-lint",
  event = { "VeryLazy" },
  config = function()
    local lint = require("lint")

    lint.linters_by_ft = {
      bash = { "shellcheck" },
      fish = { "fish" },
      ghaction = { "actionlint" },
      gitcommit = { "gitlint", "write_good" },
      go = { "revive" },
      htmldjango = { "curlylint" },
      jinja = { "curlylint" },
      markdown = { "markdownlint", "write_good" },
      protobuf = { "protolint" },
      ruby = { "rubocop " },
      rst = { "rstcheck", "write_good" },
      sh = { "shellcheck" },
      text = { "write_good" },
      yaml = { "yamllint" },
    }
  end,
}
