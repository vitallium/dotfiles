return {
  {
    "nvim-neotest/neotest",
    lazy = true,
    dependencies = {
      "olimorris/neotest-rspec",
      "zidhuss/neotest-minitest",
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-rspec"),
          require("neotest-minitest"),
        },
      })
    end,
  },
}
