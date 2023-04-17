-- Neotest (https://github.com/nvim-neotest/neotest)
--  An extensible framework for interacting with tests within NeoVim.

---@format disable-next
local keys = {
  {
    "<localleader>tt",
    function()
      require("neotest").run.run()
    end,
    desc = "Run the nearest test",
  },
  {
    "<localleader>td",
    function()
      require("neotest").run.run({ strategy = "dap" })
    end,
    desc = "Debug the nearest test",
  },
  {
    "<localleader>tf",
    function()
      require("neotest").run.run(vim.fn.expand("%"))
    end,
    desc = "Run the current file",
  },
  {
    "<localleader>tl",
    function()
      require("neotest").run.run_last()
    end,
    desc = "Repeat last test run",
  },
  {
    "<localleader>tr",
    function()
      require("neotest").summary.open()
    end,
    desc = "Open test summary",
  },
  {
    "<localleader>rt",
    function()
      require("neotest").output.open({ enter = true })
    end,
    desc = "Open test output",
  },
}

return {
  "nvim-neotest/neotest",
  ft = { "go", "ruby" },
  keys = keys,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    "antoinemadec/FixCursorHold.nvim",

    "nvim-neotest/neotest-go",
    "olimorris/neotest-rspec",
    "haydenmeade/neotest-jest",
  },
  config = function()
    -- get neotest namespace (api call creates or returns namespace)
    local neotest_ns = vim.api.nvim_create_namespace("neotest")
    vim.diagnostic.config({
      virtual_text = {
        format = function(diagnostic)
          local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
          return message
        end,
      },
    }, neotest_ns)

    local neotest = require("neotest")
    neotest.setup({
      adapters = {
        require("neotest-go")({
          experimental = {
            test_table = true,
          },
          args = { "-count=1", "-timeout=60s" },
        }),
        require("neotest-rspec")({
          rspec_cmd = function()
            return vim.tbl_flatten({
              "bundle",
              "exec",
              "rspec",
            })
          end,
        }),
      },
    })
  end,
}
