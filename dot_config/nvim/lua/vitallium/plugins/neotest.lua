return {
  "nvim-neotest/neotest",
  event = { "BufReadPost", "BufNewFile" },
  dependencies = {
    "antoinemadec/FixCursorHold.nvim",
    -- adapters
    "haydenmeade/neotest-jest",
    "nvim-neotest/neotest-go",
  },
  opts = function()
    return {
      adapters = {
        require("neotest-go"),
        require("neotest-jest"),
      },
      icons = {
        expanded = "┐",
        final_child_prefix = "└",
        failed = "",
        passed = "",
        running = "",
      },
    }
  end,
  config = function(_, opts)
    local neotest = require("neotest")
    neotest.setup(opts)

    vim.diagnostic.config({
      signs = true,
      virtual_text = true,
    }, vim.api.nvim_create_namespace("neotest"))
  end,
  keys = {
    {
      "<localleader>tt",
      function()
        require("neotest").run.run()
      end,
      desc = "run nearest test",
    },
    {
      "<localleader>tf",
      function()
        require("neotest").run.run(vim.fn.expand("%"))
      end,
      desc = "test file",
    },
    {
      "<localleader>td",
      function()
        require("neotest").run.run({ strategy = "dap" })
      end,
      desc = "debug nearest test",
    },
    {
      "<localleader>tk",
      function()
        require("neotest").run.stop()
      end,
      desc = "stop test",
    },
    {
      "<localleader>ta",
      function()
        require("neotest").run.attach()
      end,
      desc = "attach to nearest test",
    },
    {
      "<localleader>tl",
      function()
        require("neotest").run.run_last()
      end,
      desc = "run last test",
    },
    {
      "<localleader>ts",
      function()
        require("neotest").summary.toggle()
      end,
      desc = "show test summary",
    },
    {
      "<localleader>to",
      function()
        require("neotest").output.open()
      end,
      desc = "show test output",
    },
    {
      "<localleader>tw",
      function()
        require("neotest").output.open({
          open_win = function()
            vim.cmd("bel split")
          end,
        })
      end,
      desc = "show test window",
    },
  },
}
