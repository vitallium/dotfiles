return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "antoinemadec/FixCursorHold.nvim",
      "haydenmeade/neotest-jest",
      "olimorris/neotest-rspec",
      "nvim-neotest/neotest-go",
      "rouge8/neotest-rust",
    },
    opts = {
      status = { virtual_text = true },
      output = { open_on_run = true },
      quickfix = {
        open = function()
          vim.cmd("Trouble quickfix")
        end,
      },
    },
    config = function(_, opts)
      local neotest_ns = vim.api.nvim_create_namespace("neotest")
      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)

      opts.adapters = {
        require("neotest-rspec"),
        require("neotest-jest"),
        require("neotest-go")({
          experimental = {
            test_table = true,
          },
          args = { "-count=1", "-timeout=60s", "-v", "-race" },
        }),
        require("neotest-rust"),
      }

      require("neotest").setup(opts)
    end,
    keys = {
      {
        "<localleader>tf",
        function()
          require("neotest").run.run(vim.fn.expand("%s"))
        end,
        desc = "Test file",
      },
      {
        "<localleader>ts",
        function()
          require("neotest").run.run(vim.fn.getcwd())
        end,
        desc = "Test suite",
      },
      {
        "<localleader>to",
        function()
          require("neotest").output_panel.toggle()
        end,
        desc = "Toggle test output panel",
      },
      {
        "<localleader>tt",
        function()
          require("neotest").run.run()
        end,
        desc = "Run the nearest test",
      },
      {
        "<localleader>tx",
        function()
          require("neotest").run.stop()
        end,
        desc = "Stop the test",
      },
      {
        "<localleader>tv",
        function()
          require("neotest").summary.toggle()
        end,
        desc = "Toggle summary",
      },
    },
  },
}
