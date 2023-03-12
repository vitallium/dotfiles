return {
  "rcarriga/nvim-dap-ui",
  dependencies = {
    "mfussenegger/nvim-dap",
    { "rcarriga/nvim-dap-ui", config = true },
    { "leoluz/nvim-dap-go", config = true },
    {
      "theHamsta/nvim-dap-virtual-text",
      opts = {
        commented = true,
      },
    },
  },
  keys = {
    {
      "<leader>dd",
      function()
        require("dapui").toggle()
      end,
      desc = "debug: toggle UI",
    },
    {
      "<leader>dk",
      function()
        require("dap").continue()
      end,
      desc = "debug: continue",
    },
    {
      "<leader>dj",
      function()
        require("dap").step_over()
      end,
      desc = "debug: step over",
    },
    {
      "<leader>dl",
      function()
        require("dap").step_into()
      end,
      desc = "debug: step into",
    },
    {
      "<leader>dh",
      function()
        require("dap").step_out()
      end,
      desc = "debug: step out",
    },
    {
      "<leader>dl",
      function()
        require("dap").run_last()
      end,
      desc = "debug: run last",
    },
    {
      "<leader>db",
      function()
        require("dap").toggle_breakpoint()
      end,
      desc = "debug: toggle breakpoint",
    },
    {
      "<leader>dt",
      function()
        require("dap").terminate()
      end,
      desc = "debug: terminate session",
    },
  },
}
