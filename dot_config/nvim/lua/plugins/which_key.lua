return {
  "folke/which-key.nvim",
  -- "VeryLazy" hides splash screen
  lazy = false,
  config = function()
    -- If we do not wish to wait for timeoutlen
    vim.keymap.set("v", "<Leader>?", "<Esc>:WhichKey '' v<CR>", { silent = true })
    vim.keymap.set("n", "<Leader>?", "<Esc>:WhichKey '' n<CR>", { silent = true, desc = "which-key root" })

    -- https://github.com/folke/which-key.nvim#colors
    vim.cmd([[highlight default link WhichKey          Label]])
    vim.cmd([[highlight default link WhichKeySeperator String]])
    vim.cmd([[highlight default link WhichKeyGroup     Include]])
    vim.cmd([[highlight default link WhichKeyDesc      Function]])
    vim.cmd([[highlight default link WhichKeyFloat     CursorLine]])
    vim.cmd([[highlight default link WhichKeyValue     Comment]])

    local wk = require("which-key")

    wk.setup({
      plugins = {
        marks = true, -- shows a list of your marks on ' and `
        registers = false, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        -- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        spelling = {
          enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
          suggestions = 20, -- how many suggestions should be shown in the list?
        },
        presets = {
          operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
          motions = false, -- adds help for motions
          text_objects = false, -- help for text objects triggered after entering an operator
          windows = false, -- default bindings on <c-w>
          nav = true, -- misc bindings to work with windows
          z = true, -- bindings for folds, spelling and others prefixed with z
          g = true, -- bindings for prefixed with g
        },
      },
      -- add operators that will trigger motion and text object completion
      -- to enable all native operators, set the preset / operators plugin above
      operators = { gc = "Comments" },
      key_labels = {
        -- override the label used to display some keys. It doesn't effect WK in any other way.
        -- For example:
        ["<space>"] = "SPC",
        ["<cr>"] = "RET",
        ["<tab>"] = "TAB",
      },
      icons = {
        breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
        separator = "➜", -- symbol used between a key and it's label
        group = "+", -- symbol prepended to a group
      },
      window = {
        border = "none", -- none, single, double, shadow
        position = "bottom", -- bottom, top
        margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
        padding = { 1, 1, 1, 1 }, -- extra window padding [top, right, bottom, left]
      },
      layout = {
        height = { min = 4, max = 25 }, -- min and max height of the columns
        width = { min = 20, max = 50 }, -- min and max width of the columns
        spacing = 5, -- spacing between columns
      },
      -- hide mapping boilerplate
      hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " },
      show_help = true, -- show help message on the command line when the popup is visible
      triggers = "auto", -- automatically setup triggers
    })

    -- Without prefix:
    wk.register({
      ["<c-w>"] = {
        name = "Windows",
        s = { ":split<CR>", "Split window" },
        v = { ":vsplit<CR>", "Split window vertically" },
      },
    })

    -- With leader prefix:
    wk.register({
      b = {
        name = "Buffer",
        n = { ":bn<CR>", "Next buffer" },
        p = { ":bp<CR>", "Previous buffer" },
      },
      c = {
        name = "Code",
        d = { ":TroubleToggle document_diagnostics<CR>", "Show diagnostics" },
        D = {
          ":TroubleToggle workspace_diagnostics<CR>",
          "Show workspace diagnostics",
        },
        e = { vim.diagnostic.open_float, "Floating diagnostic" },
      },
      f = {
        name = "File",
        o = { ":Other<CR>", "Open other file" },
        O = { ":OtherVSplit<CR>", "Open other file in split" },
        s = { ":w<CR>", "Save" },
        S = { ":wa<CR>", "Save all" },
      },
      g = {
        name = "Git",
        G = { ":LazyGit<CR>", "LazyGit" },
        d = { ":DiffviewOpen<CR>", "Diff view" },
        L = { ":DiffviewFileHistory -f %<CR>", "File history" },
      },
      h = {
        name = "Help",
      },
      t = {
        name = "Toggle",
        c = { ":set nolist!<CR>", "Toggle invisible chars" },
        w = { ":set wrap!<CR>", "Toggle word wrap" },
      },
      l = {
        name = "Launch",
      },
      o = {
        name = "Open",
      },
      p = {
        name = "Project",
      },
      s = {
        name = "Search",
      },
      w = {
        name = "Window",
        c = { ":close<CR>", "Close window" },
        d = { ":close<CR>", "Close window" },
        s = { ":split<CR>", "Split window" },
        v = { ":vsplit<CR>", "Vertically split window" },
        h = { ":wincmd h<CR>", "Go to window to the left" },
        j = { ":wincmd j<CR>", "Go to window to the bottom" },
        k = { ":wincmd k<CR>", "Go to window to the top" },
        l = { ":wincmd l<CR>", "Go to window to the right" },
      },
      x = {
        name = "Diagnostic",
        s = { vim.diagnostic.open_float, "Show in float" },
      },
      q = { name = "Quit", q = { ":quitall<CR>", "Quit all" } },
    }, { prefix = "<leader>" })

    -- With local predix:
    wk.register({
      t = {
        name = "Test",
      },
      p = {
        name = "Packages",
      },
    }, { prefix = "<localleader>" })
  end,
}
