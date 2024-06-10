return {
  "kevinhwang91/nvim-ufo", -- Better folding
  name = "ufo",
  event = { "BufReadPost", "BufNewFile" },
  dependencies = { "kevinhwang91/promise-async" },
  priority = 1, -- Load after LSP, etc.
  opts = {
    close_fold_kinds_for_ft = { "imports" },
    provider_selector = function()
      return { "treesitter", "indent" }
    end,
  },
  config = function(_, opts)
    vim.o.foldcolumn = "0" -- '0' to hide or '1' to show
    -- Disable the fold column for certain filetypes:
    vim.api.nvim_create_autocmd("Filetype", {
      pattern = {
        "gitcommit",
        "NvimTree",
        "NeogitCommitMessage",
        "NeogitStatus",
        "Trouble",
      },
      command = "set foldcolumn=0",
    })

    vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
    vim.o.foldlevelstart = 99
    vim.o.foldenable = true

    -- Must be overridden in order for ufo to keep its folds
    vim.keymap.set("n", "zR", require("ufo").openFoldsExceptKinds)
    vim.keymap.set("n", "zM", require("ufo").closeAllFolds)

    require("ufo").setup(opts)
  end,
}
