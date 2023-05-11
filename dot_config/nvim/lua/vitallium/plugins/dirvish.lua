return {
  {
    "justinmk/vim-dirvish",
    dependencies = {
      "roginfarrer/vim-dirvish-dovish",
    },
    events = "VeryLazy",
    keys = {
      { "<leader>o-", ":Dirvish<CR>", desc = "Dirvish" },
    },
  },
}
