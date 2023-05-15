return {
  {
    "justinmk/vim-dirvish",
    dependencies = {
      "roginfarrer/vim-dirvish-dovish",
    },
    event = "VeryLazy",
    keys = {
      { "<leader>o-", ":Dirvish<CR>", desc = "Dirvish" },
    },
  },
}
