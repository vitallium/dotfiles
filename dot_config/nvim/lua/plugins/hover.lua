return {
  "lewis6991/hover.nvim",
  keys = {
    {
      "K",
      function()
        --
        if vim.bo.filetype == "rust" and package.loaded["rust-tools.hover_actions"] ~= nil then
          require("rust-tools.hover_actions").hover_actions()
        elseif vim.fs.basename(vim.api.nvim_buf_get_name(0)) == "Cargo.toml" then
          require("crates").show_popup()
        else
          require("hover").hover()
        end
      end,
      desc = "Documentation",
    },
  },
  opts = {
    init = function()
      -- Require providers
      require("hover.providers.lsp")
      require("hover.providers.gh")
      require("hover.providers.gh_user")
      require("hover.providers.man")
      require("hover.providers.dictionary")
    end,
    preview_opts = {
      border = vim.g.border,
    },
    title = false,
  },
}
