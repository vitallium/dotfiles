return {
  {
    "nvim-treesitter/nvim-treesitter",
    cmd = {
      "TSBufDisable",
      "TSBufEnable",
      "TSDisable",
      "TSEnable",
      "TSInstall",
      "TSModuleInfo",
      "TSUpdateSync",
    },
    event = { "BufReadPost", "BufNewFile" },
    dependencies = {
      {
        -- Used by mini.ai. No need to specify the text objects within treesitter config.
        "nvim-treesitter/nvim-treesitter-textobjects", -- Additional textobjects for treesitter
        event = { "BufReadPost", "BufNewFile" },
      },
      {
        "nvim-treesitter/nvim-treesitter-context", -- Keep e.g. function at top when scrolling below
        name = "treesitter-context",
        event = { "BufReadPost", "BufNewFile" },
      },
      {
        "windwp/nvim-ts-autotag", -- Auto-tags for HTML, Vue, etc.
        event = { "BufReadPost", "BufNewFile" },
      },
      {
        "JoosepAlviste/nvim-ts-context-commentstring",
        name = "ts_context_commentstring",
        event = { "BufReadPost", "BufNewFile" },
      },
      {
        "RRethy/nvim-treesitter-endwise",
        event = { "InsertEnter" },
      },
    },
    build = function()
      local ts_update = require("nvim-treesitter.install").update({
        with_sync = true,
      })
      ts_update()
    end,
    config = function()
      local parser = require("nvim-treesitter.parsers").get_parser_configs()
      parser.gotmpl = {
        install_info = {
          url = "https://github.com/ngalaiko/tree-sitter-go-template",
          files = { "src/parser.c" },
          revision = "45acf03891557b80a45ac1897e2cca2e8b9cf0ff",
        },
        filetype = "gotmpl",
        used_by = { "gohtmltmpl", "gotexttmpl", "gotmpl" },
      }

      require("nvim-treesitter.configs").setup({
        -- A list of parser names, or "all"
        ensure_installed = {
          "bash",
          "css",
          "fish",
          "dockerfile",
          "go",
          "gomod",
          "gotmpl",
          "gitcommit",
          "gitignore",
          "html",
          "http",
          "ini",
          "javascript",
          "jq",
          "json",
          "json5",
          "lua",
          "make",
          "markdown",
          "markdown_inline",
          "python",
          "regex",
          "ruby",
          "rust",
          "scss",
          "toml",
          "typescript",
          "vim",
          "vimdoc",
          "vue",
          "yaml",
        },
        -- Install parsers synchronously (only applied to `ensure_installed`)
        sync_install = false,
        -- Automatically install missing parsers when entering buffer
        -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
        auto_install = true,
        -- List of parsers to ignore installing (for "all")
        -- ignore_install = {  },

        ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
        -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

        highlight = {
          enable = true,
          disable = {
            -- Makes MD|inline highlights ugly
            "md",
            "markdown",
          },
        },
        autotag = {
          enable = true, -- Through auto-tag plugin
        },
        indent = { -- Indentation based on = operator (experimental)
          enable = true,
        },
        -- https://github.com/RRethy/nvim-treesitter-endwise
        endwise = {
          enable = true,
        },
        query_linter = {
          enable = true,
          use_virtual_text = true,
          lint_events = { "BufWrite", "CursorHold" },
        },
      })

      require("ts_context_commentstring").setup({
        enable_autocmd = true,
        languages = {
          typescript = "// %s",
          ruby = {
            __default = "# %s",
            __multiline = "=begin %s =end",
          },
        },
      })
    end,
  },
}
