return {
  {
    -- Used by mini.ai. No need to specify the text objects within treesitter config.
    "nvim-treesitter/nvim-treesitter-textobjects", -- Additional textobjects for treesitter
    event = { "BufReadPost", "BufNewFile" },
    dependencies = "nvim-treesitter/nvim-treesitter",
  },
  {
    "nvim-treesitter/nvim-treesitter-context", -- Keep e.g. function at top when scrolling below
    name = "treesitter-context",
    event = { "BufReadPost", "BufNewFile" },
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = true,
  },
  {
    "windwp/nvim-ts-autotag", -- Auto-tags for HTML, Vue, etc.
    event = { "BufReadPost", "BufNewFile" },
    dependencies = "nvim-treesitter/nvim-treesitter",
  },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    name = "ts_context_commentstring",
    event = { "BufReadPost", "BufNewFile" },
    dependencies = "nvim-treesitter/nvim-treesitter",
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = function()
      local ts_update = require("nvim-treesitter.install").update({
        with_sync = true,
      })
      ts_update()
    end,
    config = function()
      require("nvim-treesitter.configs").setup({
        -- A list of parser names, or "all"
        ensure_installed = {
          "bash",
          "css",
          "dockerfile",
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
          "markdown",
          "markdown_inline",
          "scss",
          "toml",
          "tsx",
          "typescript",
          "vim",
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
          -- `false` will disable the whole extension
          enable = true,

          -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
          -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
          -- the name of the parser)
          -- list of language that will be disabled
          -- disable = { },

          -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
          -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
          -- Using this option may slow down your editor, and you may see some duplicate highlights.
          -- Instead of true it can also be a list of languages
          additional_vim_regex_highlighting = false,
        },
        autotag = {
          enable = true, -- Through auto-tag plugin
        },
        indent = { -- Indentation based on = operator (experimental)
          enable = true,
        },
        context_commentstring = { -- For nvim-ts-context-commentstring plugin
          enable = true,
          enable_autocmd = false, -- Disabled when used with Comment.nvim
        },
      })
    end,
  },
}
