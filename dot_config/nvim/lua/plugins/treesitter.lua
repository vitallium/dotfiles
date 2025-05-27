return {
  {
    "nvim-treesitter/nvim-treesitter", -- Highlight, edit, and navigate code
    build = ":TSUpdateSync",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects", -- Additional text objects via treesitter
      "windwp/nvim-ts-autotag", -- Automatically close html/xml tags
    },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "bash",
          "css",
          "dockerfile",
          "go",
          "gomod",
          "graphql",
          "html",
          "javascript",
          "json",
          "lua",
          "markdown",
          "python",
          "ruby",
          "rust",
          "scss",
          "sql",
          "toml",
          "xml",
          "yaml",
        },
        highlight = {
          enable = true,
          -- Some languages depend on vim's regex highlighting system (such as Ruby) for indent rules.
          --  If you are experiencing weird indenting issues, add the language to
          --  the list of additional_vim_regex_highlighting and disabled languages for indent.
          additional_vim_regex_highlighting = { "ruby" },
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "gnn",
            scope_incremental = "grc",
            node_incremental = "v",
            node_decremental = "V",
          },
        },
        indent = {
          enable = true,
          disable = { "ruby" },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["aa"] = "@parameter.outer",
              ["ia"] = "@parameter.inner",
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]]"] = "@class.outer",
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ["<leader>a"] = "@parameter.inner",
            },
            swap_previous = {
              ["<leader>A"] = "@parameter.inner",
            },
          },
        },
      })
      require("nvim-ts-autotag").setup({})
      vim.opt.foldexpr = "nvim_treesitter#foldexpr()" -- Folding provided by treesitter
    end,
  },
}
