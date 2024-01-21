return {
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            {
                "folke/neodev.nvim",
                opts = {
                    library = {
                        plugins = false,
                    },
                    setup_jsonls = false,
                },
            },
            "mihyaeru21/nvim-lspconfig-bundler",
        },
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            require("lspconfig.ui.windows").default_options.border = vim.g.border

            vim.lsp.set_log_level(vim.log.levels.ERROR)

            require("vim.lsp.log").set_format_func(vim.inspect)

            vim.keymap.set("n", "<leader>li", vim.cmd.LspInfo, { desc = "LSP Info" })
            vim.keymap.set("n", "<leader>ll", vim.cmd.LspLog, { desc = "LSP Log" })
            vim.keymap.set("n", "<leader>lr", vim.cmd.LspRestart, { desc = "LSP Restart" })

            local lsp_servers = require("plugins.lsp.servers")
            lsp_servers.setup()
        end,
    },
    {
        "aznhe21/actions-preview.nvim",
        config = function()
            require("actions-preview").setup({
                diff = {
                    algorithm = "patience",
                    ignore_whitespace = true,
                },
            })
        end,
    },
    {
        "SmiteshP/nvim-navic",
        event = "LspAttach",
        config = function()
            vim.g.navic_silence = true

            require("nvim-navic").setup({
                highlight = true,
            })
        end,
    },
    { "b0o/schemastore.nvim",    version = false },
    {
        "crispgm/nvim-go",
        cmd = {
            "GoFormat",
            "GoGet",
            "GoInstall",
            "GoLint",
        },
        event = "VeryLazy",
    },
    { "ray-x/lsp_signature.nvim" },
    { "VidocqH/lsp-lens.nvim",   config = true,  lazy = false },
    { "smjonas/inc-rename.nvim", config = true }
}
