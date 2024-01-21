return {
    "stevearc/conform.nvim",
    cmd = { "ConformInfo" },
    keys = {
        {
            "<leader>bf",
            function()
                require("conform").format({ async = true, lsp_fallback = true })
            end,
            mode = "",
            desc = "Format buffer",
        },
    },
    opts = function()
        return {
            formatters_by_ft = {
                css = { "prettier" },
                fish = { "fish_indent" },
                go = { "gofumpt", "injected" },
                graphql = { "prettier" },
                html = { "prettier" },
                javascript = { { "prettierd", "prettier" } },
                javascriptreact = { "prettier" },
                json = { "prettier" },
                lua = { "stylua" },
                markdown = { "prettier" },
                python = { "isort", "black" },
                ruby = { "rubocop" },
                svelte = { "prettier" },
                typescript = { "prettier" },
                typescriptreact = { "prettier" },
                yaml = { "prettier" },
            },
            format_on_save = function(bufnr)
                if vim.bo[bufnr].filetype == "ruby" then
                    return
                end

                return { timeout_ms = 1000, lsp_fallback = false }
            end,
            format_after_save = function(bufnr)
                if vim.bo[bufnr].filetype ~= "ruby" then
                    return
                end

                return { lsp_fallback = false }
            end,
            -- notify_on_error = false,
            formatters = {
                rubocop = {
                    inherit = false,
                    command = "bundle",
                    args = {
                        "exec",
                        "rubocop",
                        "-f",
                        "quiet",
                        "--fix-layout",
                        "--stderr",
                        "--stdin",
                        "$FILENAME",
                    },
                    cwd = require("conform.util").root_file({ ".rubocop.yml", ".rubocop-gdk.yml" }),
                    require_cwd = true,
                    exit_codes = { 0, 1 },
                },
            },
        }
    end,
    init = function()
        vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    end,
}
