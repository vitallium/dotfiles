return {
    'stevearc/conform.nvim',
    event = { 'BufWritePre', 'LspAttach' },
    cmd = { 'ConformInfo' },
    keys = {
      {
        '<leader>bf',
        function()
          require('conform').format { async = true, lsp_fallback = true }
        end,
        mode = '',
        desc = '[F]ormat buffer',
      },
    },
    opts = {
      notify_on_error = false,
      format_on_save = function(bufnr)
        -- Disable "format_on_save lsp_fallback" for languages that don't
        -- have a well standardized coding style. You can add additional
        -- languages here or re-enable it for the disabled ones.
        local disable_filetypes = { c = true, cpp = true }
        return {
          timeout_ms = 500,
          lsp_fallback = not disable_filetypes[vim.bo[bufnr].filetype],
        }
      end,
      formatters_by_ft = {
        fish = { 'fish_indent' },
        go = { 'goimports', 'gofumpt' },
        javascript = { 'prettierd', 'prettier', stop_after_first = true },
        lua = { 'stylua' },
        toml = { 'taplo' },
      },
    },
  };
