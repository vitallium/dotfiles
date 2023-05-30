local M = {}

-- Hook up autocomplete for LSP to nvim-cmp, see: https://github.com/hrsh7th/cmp-nvim-lsp
M.capabilities = function()
  --
  -- Since cmp-nvim-lsp has an after/ file which load cmp, which I want deferred until InsertEnter,
  -- create a cache of the table that .default_capabilities() emits to JSON to be loaded instead.
  local path = vim.fn.stdpath("cache") .. "/capabilities.json"
  local module = "cmp_nvim_lsp"

  if not vim.loop.fs_stat(path) then
    require("lazy").load({ plugins = { "cmp-nvim-lsp" } })

    vim.fn.writefile({ vim.json.encode(require(module).default_capabilities()) }, path)

    require("plenary.reload").reload_module(module)
  end

  return vim.json.decode(vim.fn.readfile(path)[1])
end

M.on_attach = function(client, buffer)
  -- https://github.com/neovim/nvim-lspconfig/wiki/UI-Customization#highlight-symbol-under-cursor
  if client.server_capabilities.documentHighlightProvider then
    local group = vim.api.nvim_create_augroup("LSPHighlightReferences", {})

    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      group = group,
      buffer = buffer,
      callback = vim.lsp.buf.document_highlight,
    })

    vim.api.nvim_create_autocmd({ "CursorMoved" }, {
      group = group,
      buffer = buffer,
      callback = vim.lsp.buf.clear_references,
    })
  end

  -- General diagnostics.
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next Diagnostic" })
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous Diagnostic" })
  vim.keymap.set("n", "<leader>xr", vim.diagnostic.reset, { desc = "Reset" })
  vim.keymap.set("n", "<leader>xs", vim.diagnostic.open_float, { desc = "Show" })

  if client.server_capabilities.signatureHelpProvider then
    vim.keymap.set("n", "<leader>ch", vim.lsp.buf.signature_help, { desc = "Signature Help" })
    vim.keymap.set("i", "<C-k>", vim.lsp.buf.signature_help, { desc = "Signature Help" })
  end

  if client.server_capabilities.declarationProvider then
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "Go To Declaration" })
  end

  if client.server_capabilities.documentFormattingProvider then
    local ignored = require("ignored")

    vim.keymap.set("n", "<space>bf", function()
      vim.lsp.buf.format({
        async = true,
        -- Let null-ls handle formatting instead of these language servers.
        filter = function(c)
          return not vim.tbl_contains(ignored.formatters, c.name)
        end,
      })
    end, { desc = "Format" })
  end

  if client.server_capabilities.codeActionProvider then
    vim.keymap.set({ "n", "x" }, "<leader>ca", require("actions-preview").code_actions, { desc = "Actions" })
  end

  -- https://github.com/smjonas/inc-rename.nvim
  if client.server_capabilities.renameProvider then
    vim.keymap.set("n", "<leader>cr", function()
      --
      return ":" .. require("inc_rename").config.cmd_name .. " " .. vim.fn.expand("<cword>")
    end, { desc = "Rename", expr = true })
  end

  if client.server_capabilities.codeLensProvider then
    vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "CursorHoldI", "InsertLeave" }, {
      desc = "LSP Code Lens Refresh",
      buffer = buffer,
      callback = vim.lsp.codelens.refresh,
    })
  end

  if client.server_capabilities.definitionProvider then
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Go To Definition(s)" })
  end

  if client.server_capabilities.implementationProvider then
    vim.keymap.set("n", "gi", function()
      vim.cmd.FzfLua.lsp_implementations()
    end, { desc = "Go To Implementations(s)" })
  end
end

-- Adapted from folke/lazyvim
-- Searches & sets the root directory based on:
--
-- * LSP workspace folders
-- * LSP root_dir
-- * root pattern of filename of the current buffer
-- * root pattern of cwd
M.find_root = function()
  local root_patterns = {
    ".git",
    ".null-ls-root",
    "Cargo.toml",
    "Gemfile",
    "go.mod",
    "config.fish",
    "configure",
    "package.json",
    "requirements.txt",
    "stylua.toml",
  }

  ---@type string?
  local path = vim.api.nvim_buf_get_name(0)
  path = path ~= "" and vim.loop.fs_realpath(path) or nil
  ---@type string[]
  local roots = {}
  if path then
    for _, client in pairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
      local workspace = client.config.workspace_folders
      local paths = workspace and vim.tbl_map(function(ws)
        return vim.uri_to_fname(ws.uri)
      end, workspace) or client.config.root_dir and { client.config.root_dir } or {}
      for _, p in ipairs(paths) do
        local r = vim.loop.fs_realpath(p)
        if path:find(r, 1, true) then
          roots[#roots + 1] = r
        end
      end
    end
  end
  table.sort(roots, function(a, b)
    return #a > #b
  end)
  ---@type string?
  local root = roots[1]
  if not root then
    path = path and vim.fs.dirname(path) or vim.loop.cwd()
    ---@type string?
    root = vim.fs.find(M.root_patterns, { path = path, upward = true })[1]
    root = root and vim.fs.dirname(root) or vim.loop.cwd()
  end
  ---@cast root string
  return root
end

return M