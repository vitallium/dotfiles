local group = vim.api.nvim_create_augroup("Setup", {})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = group,
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 100 })
  end,
  desc = "Highlights the yanked text",
})

-- Automatically close Vim if the quickfix window is the only one open
vim.api.nvim_create_autocmd("WinEnter", {
  group = group,
  callback = function()
    if vim.fn.winnr("$") == 1 and vim.fn.win_gettype() == "quickfix" then
      vim.cmd.q()
    end
  end,
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  desc = "Map q to close the buffer.",
  pattern = { "qf", "checkhealth", "help", "man", "notify", "query", "tsplayground", "fugitive" },
  callback = function(event)
    vim.opt_local.spell = false
    vim.bo[event.buf].buflisted = false

    -- Quit with q in these types.
    vim.keymap.set("n", "q", vim.cmd.close, { noremap = true, silent = true, buffer = event.buf })

    -- Open help in a new tab.
    if vim.bo[event.buf].filetype == "help" then
      vim.cmd.wincmd("T")
    end
  end,
})

-- From https://github.com/dsully/dotfiles/blob/master/.config/nvim/lua/config/autocommands.lua#L117
vim.api.nvim_create_autocmd({ "BufReadPre", "FileReadPre" }, {
  callback = function(args)
    if vim.loop.fs_stat(args.file).size < 1024 * 512 then
      return
    end

    vim.g.large_file = true

    vim.notify("File is too large, disabling treesitter, syntax & language servers.")

    for _, client in pairs(vim.lsp.get_active_clients({ bufnr = args.buf })) do
      pcall(vim.lsp.buf_detach_client, args.buf, client.id)
    end

    -- Create a autocommand just in case.
    vim.api.nvim_create_autocmd({ "LspAttach" }, {
      buffer = args.buf,
      callback = function(a)
        vim.lsp.buf_detach_client(args.buf, a.data.client_id)
      end,
    })

    vim.diagnostic.disable()

    vim.cmd.TSDisable("highlight")
    vim.cmd.TSDisable("incremental_selection")
    vim.cmd.TSDisable("indent")
    vim.cmd.syntax("off")

    vim.bo.swapfile = false
    vim.bo.undolevels = -1
    vim.wo.foldmethod = "manual"
    vim.wo.list = false
    vim.wo.spell = false
    vim.opt.undoreload = 0
  end,
  desc = "Disable features for large files.",
})

vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("better_backup", { clear = true }),
  callback = function(event)
    local file = vim.loop.fs_realpath(event.match) or event.match
    local backup = vim.fn.fnamemodify(file, ":p:~:h")
    backup = backup:gsub("[/\\]", "%%")
    vim.go.backupext = backup
  end,
  desc = "Create directories when needed, when saving a file.",
})
