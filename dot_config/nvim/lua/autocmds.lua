local group = vim.api.nvim_create_augroup("Setup", {})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = group,
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 100 })
  end,
  desc = "Highlights the yanked text",
})

-- Hide cursorline in insert mode
vim.api.nvim_create_autocmd({ "InsertLeave", "WinEnter" }, { command = "set cursorline", group = group })
vim.api.nvim_create_autocmd({ "InsertEnter", "WinLeave" }, { command = "set nocursorline", group = group })

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
  pattern = { "qf", "checkhealth", "help", "man", "notify", "fugitive" },
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

vim.api.nvim_create_autocmd({ "BufReadPre", "FileReadPre" }, {
  callback = function(args)
    if vim.loop.fs_stat(args.file).size < 1024 * 512 then
      return
    end

    vim.g.large_file = true

    vim.notify("File is too large, disabling treesitter & language servers.")

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

    vim.bo.swapfile = false
    vim.bo.undolevels = -1
    vim.wo.foldmethod = "manual"
    vim.wo.list = false
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
