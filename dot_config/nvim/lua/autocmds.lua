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
  pattern = {
    "checkhealth",
    "fugitive",
    "help",
    "lspinfo",
    "man",
    "notify",
    "qf",
    "query",
    "tsplayground",
  },
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

-- resize splits if window got resized
vim.api.nvim_create_autocmd({ "VimResized" }, {
  callback = function()
    vim.cmd("tabdo wincmd =")
  end,
})
