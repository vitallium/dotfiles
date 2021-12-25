function map(mode, shortcut, command)
  vim.api.nvim_set_keymap(mode, shortcut, command, { noremap = true, silent = true })
end

function nmap(shortcut, command)
  map('n', shortcut, command)
end

-- Find files using Telescope command-line sugar.
nmap("<C-p>", "<cmd>Telescope find_files<cr>")
nmap("<leader>bb", "<cmd>Telescope buffers<cr>")
