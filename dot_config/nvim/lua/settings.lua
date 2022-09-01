-- General
vim.opt.clipboard = 'unnamedplus'            -- System clipboard
vim.opt.dictionary = '/usr/share/dict/words' -- Set up a dictionary
vim.opt.signcolumn = 'yes'                   -- Show signcolumns
vim.opt.splitbelow = true
vim.opt.splitright = true                    -- Split defaults

-- Search
vim.opt.ignorecase = true                    -- Ignores case in search
vim.opt.smartcase = true                     -- Overrides ignore when capital exists
vim.opt.inccommand = 'split'                 -- Displays incremental replacement

-- Editor
vim.opt.expandtab = true                     -- Expand tab to spaces
vim.opt.shiftwidth = 2                       -- Number of spaces for indentation
vim.opt.tabstop = 2                          -- Number of spaces a <Tab> is
vim.opt.timeoutlen = 500                     -- Wait less time for mapped sequences
vim.opt.smartindent = true                   -- Makes indenting smart
vim.opt.autoindent = true                    -- Good auto indent
vim.opt.complete = '.,w,b'                   -- Sources for term and line completions
vim.opt.completeopt = {
  'menu',
  'menuone',
  'noinsert',
  'noselect' }

-- Visual
vim.opt.showmode = false                     -- Don't show mode changes
vim.opt.wrap = false                         -- Don't wrap lines
vim.opt.number = true                        -- Enable line numbers
vim.opt.relativenumber = true                -- Enable relative line numbers
vim.opt.showmatch = true                     -- Show matching braces
vim.opt.colorcolumn = '100'                  -- Fill column
vim.opt.termguicolors = true                 -- Enable TrueColor support
vim.opt.background = 'light'                 -- Set background to light
vim.cmd('colorscheme modus-operandi')        -- Set theme to modus operandi

-- Status line
require('lualine').setup {
  options = {
    icons_enabled = false,
    theme = onedark,
    component_separators = '|',
    section_separators = '',
  },
  sections = {
    lualine_a = { 'mode' },
    lualine_b = { 'filename' },
    lualine_c = { 'lsp_progress' },
    lualine_x = { 'filetype' },
    lualine_y = { 'progress' },
    lualine_z = { 'location' },
  },
}

-- Key bindings
-- Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Files
vim.api.nvim_set_keymap('n', '<leader>f', [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<cr>]], { noremap = true, silent = true })

-- Buffers
-- Delete current buffer
vim.api.nvim_set_keymap('n', '<leader>bd', ':bdelete<CR>', { noremap = true, silent = true })
-- List all buffers
vim.api.nvim_set_keymap(
  'n',
  '<leader>,',
  [[<cmd>lua require('telescope.builtin').buffers({sort_lastused = true})<cr>]],
  { noremap = true, silent = true }
)

-- Gitsigns
require('gitsigns').setup {
  signs = {
    add = { hl = 'GitGutterAdd', text = '+' },
    change = { hl = 'GitGutterChange', text = '~' },
    delete = { hl = 'GitGutterDelete', text = '_' },
    topdelete = { hl = 'GitGutterDelete', text = '‾' },
    changedelete = { hl = 'GitGutterChange', text = '~' },
  },
}
