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
vim.opt.background = 'dark'                  -- Set background to dark
vim.cmd('colorscheme tokyonight-night')      -- Set theme to tokyonight-night

-- Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

require'nvim-web-devicons'.setup {
  -- globally enable different highlight colors per icon (default to true)
  -- if set to false all icons will have the default icon's color
  color_icons = true;
  -- globally enable default icons (default to false)
  -- will get overriden by `get_icons` option
  default = true;
}
