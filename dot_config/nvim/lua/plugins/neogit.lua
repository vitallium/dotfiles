local neogit = require('neogit')
local nnoremap = require('keymap').nnoremap

neogit.setup {}

nnoremap("<leader>gs", function()
    neogit.open({ })
end);
