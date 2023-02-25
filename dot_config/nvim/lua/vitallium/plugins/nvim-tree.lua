return {
  "nvim-tree/nvim-tree.lua", -- File browser
  dependencies = { "nvim-tree/nvim-web-devicons" },
  keys = {
    { "<leader>ft", ":NvimTreeFindFileToggle<CR>", desc = "Toggle file tree at current buffer" },
    { "<leader>fT", ":NvimTreeToggle<CR>", desc = "Toggle file tree" },
  },
  config = function()
    local icons = require("vitallium.icons").git
    require("nvim-tree").setup({
      view = {
        mappings = {
          custom_only = false,
          list = {
            -- user mappings go here
            { key = { "l" }, action = "edit" }, -- Open node with l
            { key = { "h" }, action = "close_node" }, -- Close node with h
            {
              key = { "o" }, -- Close the tree when opening node with "o"
              action = "edit_and_close",
              action_cb = function()
                local api = require("nvim-tree.api")
                api.node.open.edit()
                api.tree.close()
              end,
            },
          },
        },
      },
      renderer = {
        highlight_opened_files = "name",
        indent_markers = {
          enable = true,
        },
        icons = {
          git_placement = "after",
          glyphs = {
            git = {
              unstaged = icons.FileUnstaged,
              staged = icons.FileStaged,
              unmerged = icons.FileUnmerged,
              renamed = icons.FileRenamed,
              untracked = icons.FileUntracked,
              deleted = icons.FileDeleted,
              ignored = icons.FileIgnored,
            },
          },
        },
      },
      actions = { open_file = { quit_on_open = false } },
      update_focused_file = {
        enable = true,
      },
    })
  end,
}
