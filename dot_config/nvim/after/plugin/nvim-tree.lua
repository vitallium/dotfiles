local status_ok_nvim_tree, nvim_tree = pcall(require, "nvim-tree")
if not status_ok_nvim_tree then
  return
end

local status_ok_nvim_tree_config, nvim_tree_config = pcall(require, "nvim-tree.config")
if not status_ok_nvim_tree_config then
  return
end

local tree_cb = nvim_tree_config.nvim_tree_callback

nvim_tree.setup({
  hijack_cursor = true,
  sync_root_with_cwd = true,
  update_focused_file = {
    enable = true,
    update_root = true,
  },
  renderer = {
    root_folder_modifier = ":t",
  },
  diagnostics = {
    enable = true,
    show_on_dirs = true,
  },
  view = {
    width = 30,
    side = "left",
    mappings = {
      list = {
        { key = { "l", "<CR>", "o" }, cb = tree_cb "edit" },
        { key = "h",                  cb = tree_cb "close_node" },
        { key = "v",                  cb = tree_cb "vsplit" },
      },
    },
  },
})
