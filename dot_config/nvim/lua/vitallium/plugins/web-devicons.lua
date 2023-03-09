return {
  "nvim-tree/nvim-web-devicons", -- Fancy icons in pop-ups
  lazy = true,
  opts = {
    -- globally enable different highlight colors per icon (default to true)
    -- if set to false all icons will have the default icon's color
    color_icons = true,
    -- globally enable default icons (default to false)
    -- will get overriden by `get_icons` option
    default = true,
    override = {
      ["test"] = {
        icon = "󰙨",
        color = "#a6e3a1",
        name = "TestFile",
      },
      ["spec"] = {
        icon = "󰙨",
        color = "#a6e3a1",
        name = "SpecFile",
      },
    },
  },
  config = function(_, opts)
    -- Overriding `get_ext` to allow for test files.
    local devicons = require("nvim-web-devicons")

    local function get_ext(name)
      if name:find("^.+%.test%..+$") or name:find("^.+Test%..+$") or name:find("^.+_test%..+$") then
        return "test"
      end

      if name:find("^.+%.spec%..+$") or name:find("^.+Spec%..+$") or name:find("^.+_spec%..+$") then
        return "spec"
      end

      return name:match("^.*%.(.*)$") or ""
    end

    local get_icon = devicons.get_icon
    devicons.get_icon = function(name, ext, options)
      return get_icon(name, ext or get_ext(name), options)
    end

    local get_icon_colors = devicons.get_icon_colors
    devicons.get_icon_colors = function(name, ext, options)
      return get_icon_colors(name, ext or get_ext(name), options)
    end

    devicons.setup(opts)
  end,
}
