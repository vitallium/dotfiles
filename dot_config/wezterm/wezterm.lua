local wezterm = require("wezterm")
local act = wezterm.action
local mod = "SUPER|SHIFT"

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    return {
        { Text = " " .. tab.active_pane.title .. " " },
    }
end)

local fonts_configurations = {
    monolisa = {
        name = "MonoLisa Variable",
        size = 14.0,
    },
    berkeley = {
        name = "Berkeley Mono Variable",
        size = 16.0,
    },
}
local font = fonts_configurations["berkeley"]

local function scheme_for_appearance(appearance)
    if appearance:find("Dark") then
        return "Modus-Vivendi"
    else
        return "Modus-Operandi"
    end
end

wezterm.on("window-config-reloaded", function(window, pane)
    local overrides = window:get_config_overrides() or {}
    local appearance = window:get_appearance()
    local scheme = scheme_for_appearance(appearance)
    if overrides.color_scheme ~= scheme then
        overrides.color_scheme = scheme
        window:set_config_overrides(overrides)
    end
end)

local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- Font
config.font = wezterm.font_with_fallback({ font.name, "Symbols Nerd Font" })
config.font_size = font.size
config.unicode_version = 15
config.allow_square_glyphs_to_overflow_width = "Never"
config.adjust_window_size_when_changing_font_size = false

-- Colors
config.color_scheme = "Modus-Operandi"
config.enable_scroll_bar = false
config.bold_brightens_ansi_colors = true

-- Tab
config.tab_max_width = 32
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false

-- Window
config.window_close_confirmation = "NeverPrompt"

-- Bell
config.audible_bell = "Disabled"
config.visual_bell = {
    fade_in_function = "EaseIn",
    fade_in_duration_ms = 100,
    fade_out_function = "EaseOut",
    fade_out_duration_ms = 100,
}

-- General
config.clean_exit_codes = { 130 }
config.automatically_reload_config = true
config.scrollback_lines = 10000

-- Key bindings
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = true
config.keys = {
    { mods = mod, key = "k", action = act.ActivatePaneDirection("Up") },
    { mods = mod, key = "j", action = act.ActivatePaneDirection("Down") },
    { mods = mod, key = "l", action = act.ActivatePaneDirection("Right") },
    { mods = mod, key = "h", action = act.ActivatePaneDirection("Left") },
    { mods = mod, key = "t", action = act.SpawnTab("CurrentPaneDomain") },
    { mods = mod, key = "|", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { mods = mod, key = "_", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { mods = mod, key = ">", action = act.MoveTabRelative(1) },
    { mods = mod, key = "<", action = act.MoveTabRelative(-1) },
    { mods = mod, key = "M", action = act.TogglePaneZoomState },
    { mods = mod, key = "p", action = act.PaneSelect({ alphabet = "", mode = "Activate" }) },
    { mods = mod, key = "C", action = act.CopyTo("ClipboardAndPrimarySelection") },
    { mods = mod, key = "]", action = wezterm.action({ ActivateTabRelative = 1 }) },
    { mods = mod, key = "[", action = wezterm.action({ ActivateTabRelative = -1 }) },
    { key = "C", mods = "CTRL", action = wezterm.action.CopyTo("ClipboardAndPrimarySelection") },
    { mods = mod, key = "d", action = wezterm.action.ShowDebugOverlay },
}

return config
