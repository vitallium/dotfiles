local wezterm = require("wezterm")
local act = wezterm.action
local mod = "SUPER|SHIFT"

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    return {
        { Text = " " .. tab.active_pane.title .. " " },
    }
end)

local config = wezterm.config_builder()

-- Font
config.font = wezterm.font("Berkeley Mono")
config.font_size = 16.0
config.freetype_load_target = "Light"
config.unicode_version = 15
config.allow_square_glyphs_to_overflow_width = "Never"
config.adjust_window_size_when_changing_font_size = false

-- Colors
config.color_scheme = "tokyonight_night"
config.enable_scroll_bar = false
config.bold_brightens_ansi_colors = true

-- Tab
config.tab_max_width = 60
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true

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
config.front_end = "OpenGL"

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
    { key = "UpArrow", mods = "SHIFT", action = act.ScrollToPrompt(-1) },
    { key = "DownArrow", mods = "SHIFT", action = act.ScrollToPrompt(1) },
}

return config
