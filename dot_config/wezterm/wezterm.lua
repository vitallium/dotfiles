local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action
local mod = "SHIFT|SUPER"

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    return {
        { Text = " " .. tab.active_pane.title .. " " },
    }
end)

local fonts_configurations = {
    monolisa = {
        name = "Monolisa",
        size = 12.0,
        harfbuzz_features = {
            "zero=1",
            "ss01=1",
            "ss02=1",
            "ss03=1",
            "ss04=1",
            "ss05=1",
            "ss06=1",
            "onum=0",
            "frac=0",
            "sups=0",
            "subs=0",
            "calt=1",
            "liga=1",
        },
    },
    berkeley = {
        name = "Berkeley Mono",
        size = 14.0,
        harfbuzz_features = {},
    },
    mononoki = {
        name = "mononoki",
        size = 18.0,
        harfbuzz_features = {},
    },
    pragmata = {
        name = "PragmataPro Mono Liga",
        size = 16.0,
        harfbuzz_features = {},
    },
}
local font = fonts_configurations["berkeley"]

return {
    front_end = "WebGpu",
    -- Font
    font = wezterm.font_with_fallback({ font.name, "Symbols Nerd Font Mono" }),
    harfbuzz_features = font.harfbuzz_features,
    font_size = font.size,
    use_cap_height_to_scale_fallback_fonts = true,
    unicode_version = 14,
    -- Colors
    color_scheme = "tokyonight_night",
    enable_scroll_bar = false,
    bold_brightens_ansi_colors = true,
    -- Tab
    tab_max_width = 32,
    use_fancy_tab_bar = true,
    hide_tab_bar_if_only_one_tab = true,
    -- Window
    window_close_confirmation = "NeverPrompt",
    window_decorations = "TITLE | RESIZE",
    adjust_window_size_when_changing_font_size = false,
    -- Bell
    audible_bell = "Disabled",
    visual_bell = {
        fade_in_function = "EaseIn",
        fade_in_duration_ms = 100,
        fade_out_function = "EaseOut",
        fade_out_duration_ms = 100,
    },
    -- General
    clean_exit_codes = { 130 },
    automatically_reload_config = true,
    scrollback_lines = 10000,
    term = "wezterm",
    -- Key bindings
    send_composed_key_when_left_alt_is_pressed = false,
    send_composed_key_when_right_alt_is_pressed = true,
    keys = {
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
        { mods = mod, key = "[", action = wezterm.action({ ActivateTabRelative = 1 }) },
        { mods = mod, key = "]", action = wezterm.action({ ActivateTabRelative = -1 }) },
        { key = "C", mods = "CTRL", action = wezterm.action.CopyTo("ClipboardAndPrimarySelection") },
        { mods = mod, key = "d", action = wezterm.action.ShowDebugOverlay },
    },
}
