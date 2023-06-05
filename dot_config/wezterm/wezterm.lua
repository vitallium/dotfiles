local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action
local mod = "SHIFT|SUPER"

wezterm.on("gui-startup", function()
	local _, _, window = mux.spawn_window({})
	window:gui_window():maximize()
end)

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	return {
		{ Text = " " .. tab.active_pane.title .. " " },
	}
end)

---@diagnostic disable-next-line: unused-local
local monolisa = "Monolisa"
---@diagnostic disable-next-line: unused-local
local mononoki = "mononoki"
---@diagnostic disable-next-line: unused-local
local berkeley = "Berkeley Mono"

local font = berkeley
local font_size = 16.0

local harfbuzz_features = {}
if font == monolisa then
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
	}
elseif font == berkeley then
	harfbuzz_features = {
		"calt=0",
		"dlig=1",
	}
end

return {
	font = wezterm.font_with_fallback({ font, "Symbols Nerd Font Mono" }),
	harfbuzz_features = harfbuzz_features,
	font_size = font_size,
	use_cap_height_to_scale_fallback_fonts = true,
	-- colors
	color_scheme = "tokyonight_night",
	enable_scroll_bar = false,
	bold_brightens_ansi_colors = true,

	-- tab
	tab_max_width = 32,
	use_fancy_tab_bar = true,
	hide_tab_bar_if_only_one_tab = true,
	-- window
	window_close_confirmation = "NeverPrompt",
	window_decorations = "TITLE | RESIZE",
	adjust_window_size_when_changing_font_size = false,
	-- bell
	audible_bell = "Disabled",
	visual_bell = {
		fade_in_function = "EaseIn",
		fade_in_duration_ms = 100,
		fade_out_function = "EaseOut",
		fade_out_duration_ms = 100,
	},
	-- general config
	clean_exit_codes = { 130 },
	automatically_reload_config = true,
	scrollback_lines = 10000,
	-- key bindings
	send_composed_key_when_left_alt_is_pressed = false,
	send_composed_key_when_right_alt_is_pressed = false,
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
