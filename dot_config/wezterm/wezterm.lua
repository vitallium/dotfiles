local wezterm = require("wezterm")
local mux = wezterm.mux

wezterm.on("gui-startup", function()
	local _, _, window = mux.spawn_window({})
	window:gui_window():maximize()
end)

return {
	font = wezterm.font_with_fallback({
		{
			family = "MonoLisa",
		},
		{
			family = "Symbols Nerd Font Mono",
		},
	}),
	font_size = 14.0,
	color_scheme = "Catppuccin Mocha",
	enable_scroll_bar = false,
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
}
