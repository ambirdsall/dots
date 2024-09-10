local wezterm = require("wezterm")

local color_schemes = { 'DanQing (base16)', 'DanQing Light (base16)', 'Dracula+' }
local color_scheme_toggle_counter = 0

wezterm.on('toggle-colorscheme', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  if not overrides.color_scheme then
    -- overrides.color_scheme = 'Navy and Ivory (terminal.sexy)'
    overrides.color_scheme = 'neobones_light'
  else
    overrides.color_scheme = nil
  end
  window:set_config_overrides(overrides)
  wezterm.run_child_process { 'emacsclient', '--socket-name=ttylated', '--eval', "'(amb/toggle-themes)'" }
end)

return {
  apply = function(config)
    config.color_scheme = color_schemes[1]
    config.hide_tab_bar_if_only_one_tab = true
    config.use_fancy_tab_bar = true
    config.window_padding = {
      left = 0,
      right = 0,
      top = 0,
      bottom = 0,
    }
  end,
}
