local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

require('config/keybinding').apply(config)
require('config/appearance').apply(config)

-- TODO local file

return config
