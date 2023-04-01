local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

require('config/multiplexing').apply(config)
require('config/keybinding').apply(config)

-- TODO local file

return config
