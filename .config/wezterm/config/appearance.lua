local wezterm = require("wezterm")

-- Equivalent to POSIX basename(3)
-- Given "/foo/bar" returns "bar"
-- Given "c:\\foo\\bar" returns "bar"
local function basename(s)
  return string.gsub(s, '(.*[/\\])(.*)', '%2')
end

local function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

wezterm.on(
  'format-window-title',
  function(tab, pane)
    local zoomed_prefix = ''
    local zoomed_suffix = ''
    if tab.active_pane.is_zoomed then
      -- something more visual (e.g. microscope emoji) would be cool
      zoomed_prefix = "🔎🔎🔎 "
      zoomed_suffix = " 🔍🔍🔍"
    end

    local workspace = "[" .. wezterm.mux.get_active_workspace() .. "] "

    -- TODO if the foreground process is `zsh`, display (possibly formatted/condensed)
    -- working directory
    local title = basename(pane.foreground_process_name)

    return workspace .. zoomed_prefix .. title .. zoomed_suffix
end)

-- wezterm.on('update-right-status', function(window, pane)
--   window:set_right_status(window:active_workspace())
-- end)

local color_schemes = { 'DanQing (base16)', 'DanQing Light (base16)' }
local color_scheme_toggle_counter = 0

return {
  -- TODO make a theme toggler that actually works
  make_color_scheme_toggle = function (config)
    return function ()
      color_scheme_toggle_counter = color_scheme_toggle_counter + 1
      local next_index = 1 + (color_scheme_toggle_counter % #color_schemes)
      config.color_scheme = color_schemes[next_index]
    end
  end,
  apply = function (config)
    config.hide_tab_bar_if_only_one_tab = true
    config.use_fancy_tab_bar = false
    config.color_scheme = 'DanQing (base16)'
    config.color_scheme = 'DanQing Light (base16)'
    config.color_scheme = 'Dracula+'
  end
}
