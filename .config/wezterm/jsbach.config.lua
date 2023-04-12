local wezterm = require 'wezterm'
local act = wezterm.action

local colors = {
  -- The default text color
  foreground = '#abb2bf',
  -- The default background color
  background = '#282c34',

  -- Overrides the cell background color when the current cell is occupied by the
  -- cursor and the cursor style is set to Block
  --cursor_bg = "#2c323c",
  cursor_bg = '#abb2bf',
  -- -- Overrides the text color when the current cell is occupied by the cursor
  -- cursor_fg = "#5c6370",
  -- Specifies the border color of the cursor when the cursor style is set to Block,
  -- or the color of the vertical or horizontal bar when the cursor style is set to
  -- Bar or Underline.
  cursor_border = '#5c6370',

  -- the foreground color of selected text
  selection_fg = '#2c323c',
  -- the background color of selected text
  selection_bg = '#5c6370',

  -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  scrollbar_thumb = '#222222',

  -- The color of the split lines between panes
  split = '#444444',

  ansi = { '#2c323c', '#e06c75', '#98c379', '#e5c07b', '#61afef', '#c678dd', '#56b6c2', '#5c6370' },
  brights = { '#2c323c', '#e06c75', '#98c379', '#e5c07b', '#61afef', '#c678dd', '#56b6c2', '#5c6370' },
}

local function isViProcess(pane)
    -- get_foreground_process_name On Linux, macOS and Windows, 
    -- the process can be queried to determine this path. Other operating systems 
    -- (notably, FreeBSD and other unix systems) are not currently supported
    return pane:get_foreground_process_name():find('n?vim') ~= nil
    -- return pane:get_title():find("n?vim") ~= nil
end

local function conditionalActivatePane(window, pane, pane_direction, vim_direction)
    if isViProcess(pane) then
        window:perform_action(
            -- This should match the keybinds you set in Neovim.
            act.SendKey({ key = vim_direction, mods = 'ALT' }),
            pane
        )
    else
        window:perform_action(act.ActivatePaneDirection(pane_direction), pane)
    end
end

wezterm.on('ActivatePaneDirection-right', function(window, pane)
    conditionalActivatePane(window, pane, 'Right', 'l')
end)
wezterm.on('ActivatePaneDirection-left', function(window, pane)
    conditionalActivatePane(window, pane, 'Left', 'h')
end)
wezterm.on('ActivatePaneDirection-up', function(window, pane)
    conditionalActivatePane(window, pane, 'Up', 'k')
end)
wezterm.on('ActivatePaneDirection-down', function(window, pane)
    conditionalActivatePane(window, pane, 'Down', 'j')
end)

return {
  font = wezterm.font 'MesloLGS NF',
  font_size = 13,
  enable_scroll_bar = true,
  colors = colors,
  default_cursor_style = 'SteadyBar',
  enable_kitty_graphics = true,
  front_end = "WebGpu",
  hide_tab_bar_if_only_one_tab = true,
  use_fancy_tab_bar = true,
  window_frame = {
    font_size = 12.0,
  },
  ssh_domains = {
    {
      name = 'fedora',
      remote_address = 'fedora',
    },
    {
      name = 'sc',
      remote_address = 'sc',
    },
    {
      name = 'scdt',
      remote_address = 'scdt',
    },
  },
  leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 },
  keys = {
    { key = 's', mods = 'LEADER', action = act { SplitHorizontal = { domain = 'CurrentPaneDomain' } } },
    { key = 'v', mods = 'LEADER', action = act { SplitVertical = { domain = 'CurrentPaneDomain' } } },

    -- -- Send "CTRL-A" to the terminal when pressing CTRL-A, CTRL-A
    { key = 'a', mods = 'LEADER|CTRL', action = act { SendString = '\x01' } },

    -- Fuzzy attach to domain
    { key = 't', mods = 'CMD|SHIFT', action = act.ShowLauncherArgs { flags = 'FUZZY|DOMAINS' } },

    -- Detach domain of the current pain
    { key = 'd', mods = 'LEADER', action = act.DetachDomain 'CurrentPaneDomain' },

    -- Switch back to last tab
    { key = ',', mods = 'LEADER', action = wezterm.action.ActivateLastTab },

    -- Navigate between tabs
    { key = 'LeftArrow', mods = 'SHIFT', action = act.ActivateTabRelative(-1) },
    { key = 'RightArrow', mods = 'SHIFT', action = act.ActivateTabRelative(1) },

    -- TMUX like switch tabs
    { key = 'w', mods = 'LEADER', action = act.ShowLauncherArgs { flags = 'FUZZY|TABS' } },

    -- TMUX like switch tabs
    { key = 'p', mods = 'LEADER', action = act.ShowLauncherArgs { flags = 'FUZZY|COMMANDS' } },

    -- TMUX like zoom tab
    { key = 'z', mods = 'LEADER', action = 'TogglePaneZoomState' },

    -- Close tab without confirmation
    { key = 'w', mods = 'CMD', action = act { CloseCurrentPane = { confirm = false } } },

    -- Navigate with 
    { key = 'LeftArrow', mods = 'CTRL|ALT', action = act.ActivatePaneDirection 'Left' },
    { key = 'RightArrow', mods = 'CTRL|ALT', action = act.ActivatePaneDirection 'Right' },
    { key = 'UpArrow', mods = 'CTRL|ALT', action = act.ActivatePaneDirection 'Up' },
    { key = 'DownArrow', mods = 'CTRL|ALT', action = act.ActivatePaneDirection 'Down' },

    { key = 'Space', mods = 'LEADER', action = act.RotatePanes 'CounterClockwise' },
    { key = '0', mods = 'CTRL', action = act.PaneSelect { mode = 'SwapWithActive' } },

    { key = 'c', mods = 'LEADER', action = act.PaneSelect { mode = 'Activate' } },

    -- Copy mode
    { key = '[', mods = 'LEADER', action = wezterm.action.ActivateCopyMode },

    { key = 'h', mods = 'ALT', action = act.EmitEvent('ActivatePaneDirection-left') },
    { key = 'j', mods = 'ALT', action = act.EmitEvent('ActivatePaneDirection-down') },
    { key = 'k', mods = 'ALT', action = act.EmitEvent('ActivatePaneDirection-up') },
    { key = 'l', mods = 'ALT', action = act.EmitEvent('ActivatePaneDirection-right') },

    {
      key = 'e',
      mods = 'LEADER',
      action = wezterm.action {
        QuickSelectArgs = {
          patterns = {
            'http?://\\S+',
            'https?://\\S+',
          },
          action = wezterm.action_callback(function(window, pane)
            local url = window:get_selection_text_for_pane(pane)
            wezterm.open_with(url)
          end),
        },
      },
    },

    -- Other useful keybindings:
    -- ctrl-shift-v smart grab, uppercase copies *and* pastes
    -- ctrl-shift-x
    -- wezterm imgcat /path/to/img
  },
  quick_select_patterns = {
    'croc .*',
  },
}
