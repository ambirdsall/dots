local wezterm = require "wezterm"
local act = wezterm.action

local leader = function(key, action)
  return { key=key, mods="LEADER", action=action }
end

return {
  apply = function(config)
    config.leader = { key = "Space", mods = "CTRL", timeout_milliseconds = 1000 }
    config.keys = {
      { key = 'x', mods = 'META', action = act.ShowLauncherArgs { flags = 'FUZZY|COMMANDS' } },

      leader('Space', act.ActivateCopyMode),

      -- open/shut them, open/shut them, give a little clap clap clap
      leader("c", act.SpawnTab "CurrentPaneDomain"),
      leader("t", act.SpawnTab "CurrentPaneDomain"),
      leader("s", act.SplitVertical { domain='CurrentPaneDomain' }),
      leader("v", act.SplitHorizontal { domain='CurrentPaneDomain' }),
      leader("z", act.TogglePaneZoomState),
      leader("x", act.CloseCurrentPane { confirm=false }),

      -- get around, round, round, I get around
      ---- panes
      leader("h", act.ActivatePaneDirection "Left"),
      leader("LeftArrow", act.ActivatePaneDirection "Left"),
      leader("j", act.ActivatePaneDirection "Down"),
      leader("DownArrow", act.ActivatePaneDirection "Down"),
      leader("k", act.ActivatePaneDirection "Up"),
      leader("UpArrow", act.ActivatePaneDirection "Up"),
      leader("l", act.ActivatePaneDirection "Right"),
      leader("RightArrow", act.ActivatePaneDirection "Right"),
      ---- tabs
      leader("1", act { ActivateTab=0 }),
      leader("2", act { ActivateTab=1 }),
      leader("3", act { ActivateTab=2 }),
      leader("4", act { ActivateTab=3 }),
      leader("5", act { ActivateTab=4 }),
      leader("6", act { ActivateTab=5 }),
      leader("7", act { ActivateTab=6 }),
      leader("8", act { ActivateTab=7 }),
      leader("9", act { ActivateTab=-1 }),
      leader("p", act { ActivateTabRelative=-1 }),
      leader("n", act { ActivateTabRelative=1 }),
      -- Fuzzy attach to domain
      -- { key = 'S', mods = 'LEADER', action = act.ShowLauncherArgs { flags = 'FUZZY|WORKSPACES' } },
      leader('S', act.ShowLauncherArgs { flags = 'FUZZY|WORKSPACES' }),

      -- "Detach" from current workspace
      leader('d', act.SwitchToWorkspace { name = 'default' }),

      -- Switch back to last tab
      leader('`', wezterm.action.ActivateLastTab),

      -- Navigate between tabs
      { key = 'LeftArrow', mods = 'SHIFT', action = act.ActivateTabRelative(-1) },
      { key = 'RightArrow', mods = 'SHIFT', action = act.ActivateTabRelative(1) },
    }

    -- if os.execute("[ $(uname) == Darwin ]") then
    --   table.insert(
    --     config.keys,
    --     { key = 'x', mods = 'CMD', action = act.ShowLauncherArgs { flags = 'FUZZY|COMMANDS' } }
    --   )
    -- end
  end
}
