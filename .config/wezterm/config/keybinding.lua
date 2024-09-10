local wezterm = require "wezterm"
local act = wezterm.action

local leader = function(key, action)
  return { key=key, mods="LEADER", action=action }
end

local unbind = function(key, mods)
  return { key=key, mods=mods, action=act.DisableDefaultAssignment }
end

-- TODO unbind shift+<arrow keys>
return {
  apply = function(config)
    -- config.enable_csi_u_key_encoding = true
    config.enable_kitty_keyboard = true
    config.leader = { key = "Space", mods = "CTRL", timeout_milliseconds = 1000 }

    -- gimme my alt back
    -- TODO conditionally tangle this sumbitch based on `uname` once I get an org file going for this config
    config.send_composed_key_when_left_alt_is_pressed = false
    config.send_composed_key_when_right_alt_is_pressed = false

    -- okay now we go ham
    config.keys = {
      -- there are some defaults that jam my shit up
      unbind('Enter', 'ALT'),
      unbind('Tab', 'CTRL'),
      { key = 'Enter', mods = 'SUPER', action = act.ToggleFullScreen },

      -- now, onto the fun stuff
      { key = 'x', mods = 'SUPER', action = act.ShowLauncherArgs { flags = 'FUZZY|COMMANDS' } },

      leader('Space', act.ActivateCopyMode),
      leader('t', act.EmitEvent 'toggle-colorscheme'),

      -- open/shut them, open/shut them, give a little clap clap clap
      leader("c", act.SpawnTab "CurrentPaneDomain"),
      leader("s", act.SplitVertical { domain='CurrentPaneDomain' }),
      leader("v", act.SplitHorizontal { domain='CurrentPaneDomain' }),
      leader("z", act.TogglePaneZoomState),
      leader("x", act.CloseCurrentPane { confirm=false }),
      leader("b", wezterm.action_callback(function(win, pane)
                 local tab, window = pane.move_to_new_tab()
      end)),

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

      leader('`', act.ActivateLastTab),

      -- navigate and swap panes ace-window style
      leader("w", act.PaneSelect),
      leader("W", act.PaneSelect { mode = "SwapWithActive" }),

      -- rename tab
      leader(",", act.PromptInputLine {
        description = "Enter new name for tab",
        action = wezterm.action_callback(function(window, pane, line)
          if line and #line > 0 then
            window:active_tab():set_title(line)
          end
        end),
      }),

      -- resizing panes
      -- (I don't know how to accomplish these yet)
      -- leader("=", equalize pane sizes somehow),
      -- leader("g", set current pane width to reciprocal of golden ratio vis-a-vis its containing window),

      -- TODO Workspaces (make them work for my tmux-addled brain)
      -- { key = 'S', mods = 'LEADER', action = act.ShowLauncherArgs { flags = 'FUZZY|WORKSPACES' } },
      leader('S', act.ShowLauncherArgs { flags = 'FUZZY|WORKSPACES' }),

      -- "Detach" from current workspace
      leader('d', act.SwitchToWorkspace { name = 'default' }),

      -- Switch back to prior workspace
      -- n.b. this should be "previous" workspace in a stack, but that's not how the
      -- underlying code works, so it's just cyclically iterating through a list
      leader('L', act.SwitchWorkspaceRelative(-1)),

    }

    -- if os.execute("[ $(uname) == Darwin ]") then
    --   table.insert(
    --     config.keys,
    --     { key = 'x', mods = 'CMD', action = act.ShowLauncherArgs { flags = 'FUZZY|COMMANDS' } }
    --   )
    -- end
  end
}
