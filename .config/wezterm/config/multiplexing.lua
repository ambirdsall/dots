local wezterm = require 'wezterm'
local mux = wezterm.mux

wezterm.on('gui-startup', function(cmd)
  local work_dir = wezterm.home_dir .. '/c/airbyte-platform-internal'
  local work_FE_dir = work_dir .. '/oss/airbyte-webapp'

  -- allow `wezterm start -- something` to affect what we spawn
  -- in our initial window
  local args = {}
  if cmd then
    args = cmd.args
  else
    -- Set a workspace for coding on a current project
    -- first argument is the tab object
    local _, airbyte_pane, airbyte_window = mux.spawn_window {
        workspace = 'airbyte',
        cwd = work_FE_dir,
        args = args,
    }
    -- TODO: `send_text 'ssh-add\n'`
    airbyte_window:spawn_tab {
        cwd = work_dir,
    }
    -- queue up a dev server but do not start it for me
    airbyte_pane:send_text 'pnpm start:cloud'


    mux.spawn_window {
        workspace = 'conf',
        cwd = wezterm.home_dir .. '/.config',
    }

    mux.set_active_workspace 'airbyte'
  end
end)

return {
  apply = function(_)
    -- config.unix_domains = {
    --   {
    --     name = "airbyte",
    --   },
    --   {
    --     name = "conf"
    --   },
    -- }
  end
}
