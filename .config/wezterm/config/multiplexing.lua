-- local wezterm = require 'wezterm'
-- local mux = wezterm.mux

-- wezterm.on('gui-startup', function(cmd)
--   local work_dir = wezterm.home_dir .. '/c/airbyte-platform-internal'
--   local work_FE_dir = work_dir .. '/oss/airbyte-webapp'

--   -- allow `wezterm start -- something` to affect what we spawn
--   -- in our initial window
--   local args = {}
--   if cmd then
--     args = cmd.args
--   else
--     -- TODO really all the below should be in this `else` clause with different, simpler
--     -- defaults for the above
--     args = { "cd", work_FE_dir }
--   end

--   -- Set a workspace for coding on a current project
--   -- Top pane is for the editor, bottom pane is for the build tool
--   local tab, build_pane, window = mux.spawn_window {
--     workspace = 'airbyte',
--     args = args,
--   }
--   window:spawn_tab {
--     cwd = work_dir,
--   }
--   -- queue up a dev server but do not start it for me
--   build_pane:send_text 'pnpm start:cloud'

--   -- A workspace for interacting with a local machine that
--   -- runs some docker containners for home automation
--   local tab, pane, window = mux.spawn_window {
--     workspace = 'conf',
--     cwd = wezterm.home_dir .. '/.config',
--   }

--   -- We want to startup in the coding workspace
--   mux.set_active_workspace 'airbyte'
-- end)

return {
  apply = function(config)
    config.unix_domains = {
      {
        name = "airbyte",
      },
      {
        name = "conf"
      },
    }
  end
}
