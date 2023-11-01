-- * reload the config if any source file in the config dir changes
function reloadConfig(files)
  doReload = false
  for _,file in pairs(files) do
    if file:sub(-4) == ".lua" or file:sub(-4) == ".fnl" then
      doReload = true
    end
  end

  if doReload then
    hs.reload()
  end
end

local watcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon", reloadConfig):start()

-- * load fennel language and config
local fennel = require("fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)
-- TODO test, enable if working correctly for this version of fennel
-- debug.traceback = fennel.traceback

require("config")
