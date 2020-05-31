-- see monorail/docs/dev-processes-runit.md

----------
-- Procbar
----------

-- disable a service by renaming to '.service'. it will be displayed on the list but supports no interaction.
-- click on a service to:
--   - if service is running, bounce the service
--   - if service is not running, start the service
-- opt-click on a service to pop open a tail on that service's log
-- shift-click on a service to stop that service

-- copy the following line to your ~/.hammerspoon/init.lua, uncomment it, and set it to a correct path for your system
procbarServiceDir = "/Users/alex.birdsall/c/monorail/procs/"

function procbarViewLogs(service)
  command = "tail -F -n 500 " .. procbarServiceDir .. service .. "/log/logs/current"
  hs.osascript.applescript('tell application "Terminal" to do script "' .. command .. '"')
  hs.application.open('Terminal')
end

function procbarServiceStatus(service)
  local file = procbarServiceDir .. service .. "/supervise/stat"
  local fh = io.open(file)
  local status = fh:read()
  return status
end

function procbarServiceExists(service)
  local runFile = io.open(procbarServiceDir .. service .. "/run", "r")
  if runFile ~= nil then io.close(runFile) return true else return false end
end

function procbarServiceIsOnce(service)
  local onceFile = io.open(procbarServiceDir .. service .. "/once", "r")
  if onceFile ~= nil then io.close(onceFile) return true else return false end
end

function procbarActOnService(op, service)
  hs.execute("/usr/local/bin/sv " .. op .. " " .. procbarServiceDir .. service)
end

function procbarClicked(keyboard, item)
  if keyboard.cmd then
    procbarViewLogs(item.service)
  else
    local action = "up"

    if item.running then
      action = "term"
    end

    if keyboard.shift then
      action = "down"
    end

    if procbarServiceIsOnce(item.service) then
      action = "once"
    end

    procbarActOnService(action, item.service)
  end
end

-- Return (index,string) iterator with sorted list of directory entries in the procs dir (skipping . and ..)
function procbarServicesList()
  local entries = {}
  for entry in hs.fs.dir(procbarServiceDir) do -- https://www.hammerspoon.org/docs/hs.fs.html#dir
    if procbarServiceExists(entry) then
      table.insert(entries, entry)
    end
  end
  table.sort(entries)
  return ipairs(entries)
end

-- Build a table containing menu entries for each service in the procs dir
function procbarServicesMenu()
  statuses = {}
  for i, service in procbarServicesList() do
    local enabled = true

    if string.sub(service,1,1) == "." then
      enabled = false
    end

    local status = procbarServiceStatus(service)
    local running = status == "run"

    table.insert(statuses, {
      title = service,
      disabled = not enabled,
      checked = running,
      fn = procbarClicked,
      service = service,
      running = running
    })
  end
  return statuses
end

local procbar = hs.menubar.new()

function procbarUpdateMenu()
  procbar:setMenu(procbarServicesMenu)
end

if procbar then
  procbar:setTitle("mono")
  procbarUpdateMenu()
end
