;; * define utility fns
;; ** API wrappers
(fn notify [titleText bodyText]
  "Send a native growl notification with text"
  (: (hs.notify.new {:title titleText :informativeText bodyText}) :send))

(fn alert [text] (hs.alert.show text))

(local logger (hs.logger.new :amb :debug))
(fn log [message] (logger:i message))

(fn bearclaw [key func]
  "bind a function func to run on bearclaw+key"
  (hs.hotkey.bind [:cmd :alt :ctrl] key func))

(fn shift+bearclaw [key func]
  "bind a function func to run on shift+bearclaw+key"
  (hs.hotkey.bind [:shift :cmd :alt :ctrl] key func))

{:notify notify
 :alert alert
 :log log
 :bearclaw bearclaw
 :shift+bearclaw shift+bearclaw}
