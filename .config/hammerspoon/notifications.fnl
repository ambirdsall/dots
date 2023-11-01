(fn notify [titleText bodyText]
  "Send a native growl notification with text"
  (: (hs.notify.new {:title titleText :informativeText bodyText}) :send))

(fn alert [text] (hs.alert.show text))

(local logger (hs.logger.new :amb :debug))
(fn log [message] (logger:i message))

(fn dump [o]
  (if (= (type o) :table)
      (do
        (var s "{ ")
        (each [key v (pairs o)]
          (var k key)
          (if (~= (type key) :number)
              (set k (.. "\"" k "\"")))
          (set s (.. s "[" k "] = " (dump v) ",")))
        (.. s "} "))
      (tostring o)))

{: notify
 : alert
 : dump
 : log}
