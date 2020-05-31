;; -*- auto-save-default: nil; -*-

;; * define utility fns
;; ** API wrappers
(fn notify [titleText bodyText]
  "Send a native growl notification with text"
  (: (hs.notify.new {:title titleText :informativeText bodyText}) :send))

(fn alert [text] (hs.alert.show text))

;; * script loading setup
;; ** load init-local.lua
(let [localfile (.. hs.configdir "/init-local.lua")]
  (and (hs.fs.attributes localfile)
       (dofile localfile)))

;; * keybindings
;; ** modifiers and helper fns
(local full-bear-claw [:cmd :alt :ctrl])
(fn bearclaw [key func]
  "bind a function func to run on bearclaw+key"
  (hs.hotkey.bind full-bear-claw key func))

;; ** hammerspoon management
(bearclaw :i hs.toggleConsole)
(bearclaw :r hs.reload)

;; ** random apps
(bearclaw :s hs.spotify.displayCurrentTrack)

;; ** window management
;; currently, the grid is the default 3x3, so grid can be used for 1/3s and bearclaw + hjkl can be
;; used for halves. Might go to a 12x12 grid to get both.
(bearclaw :g hs.grid.show)

(bearclaw :h (fn []
               "Move current window to left half of screen"
               (let [win (hs.window.focusedWindow)
                     f (: win :frame)
                     screen (: win :screen)
                     max (: screen :frame)]

                 (set f.x max.x )
                 (set f.y max.y)
                 (set f.w (/ max.w 2))
                 (set f.h max.h)
                 (: win :setFrame f))))
(bearclaw :j (fn []
               "Move current window to bottom half of screen"
               (let [win (hs.window.focusedWindow)
                     f (: win :frame)
                     screen (: win :screen)
                     max (: screen :frame)]

                 (set f.x max.x )
                 (set f.y (+ max.y (/ max.h 2)))
                 (set f.w max.w)
                 (set f.h (/ max.h 2))
                 (: win :setFrame f))))
(bearclaw :k (fn []
               "Move current window to top half of screen"
               (let [win (hs.window.focusedWindow)
                     f (: win :frame)
                     screen (: win :screen)
                     max (: screen :frame)]

                 (set f.x max.x)
                 (set f.y max.y)
                 (set f.w max.w)
                 (set f.h (/ max.h 2))
                 (: win :setFrame f))))
(bearclaw :l (fn []
               "Move current window to right half of screen"
               (let [win (hs.window.focusedWindow)
                     f (: win :frame)
                     screen (: win :screen)
                     max (: screen :frame)]

                 (set f.x (+ max.x (/ max.w 2)))
                 (set f.y max.y)
                 (set f.w (/ max.w 2))
                 (set f.h max.h)
                 (: win :setFrame f))))
(bearclaw :o (fn []
               "Make current window fullscreen"
               (let [win (hs.window.focusedWindow)
                     f (: win :frame)
                     screen (: win :screen)
                     max (: screen :frame)]

                 (set f.x max.x)
                 (set f.y max.y)
                 (set f.w max.w)
                 (set f.h max.h)
                 (: win :setFrame f))))

;; * You're fresh and you know it. Let 'em know.
(alert "hammer: spooned")
