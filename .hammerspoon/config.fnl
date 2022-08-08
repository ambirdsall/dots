;; -*- mode: fennel; auto-save-default: nil -*-

;; * define utility fns
;; ** API wrappers
(fn notify [titleText bodyText]
  "Send a native growl notification with text"
  (: (hs.notify.new {:title titleText :informativeText bodyText}) :send))

(fn alert [text] (hs.alert.show text))

(local logger (hs.logger.new :amb :debug))
(fn log [message] (logger:i message))

;; * script loading setup
;; ** load init-local.lua
(let [localfile (.. hs.configdir "/init-local.lua")]
  (and (hs.fs.attributes localfile)
       (dofile localfile)))

;; * keybindings
;; ** modifiers and helper fns
(fn bearclaw [key func]
  "bind a function func to run on bearclaw+key"
  (hs.hotkey.bind [:cmd :alt :ctrl] key func))

(fn shift+bearclaw [key func]
  "bind a function func to run on shift+bearclaw+key"
  (hs.hotkey.bind [:shift :cmd :alt :ctrl] key func))

;; ** hammerspoon management
(bearclaw :i hs.toggleConsole)
(bearclaw :r hs.reload)

;; ** window management
;; *** switchers
(local dev-switcher (hs.window.switcher.new ["Emacs" "iTerm"]))
(bearclaw :d #((log "itching to do some dev switching") (dev-switcher:next)))
(shift+bearclaw :d #((log "switching to do some dev, wait...") (dev-switcher:previous)))

;; *** open specific apps
;; TODO Make the apps shortcuts toggle instead of unconditionally focusing
(bearclaw :e #(let [emacs (hs.application.get "Emacs")
                    emacs-ng (hs.application.get "emacs")]
                ;; because the directory emacs was launched from matters, don't use
                ;; (hs.application.launchOrFocus "Emacs")
                (if emacs (: emacs :setFrontmost)
                    emacs-ng (: emacs-ng :setFrontmost)
                    (alert "Bruh. How do I focus an emacs GUI if you aren't running one."))))

(bearclaw :t #(hs.eventtap.keyStroke [:alt] :space))
(shift+bearclaw :t #(hs.application.launchOrFocus "iTerm"))

(bearclaw :s #(hs.application.launchOrFocus "Slack"))

(bearclaw :c #(hs.application.launchOrFocus "Google Chrome"))

(shift+bearclaw :c #(let [cypress (hs.application.get "Cypress")]
                ;; because the directory emacs was launched from matters, don't use
                ;; (hs.application.launchOrFocus "Emacs")
                (if cypress (: cypress :setFrontmost)
                    (alert "Bruh. How do I focus cypress if you aren't running it."))))

;; *** grid
;; currently, the grid is the default 3x3, so grid can be used for 1/3s and bearclaw + hjkl can be
;; used for halves. Might go to a 12x12 grid to get both.
(bearclaw :g hs.grid.show)

;; *** custom window management

(fn window-resizer [dimension-setter]
  "Returns a function which will resize the focused window based on the
`dimension-setter` function you pass in. It is
called with two arguments: the focused window's frame, used to set the
new dimensions; and the screen frame, a reference for calculating the intended dimensions.

there are four numbers giving a window's position:
x :: top-left corner -> left edge of screen
y :: top-left corner -> top of screen 
w :: bottom-right corner -> left edge of screen
h :: bottom-right corner -> top of screen
"
  (fn []
    (let [win (hs.window.focusedWindow)
              window (win:frame)
              windscreen (win:screen)
              screen (windscreen:frame)]

      (dimension-setter window screen)
      (win:setFrame window))))

(local window/center
       (fn []
         (let [win (hs.window.focusedWindow)
               window (win:frame)
               windscreen (win:screen)
               screen (windscreen:frame)

               ;; TODO: calling this function on an already-centered window
               ;; shifts it off-center and that's lame
               pixel-width (- window.w window.x)
               pixel-height (- window.h window.y)
               new-x (/ (- screen.w pixel-width) 2)
               new-y (/ (- screen.h pixel-height) 2)]

           (set window.x new-x)
           (set window.y new-y)

           (win:setFrame window))))

(bearclaw "=" window/center)

(local window/fullscreen
       (window-resizer (fn [window screen]
                         (set window.x screen.x)
                         (set window.y screen.y)
                         (set window.w screen.w)
                         (set window.h screen.h))))

(local window/left-half
       (window-resizer (fn [window screen]
                         (set window.x screen.x)
                         (set window.y screen.y)
                         (set window.w (/ screen.w 2))
                         (set window.h screen.h))))

(local window/bottom-half
       (window-resizer (fn [window screen]
                         (set window.x screen.x )
                         (set window.y (+ screen.y (/ screen.h 2)))
                         (set window.w screen.w)
                         (set window.h (/ screen.h 2)))))

(local window/top-half
       (window-resizer (fn [window screen]
                         (set window.x screen.x)
                         (set window.y screen.y)
                         (set window.w screen.w)
                         (set window.h (/ screen.h 2)))))

(local window/right-half
       (window-resizer (fn [window screen]
                         (set window.x (+ screen.x (/ screen.w 2)))
                         (set window.y screen.y)
                         (set window.w (/ screen.w 2))
                         (set window.h screen.h))))

(bearclaw :return window/fullscreen)

(bearclaw :h window/left-half)
(bearclaw :j window/bottom-half)
(bearclaw :k window/top-half)
(bearclaw :l window/right-half)

(bearclaw :left window/left-half)
(bearclaw :down window/bottom-half)
(bearclaw :up window/top-half)
(bearclaw :right window/right-half)

;; * You're fresh and you know it. Let 'em know.
(alert "hammer: spooned")
