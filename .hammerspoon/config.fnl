;; -*- mode: fennel; auto-save-default: nil -*-

(local {: notify
        : alert
        : log
        : bearclaw
        : BEARCLAW
        : dump} (require :utils))

;; * script loading setup
;; ** load init-local.lua
(let [localfile (.. hs.configdir "/init-local.lua")]
  (and (hs.fs.attributes localfile)
       (dofile localfile)))

;; * keybindings
;; ** modifiers and helper fns

;; ** hammerspoon management
(bearclaw :i hs.toggleConsole)
(bearclaw :r hs.reload)

;; ** window management
;; *** switchers
(local dev-switcher (hs.window.switcher.new ["Emacs" "WezTerm"]))
(bearclaw :space #((log "itching for some switching") (dev-switcher:next)))

;; *** open specific apps
;; TODO Make the apps shortcuts toggle instead of unconditionally focusing
(bearclaw :e #(let [emacs (hs.application.get "Emacs")
                    emacs-ng (hs.application.get "emacs")]
                ;; because the directory emacs was launched from matters, don't use
                ;; (hs.application.launchOrFocus "Emacs")
                (if emacs (: emacs :setFrontmost)
                    emacs-ng (: emacs-ng :setFrontmost)
                    (alert "Bruh. How do I focus an emacs GUI if you aren't running one."))))

;; (bearclaw :t #(hs.application.launchOrFocus "iTerm"))
(bearclaw :t #(hs.application.launchOrFocus "WezTerm"))

(bearclaw :s #(hs.application.launchOrFocus "Slack"))

(bearclaw :c #(hs.application.launchOrFocus "Google Chrome"))

(BEARCLAW :c #(let [cypress (hs.application.get "Cypress")]
                ;; because the directory emacs was launched from matters, don't use
                ;; (hs.application.launchOrFocus "Emacs")
                (if cypress (: cypress :setFrontmost)
                    (alert "Bruh. How do I focus cypress if you aren't running it."))))

;; *** grid
(hs.grid.setGrid :3x2)

;; Underdocumented: the grid *must* include at least 5 rows, regardless of how many rows
;; you actually need. Ergo ipso facto, since I only actually want to use
;;     [[:u :i :o] [:j :k :l]],
;; I need to define the grid hints to be those *plus* all the rows above and below. The
;; unused hint keys don't strictly need to be adjacent, but why would I make it
;; unnecessarily painful to change my mind?
(set hs.grid.HINTS [[:f6 :f7 :f8]
                    [:7  :8  :9]
                    [:u  :i  :o]
                    [:j  :k  :l]
                    [:m  ","  :.]])

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

;; TODO: make this toggle between full size and previous.
;; How to store previous size, though: global hashmap?
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

(bearclaw :m window/fullscreen)

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
