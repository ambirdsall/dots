;; -*- mode: fennel; auto-save-default: nil -*-

(local {: notify
        : alert
        : dump
        : log} (require :notifications))

(local {: puppy-paw
        : bearclaw
        : BEARCLAW
        : spacehammer} (require :keybindings))

(local {: window/center
        : window/fullscreen
        : window/left-half
        : window/right-half
        : window/top-half
        : window/bottom-half
        : window/left
        : window/right
        : window/up
        : window/down} (require :windows))

;; * script loading setup
;; ** load init-local.lua
(let [localfile (.. hs.configdir "/init-local.lua")]
  (and (hs.fs.attributes localfile)
       (dofile localfile)))

(bearclaw :i hs.toggleConsole)
(bearclaw :r hs.reload)

;; ** window management
;; *** switchers

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
(bearclaw :g hs.grid.show)

;; *** custom window management
(bearclaw "=" window/center)

(bearclaw :m window/fullscreen)

(bearclaw :h window/left-half)
(bearclaw :j window/bottom-half)
(bearclaw :k window/top-half)
(bearclaw :l window/right-half)

(puppy-paw :h window/left)
(puppy-paw :j window/down)
(puppy-paw :k window/up)
(puppy-paw :l window/right)

(bearclaw :left window/left-half)
(bearclaw :down window/bottom-half)
(bearclaw :up window/top-half)
(bearclaw :right window/right-half)

(local dev-switcher (hs.window.switcher.new ["Emacs" "WezTerm"]))
(local chrome-windows (hs.window.filter.new ["Emacs" "WezTerm" "Google Chrome"]))
(spacehammer :c #(hs.hints.windowHints (chrome-windows:getWindows)))
(spacehammer :d #(dev-switcher:next))
(hs.console.darkMode true)

;; * You're fresh and you know it. Let 'em know.
(alert "hammer: spooned")
