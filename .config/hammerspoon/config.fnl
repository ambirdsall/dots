;; -*- mode: fennel; auto-save-default: nil -*-

;; * imports
(local {: notify
        : alert
        : dump
        : log} (require :notifications))

(local {: puppy-paw
        : PUPPY-PAW
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
        : window/down
        : window/enlarge
        : window/shrink} (require :windows))

;; * load init-local.lua and CLI
(let [localfile (.. hs.configdir "/init-local.lua")]
  (and (hs.fs.attributes localfile)
       (dofile localfile)))

(hs.ipc.cliInstall)

;; * bind those keys, make that user env
;; ** hammers of the spoonsmith
(BEARCLAW :r hs.reload)

;; ;; TODO make some kind of fennel repl
(bearclaw :i hs.toggleConsole)
(puppy-paw :x hs.toggleConsole)

;; ** open specific apps
;; ;; TODO Make the apps shortcuts toggle instead of unconditionally focusing
;; shift required so I can use (bearclaw :c) to center a window
(BEARCLAW :c #(hs.application.launchOrFocus "Google Chrome"))
;; (bearclaw :c #(hs.application.launchOrFocus "Google Chrome"))
;; (BEARCLAW :c #(let [cypress (hs.application.get "Cypress")]
;;                 ;; because the directory emacs was launched from matters, don't use
;;                 ;; (hs.application.launchOrFocus "Emacs")
;;                 (if cypress (: cypress :setFrontmost)
;;                     (alert "Bruh. How do I focus cypress if you aren't running it."))))
(bearclaw :e #(let [emacs (hs.application.get "Emacs")
                    emacs-ng (hs.application.get "emacs")]
                ;; because the directory emacs was launched from matters, don't use
                ;; (hs.application.launchOrFocus "Emacs")
                (if emacs (: emacs :setFrontmost)
                    emacs-ng (: emacs-ng :setFrontmost)
                    (alert "Bruh. How do I focus an emacs GUI if you aren't running one."))))
(BEARCLAW :m #(hs.application.launchOrFocus "Messages"))

;; ;; (bearclaw :t #(hs.application.launchOrFocus "iTerm"))
(bearclaw :s #(hs.application.launchOrFocus "Slack"))
;; TODO dedup lmao
;; *** wezterm, let's go to weztown
;; TODO extract to a module and don't import
(fn in-current-space [window]
  (let [focused-space (hs.spaces.focusedSpace)
        window-spaces (hs.spaces.windowSpaces window)]
    (when (and focused-space window-spaces)
      (each [_ space (ipairs window-spaces)]
        (when (= space focused-space) (lua "return true"))))
    false))

;; TODO extract to a module and import
(fn make-dropdown-toggle [app-name new-window-menu-path]
  (fn []
    (var app (hs.application.get app-name))
    (if app (if (not (app:mainWindow)) (app:selectMenuItem new-window-menu-path)
                (not (in-current-space (app:mainWindow))) (do (hs.spaces.moveWindowToSpace (app:mainWindow) (hs.spaces.focusedSpace))
                                                              (app:setFrontmost))
                (app:isFrontmost) (app:hide)
                (app:activate))
        (do
          (hs.application.launchOrFocus app-name)
          (set app (hs.application.get app-name))))
    (: (app:mainWindow) :moveToUnit "[100,100,0,0]")))

(local wezterm-dropdown-toggle (make-dropdown-toggle :wezterm-gui [:Shell "New Window"]))
(bearclaw :t wezterm-dropdown-toggle)
(puppy-paw :t wezterm-dropdown-toggle)
(puppy-paw :return wezterm-dropdown-toggle)

;; (puppy-paw :z #(hs.application.launchOrFocus "zoom.us"))

;; ** window management
(bearclaw :g hs.grid.show)
;; (puppy-paw "." window/enlarge)
;; (puppy-paw "," window/shrink)

(bearclaw :c window/center)

(bearclaw :m window/fullscreen)

(bearclaw :h window/left-half)
(bearclaw :j window/bottom-half)
(bearclaw :k window/top-half)
(bearclaw :l window/right-half)

(puppy-paw :h window/left)
(puppy-paw :j window/down)
(puppy-paw :k window/up)
(puppy-paw :l window/right)
;; (bearclaw :h window/left)
;; (bearclaw :j window/down)
;; (puppy-paw :k window/up)
;; (puppy-paw :l window/right)

(bearclaw :left window/left-half)
(bearclaw :down window/bottom-half)
(bearclaw :up window/top-half)
(bearclaw :right window/right-half)

;; * PaperWM
;; (local PaperWM (hs.loadSpoon :PaperWM))

;; ;; ** configure PaperWM variables
;; (set PaperWM.window_gap 4)
;; ;; ¼, golden/ratio, halfsies, ratio/golden, ¾, 95%, fill 'er up
;; (set PaperWM.window_ratios [0.25 0.38195 0.5 0.61804 0.75 0.95 1])

;; ;; ** set up toggle function
;; (var paperwm-on false)
;; (fn start-paperwm []
;;   (set paperwm-on true)
;;   (PaperWM:start))
;; (fn stop-paperwm []
;;   (set paperwm-on false)
;;   (PaperWM:stop))
;; (fn toggle-paperwm []
;;   (if paperwm-on
;;       (stop-paperwm)
;;       (start-paperwm)))

;; ;; ** keybindings
;; (PaperWM:bindHotkeys {:focus_left     [[:ctrl :alt :cmd] :h]
;;                       :focus_down     [[:ctrl :alt :cmd] :j]
;;                       :focus_up       [[:ctrl :alt :cmd] :k]
;;                       :focus_right    [[:ctrl :alt :cmd] :l]

;;                       :swap_left      [[:ctrl :alt :cmd :shift] :h]
;;                       :swap_down      [[:ctrl :alt :cmd :shift] :j]
;;                       :swap_up        [[:ctrl :alt :cmd :shift] :k]
;;                       :swap_right     [[:ctrl :alt :cmd :shift] :l]

;;                       :full_width     [[:ctrl :alt :cmd] :f]
;;                       :center_window  [[:ctrl :alt :cmd] :c]

;;                       :switch_space_1 [[:ctrl :alt :cmd] :1]
;;                       :switch_space_2 [[:ctrl :alt :cmd] :2]
;;                       :switch_space_3 [[:ctrl :alt :cmd] :3]
;;                       :switch_space_4 [[:ctrl :alt :cmd] :4]
;;                       :switch_space_5 [[:ctrl :alt :cmd] :5]
;;                       :switch_space_6 [[:ctrl :alt :cmd] :6]
;;                       :switch_space_7 [[:ctrl :alt :cmd] :7]
;;                       :switch_space_8 [[:ctrl :alt :cmd] :8]
;;                       :switch_space_9 [[:ctrl :alt :cmd] :9]

;;                       :move_window_1  [[:ctrl :alt :cmd :shift] :1]
;;                       :move_window_2  [[:ctrl :alt :cmd :shift] :2]
;;                       :move_window_3  [[:ctrl :alt :cmd :shift] :3]
;;                       :move_window_4  [[:ctrl :alt :cmd :shift] :4]
;;                       :move_window_5  [[:ctrl :alt :cmd :shift] :5]
;;                       :move_window_6  [[:ctrl :alt :cmd :shift] :6]
;;                       :move_window_7  [[:ctrl :alt :cmd :shift] :7]
;;                       :move_window_8  [[:ctrl :alt :cmd :shift] :8]
;;                       :move_window_9  [[:ctrl :alt :cmd :shift] :9]

;;                       :slurp_in       [[:ctrl :alt :cmd] :i]
;;                       :barf_out       [[:ctrl :alt :cmd] :o]})

;; (puppy-paw :left PaperWM.actions.focus_left)
;; (PUPPY-PAW :left PaperWM.actions.swap_left)
;; (bearclaw :left PaperWM.actions.focus_left)
;; (puppy-paw :right PaperWM.actions.focus_right)
;; (PUPPY-PAW :right PaperWM.actions.swap_right)
;; (bearclaw :right PaperWM.actions.focus_right)
;; (bearclaw :up PaperWM.actions.focus_up)
;; (puppy-paw :up PaperWM.actions.focus_up)
;; (PUPPY-PAW :up PaperWM.actions.swap_up)
;; (bearclaw :down PaperWM.actions.focus_down)
;; (puppy-paw :down PaperWM.actions.focus_down)
;; (PUPPY-PAW :down PaperWM.actions.swap_down)
;; (bearclaw :r PaperWM.actions.cycle_width)
;; (puppy-paw :r PaperWM.actions.reverse_cycle_width)

;; (BEARCLAW :p toggle-paperwm)

;; ** let 'er rip
;(start-paperwm)

;; * now, just to make it extra likely shit will break, spacehammer
;; (local Spacehammer (hs.loadSpoon :Spacehammer))
;; (Spacehammer:start)

;; * You're fresh and you know it. Let 'em know.
(alert "hammer: spooned")
