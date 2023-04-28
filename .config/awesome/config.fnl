(pcall require :luarocks.loader)

(local fennel (require :fennel))
(local gears (require :gears))
(local awful (require :awful))
(require :awful.autofocus)
(local wibox (require :wibox))
(local beautiful (require :beautiful))
(local naughty (require :naughty))
(local menubar (require :menubar))
(local hotkeys-popup (require :awful.hotkeys_popup))
(require :awful.hotkeys_popup.keys)

(when awesome.startup_errors
  (naughty.notify {:preset naughty.config.presets.critical
                   :text awesome.startup_errors
                   :title "Shit, there were errors during startup!"}))

(do
  (var in-error false)
  (awesome.connect_signal "debug::error"
                          (fn [err]
                            (when in-error (lua "return "))
                            (set in-error true)
                            (naughty.notify {:preset naughty.config.presets.critical
                                             :text (tostring err)
                                             :title "Oops, an error happened!"})
                            (set in-error false))))

(let [theme-path (string.format "%s/.config/awesome/themes/%s/theme.lua"
                                (os.getenv :HOME) :zenburn)]
  (beautiful.init theme-path))

(set beautiful.wallpaper
     (string.format "%s/Pictures/crescent-pluto.jpg" (os.getenv :HOME)))

(global terminal :kitty)
(global editor (or (os.getenv :EDITOR) :emacs))
(global editor-cmd (.. terminal " -e " editor))

(global modkey :Mod4)

(set awful.layout.layouts
     [awful.layout.suit.spiral.dwindle
      awful.layout.suit.max
      ;; awful.layout.suit.floating
      awful.layout.suit.magnifier
      awful.layout.suit.corner.nw])

(global myawesomemenu [[:hotkeys #(hotkeys-popup.show_help nil (awful.screen.focused))]
                       [:manual (.. terminal " -e man awesome")]
                       ["edit config" (.. editor-cmd " " awesome.conffile)]
                       [:restart awesome.restart]
                       [:quit #(awesome.quit)]])

(global mymainmenu
        (awful.menu {:items [[:awesome myawesomemenu beautiful.awesome_icon]
                             ["open terminal" terminal]]}))

(global mylauncher
        (awful.widget.launcher {:image beautiful.awesome_icon :menu mymainmenu}))

(set menubar.utils.terminal terminal)

(global mykeyboardlayout (awful.widget.keyboardlayout))

(global mytextclock (wibox.widget.textclock))

(local taglist-buttons
       (gears.table.join (awful.button {} 1 (fn [t] (t:view_only)))
                         (awful.button [modkey] 1
                                       (fn [t]
                                         (when client.focus
                                           (client.focus:move_to_tag t))))
                         (awful.button {} 3 awful.tag.viewtoggle)
                         (awful.button [modkey] 3
                                       (fn [t]
                                         (when client.focus
                                           (client.focus:toggle_tag t))))
                         (awful.button {} 4
                                       (fn [t] (awful.tag.viewnext t.screen)))
                         (awful.button {} 5
                                       (fn [t] (awful.tag.viewprev t.screen)))))

(local tasklist-buttons
       (gears.table.join (awful.button {} 1
                                       (fn [c]
                                         (if (= c client.focus)
                                             (set c.minimized true)
                                             (c:emit_signal "request::activate"
                                                            :tasklist
                                                            {:raise true}))))
                         (awful.button {} 3 #(awful.menu.client_list {:theme {:width 250}}))
                         (awful.button {} 4 #(awful.client.focus.byidx 1))
                         (awful.button {} 5 #(awful.client.focus.byidx (- 1)))))

(fn set-wallpaper [s]
  (if beautiful.wallpaper
      (do
        (var wallpaper beautiful.wallpaper)
        (when (= (type wallpaper) :function) (set wallpaper (wallpaper s)))
        (gears.wallpaper.maximized wallpaper s true))
      (gears.wallpaper.maximized "~/Pictures/crescent-pluto.jpg" s true)))

(screen.connect_signal "property::geometry" set-wallpaper)

(awful.screen.connect_for_each_screen
 (fn [s]
   (set-wallpaper s)
   (awful.tag [:1 :2 :3 :4 :5 :6 :7 :8 :9]
              s (. awful.layout.layouts 1))
   (set s.mypromptbox (awful.widget.prompt))
   (set s.mylayoutbox (awful.widget.layoutbox s))
   (s.mylayoutbox:buttons (gears.table.join
                           (awful.button {} 1 #(awful.layout.inc 1))
                           (awful.button {} 3 #(awful.layout.inc (- 1)))
                           (awful.button {} 4 #(awful.layout.inc 1))
                           (awful.button {} 5 #(awful.layout.inc (- 1)))))
   (set s.mytaglist
        (awful.widget.taglist {:buttons taglist-buttons
                               :filter awful.widget.taglist.filter.all
                               :screen s}))
   (set s.mytasklist
        (awful.widget.tasklist {:buttons tasklist-buttons
                                :filter awful.widget.tasklist.filter.currenttags
                                :screen s}))
   (set s.mywibox
        (awful.wibar {:position :top
                      :screen s}))
   (s.mywibox:setup {1 {1 mylauncher
                        2 s.mytaglist
                        3 s.mypromptbox
                        :layout wibox.layout.fixed.horizontal}
                     2 s.mytasklist
                     3 {1 mykeyboardlayout
                        2 (wibox.widget.systray)
                        3 mytextclock
                        4 s.mylayoutbox
                        :layout wibox.layout.fixed.horizontal}
                     :layout wibox.layout.align.horizontal})))

(root.buttons (gears.table.join (awful.button {} 3 #(mymainmenu:toggle))
                                (awful.button {} 4 awful.tag.viewnext)
                                (awful.button {} 5 awful.tag.viewprev)))

(fn super [key func menu-spec]
  "Set a keybinding with the super modifier"
  (awful.key [:Mod4] key func menu-spec))

(fn bearclaw [key func menu-spec]
  "Set a keybinding with the super modifier"
  (awful.key [:Control :Mod4 :Mod1] key func menu-spec))

(macro sh [cmd]
  "Define a function which calls a shell command with a minimum of typing"
  `(fn []
    (awful.spawn ,cmd)))

(global globalkeys
        (gears.table.join
         (awful.key [] :XF86MonBrightnessUp (sh "xbacklight +5"))
         (awful.key [] :XF86MonBrightnessDown (sh "xbacklight -5"))
         (bearclaw :k (sh "/home/amb/bin/kbd-backlight-on"))
         (bearclaw :j (sh "/home/amb/bin/kbd-backlight-off"))
         (awful.key [] :XF86AudioPlay (sh "playerctl play"))
         (awful.key [] :XF86AudioPause (sh "playerctl pause"))
         (awful.key [] :XF86AudioNext (sh "playerctl next"))
         (awful.key [] :XF86AudioPrev (sh "playerctl previous"))
         (awful.key [] :XF86AudioMute (sh "amixer sset Master toggle"))
         (super :d (sh "rofi -modi drun -show drun -config ~/.config/rofi/rofidmenu.rasi"))

         (super :s hotkeys-popup.show_help
                {:description "show help" :group :awesome})
         (super :Left awful.tag.viewprev
                {:description "view previous" :group :tag})
         (super :Right awful.tag.viewnext
                {:description "view next" :group :tag})
         (super :Escape awful.tag.history.restore
                {:description "go back" :group :tag})
         (super :j #(awful.client.focus.byidx 1)
                {:description "focus next by index"
                 :group :client})
         (super :k #(awful.client.focus.byidx (- 1))
                {:description "focus previous by index"
                 :group :client})
         (super :w #(mymainmenu:show)
                {:description "show main menu"
                 :group :awesome})
         (awful.key [modkey :Shift] :j #(awful.client.swap.byidx 1)
                    {:description "swap with next client by index"
                     :group :client})
         (awful.key [modkey :Shift] :k #(awful.client.swap.byidx (- 1))
                    {:description "swap with previous client by index"
                     :group :client})
         (awful.key [modkey :Control] :j #(awful.screen.focus_relative 1)
                    {:description "focus the next screen"
                     :group :screen})
         (awful.key [modkey :Control] :k #(awful.screen.focus_relative (- 1))
                    {:description "focus the previous screen"
                     :group :screen})
         (super :u awful.client.urgent.jumpto
                {:description "jump to urgent client"
                 :group :client})
         (super :Tab
                (fn []
                  (awful.client.focus.history.previous)
                  (when client.focus (client.focus:raise)))
                {:description "go back" :group :client})
         (super :Return
                (sh terminal)
                {:description "open a terminal"
                 :group :launcher})
         (awful.key [modkey :Control] :r awesome.restart
                    {:description "reload awesome"
                     :group :awesome})
         (awful.key [modkey :Shift] :q awesome.quit
                    {:description "quit awesome"
                     :group :awesome})
         (super :l #(awful.tag.incmwfact 0.05)
                {:description "increase master width factor"
                 :group :layout})
         (super :h #(awful.tag.incmwfact (- 0.05))
                {:description "decrease master width factor"
                 :group :layout})
         (awful.key [modkey :Shift] :h #(awful.tag.incnmaster 1 nil true)
                    {:description "increase the number of master clients"
                     :group :layout})
         (awful.key [modkey :Shift] :l #(awful.tag.incnmaster (- 1) nil true)
                    {:description "decrease the number of master clients"
                     :group :layout})
         (awful.key [modkey :Control] :h #(awful.tag.incncol 1 nil true)
                    {:description "increase the number of columns"
                     :group :layout})
         (awful.key [modkey :Control] :l #(awful.tag.incncol (- 1) nil true)
                    {:description "decrease the number of columns"
                     :group :layout})
         (super :space #(awful.layout.inc 1)
                {:description "select next"
                 :group :layout})
         (awful.key [modkey :Shift] :space #(awful.layout.inc (- 1))
                    {:description "select previous"
                     :group :layout})
         (awful.key [modkey :Control] :n #(let [c (awful.client.restore)]
                                            (when c
                                              (c:emit_signal "request::activate"
                                                             :key.unminimize
                                                             {:raise true})))
                    {:description "restore minimized"
                     :group :client})
         (super :r #(awful.util.spawn :dmenu_run)
                {:description "run prompt"
                 :group :launcher})
         (super :x #(awful.prompt.run {:exe_callback #(awful.util.eval (fennel.compileString $1))
                                       :history_path (.. (awful.util.get_cache_dir)
                                                         :/history_fennel-eval)
                                       :prompt "Run Fennel code: "
                                       :textbox (. (awful.screen.focused)
                                                   :mypromptbox
                                                   :widget)}))
         ;; (super :x #(awful.prompt.run {:exe_callback awful.util.eval
         ;;                               :history_path (.. (awful.util.get_cache_dir)
         ;;                                                 :/history_eval)
         ;;                               :prompt "Run Lua code: "
         ;;                               :textbox (. (awful.screen.focused)
         ;;                                           :mypromptbox
         ;;                                           :widget)})
         ;;        {:description "lua execute prompt"
         ;;         :group :awesome})
         (awful.key [modkey :Shift] :d #(menubar.show)
                {:description "show the menubar"
                 :group :launcher})
         (awful.key [modkey :Shift] :e (sh :emoji-picker)
                    {:description "emojis"
                     :group :launcher})
         (awful.key [:Mod1] :Tab
                    (fn []
                      (awful.client.focus.history.previous)
                      (when client.focus (client.focus:raise)))
                    {:description "switch between clients"
                     :group :client})))

(global clientkeys
        (gears.table.join
         (super :f
                (fn [c]
                  (set c.fullscreen (not c.fullscreen))
                  (c:raise))
                {:description "toggle fullscreen"
                 :group :client})
         (super :q (fn [c] (c:kill))
                {:description :close :group :client})
         (awful.key [modkey :Control] :space
                    awful.client.floating.toggle
                    {:description "toggle floating"
                     :group :client})
         (awful.key [modkey :Control] :Return
                    (fn [c] (c:swap (awful.client.getmaster)))
                    {:description "move to master"
                     :group :client})
         (super :o (fn [c] (c:move_to_screen))
                {:description "move to screen"
                 :group :client})
         (super :t
                (fn [c] (set c.ontop (not c.ontop)))
                {:description "toggle keep on top"
                 :group :client})
         (super :n
                (fn [c] (set c.minimized true))
                {:description :minimize :group :client})
         (super :m
                (fn [c]
                  (set c.maximized (not c.maximized))
                  (c:raise))
                {:description "(un)maximize"
                 :group :client})
         (awful.key [modkey :Control] :m
                    (fn [c]
                      (set c.maximized_vertical
                           (not c.maximized_vertical))
                      (c:raise))
                    {:description "(un)maximize vertically"
                     :group :client})
         (awful.key [modkey :Shift] :m
                    (fn [c]
                      (set c.maximized_horizontal
                           (not c.maximized_horizontal))
                      (c:raise))
                    {:description "(un)maximize horizontally"
                     :group :client})))

(for [i 1 9]
  (global globalkeys
          (gears.table.join
           globalkeys
           (super (.. "#" (+ i 9))
                  (fn []
                    (let [screen (awful.screen.focused)
                          tag (. screen.tags i)]
                      (when tag (tag:view_only))))
                  {:description (.. "view tag #" i)
                   :group :tag})
           (awful.key [modkey :Control] (.. "#" (+ i 9))
                      (fn []
                        (let [screen (awful.screen.focused)
                              tag (. screen.tags i)]
                          (when tag (awful.tag.viewtoggle tag))))
                      {:description (.. "toggle tag #" i)
                       :group :tag})
           (awful.key [modkey :Shift] (.. "#" (+ i 9))
                      (fn []
                        (when client.focus
                          (local tag
                                 (. client.focus.screen.tags i))
                          (when tag
                            (client.focus:move_to_tag tag))))
                      {:description (.. "move focused client to tag #"
                                        i)
                       :group :tag})
           (awful.key [modkey :Control :Shift]
                      (.. "#" (+ i 9))
                      (fn []
                        (when client.focus
                          (local tag
                                 (. client.focus.screen.tags i))
                          (when tag
                            (client.focus:toggle_tag tag))))
                      {:description (.. "toggle focused client on tag #"
                                        i)
                       :group :tag}))))

(global clientbuttons
        (gears.table.join
         (awful.button {} 1
                       (fn [c]
                         (c:emit_signal "request::activate"
                                        :mouse_click
                                        {:raise true})))
         (awful.button [modkey] 1
                       (fn [c]
                         (c:emit_signal "request::activate"
                                        :mouse_click
                                        {:raise true})
                         (awful.mouse.client.move c)))
         (awful.button [modkey] 3
                       (fn [c]
                         (c:emit_signal "request::activate"
                                        :mouse_click
                                        {:raise true})
                         (awful.mouse.client.resize c)))))

(root.keys globalkeys)

(set awful.rules.rules
     [{:properties {:border_color beautiful.border_normal
                    :border_width beautiful.border_width
                    :buttons clientbuttons
                    :focus awful.client.focus.filter
                    :keys clientkeys
                    :placement (+ awful.placement.no_overlap
                                  awful.placement.no_offscreen)
                    :raise true
                    :screen awful.screen.preferred}
       :rule {}}
      {:properties {:floating true}
       :rule_any {:class [:Arandr
                          :Blueman-manager
                          :Gpick
                          :Kruler
                          :MessageWin
                          :Sxiv
                          "Tor Browser"
                          :Wpa_gui
                          :veromix
                          :xtightvncviewer]
                  :instance [:DTA :copyq :pinentry]
                  :name ["Event Tester"]
                  :role [:AlarmWindow :ConfigManager :pop-up]}}
      {:properties {:titlebars_enabled true}
       :rule_any {:type [:normal :dialog]}}])

(client.connect_signal :manage
                       (fn [c]
                         (when (and (and awesome.startup
                                         (not c.size_hints.user_position))
                                    (not c.size_hints.program_position))
                           (awful.placement.no_offscreen c))))

(client.connect_signal "request::titlebars"
                       (fn [c]
                         (let [buttons (gears.table.join (awful.button {} 1
                                                                       (fn []
                                                                         (c:emit_signal "request::activate"
                                                                                        :titlebar
                                                                                        {:raise true})
                                                                         (awful.mouse.client.move c)))
                                                         (awful.button {} 3
                                                                       (fn []
                                                                         (c:emit_signal "request::activate"
                                                                                        :titlebar
                                                                                        {:raise true})
                                                                         (awful.mouse.client.resize c))))]
                           (: (awful.titlebar c) :setup
                              {1 {1 (awful.titlebar.widget.iconwidget c)
                                  : buttons
                                  :layout wibox.layout.fixed.horizontal}
                               2 {1 {:align :center
                                     :widget (awful.titlebar.widget.titlewidget c)}
                                  : buttons
                                  :layout wibox.layout.flex.horizontal}
                               3 {1 (awful.titlebar.widget.floatingbutton c)
                                  2 (awful.titlebar.widget.maximizedbutton c)
                                  3 (awful.titlebar.widget.stickybutton c)
                                  4 (awful.titlebar.widget.ontopbutton c)
                                  5 (awful.titlebar.widget.closebutton c)
                                  :layout (wibox.layout.fixed.horizontal)}
                               :layout wibox.layout.align.horizontal}))))

(client.connect_signal "mouse::enter"
                       (fn [c]
                         (c:emit_signal "request::activate" :mouse_enter
                                        {:raise false})))

(client.connect_signal :focus
                       (fn [c] (set c.border_color beautiful.border_focus)))

(client.connect_signal :unfocus
                       (fn [c] (set c.border_color beautiful.border_normal)))
