;; First we set the grid, then we define helper functions for export
(hs.grid.setGrid :3x2)
;; TODO needs geometry argument
(hs.grid.setMargins [0 0])

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

;; helper functions here on out
(fn current []
  "Returns the frontmost window. current being 'Current window' should be clear from the
  filename, eh?"
  (hs.window.frontmostWindow))

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
    (let [win (current)
          window (win:frame)
          windscreen (win:screen)
          screen (windscreen:frame)]

      (dimension-setter window screen)
      (win:setFrame window))))

(fn window/center [] (: (current) :centerOnScreen))

(fn window/fullscreen [] (: (current) :maximize))

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

(local window/left #(: (current) :focusWindowWest))
(local window/right #(: (current) :focusWindowEast))
(local window/up #(: (current) :focusWindowNorth))
(local window/down #(: (current) :focusWindowSouth))
(local window/enlarge #(hs.grid.resizeWindowWider (current)))
(local window/shrink #(hs.grid.resizeWindowThinner (current)))

{: window/center
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
 : window/shrink}
