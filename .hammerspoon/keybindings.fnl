(local {: alert} (require :notifications))
(local ModalMgr (hs.loadSpoon "ModalMgr"))
;; Valid strings are any single-character string, or any of the following strings:
;; - f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12,
;;   f13, f14, f15, f16, f17, f18, f19, f20,
;; - pad., pad*, pad+, pad/, pad-, pad=,
;; - pad0, pad1, pad2, pad3, pad4, pad5, pad6, pad7, pad8, pad9,
;; - padclear, padenter, return, tab, space, delete, escape, help,
;; - home, pageup, forwarddelete, end, pagedown, left, right, down, up,
;; - shift, rightshift, cmd, rightcmd, alt, rightalt, ctrl, rightctrl,
;; - capslock, fn
(fn puppy-paw [key func]
  "bind a function func to run on bearclaw+key"
  (hs.hotkey.bind [:cmd :alt] key func))

(fn bearclaw [key func]
  "bind a function func to run on bearclaw+key"
  (hs.hotkey.bind [:cmd :alt :ctrl] key func))

(fn BEARCLAW [key func]
  "bind a function func to run on shift+bearclaw+key"
  (hs.hotkey.bind [:shift :cmd :alt :ctrl] key func))

;; TODO which-key- or rofi-style pop-up hints
(fn def-modal-with-timeout [timeout mods key]
  (let [modal (hs.hotkey.modal.new mods key)]
    (var timer (hs.timer.new 4 #(modal:exit)))
    (tset modal :entered #(timer:start))
    (tset modal :exited #(and (timer:running) (timer:stop)))

    modal))

(local hammer (def-modal-with-timeout 4 [:cmd :alt :ctrl] :space))
(hammer:bind [] :escape nil nil #(hammer:exit))

(ModalMgr:new :spacehammer)
(tset ModalMgr.modal_list :spacehammer hammer)
(bearclaw :z #(ModalMgr:activate [:spacehammer]))
(ModalMgr:deactivateAll)

;; TODO recursive bindings/prefix keys/whatever you want to call it
;; TODO support mod keys
(fn spacehammer [key func message]
  "bind KEY to FUNC in the spacehammer modal keymap, displaying an optional MESSAGE"
  (hammer:bind [] key message func))

(fn in-prefix [modal] (fn [key func] (modal:bind [] key nil func)))

{: puppy-paw
 : bearclaw
 : BEARCLAW
 : spacehammer
 : in-prefix}
