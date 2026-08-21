;;; tangle-support.el -*- lexical-binding: t; -*-
;; Helpers for :tangle headers in config.org.
;;
;; These must work in the *async* save-tangle subprocess, which loads only
;; early-init.el — neither init.el nor config.el — so my machine-local toggles
;; (amb/enable-*, set in local-toggles.el and defaulted in init.el) are not in
;; scope there. This file is therefore hand-maintained (not tangled) and loaded
;; by path directly from the headers that need it; see the copilot blocks in
;; config.org for the calling convention.
;;
;;; Code:

(defun amb/tangle-when (toggle dest)
  "For use in a `:tangle' header: return DEST if TOGGLE is truthy, else \"no\".
TOGGLE is a symbol. Loads local-toggles.el (if present, relative to
`default-directory', which is the doom user dir during tangling) so TOGGLE
resolves even in a bare tangle subprocess. An unbound TOGGLE (e.g. no
local-toggles.el on this machine) reads as off."
  (let ((local (expand-file-name "local-toggles.el" default-directory)))
    (when (file-exists-p local) (load local nil t)))
  (if (and (boundp toggle) (symbol-value toggle)) dest "no"))

(provide 'tangle-support)
;;; tangle-support.el ends here
