(local { :config_builder config-builder } (require :wezterm))

(fn wezterm-config-filepath [filename]
  (.. (os.getenv "HOME") "/.config/wezterm/" filename ".fnl"))

(local { : file-exists
         : identity } (dofile (wezterm-config-filepath :utils)))

(local local-overrides
       (let [localfile (wezterm-config-filepath :local)]
         (if (file-exists localfile)
              ;; in fact, require the file and append its decorator function to the ol' list
              (require localfile)
              { :apply identity })))

(local config
       (or
        (and config-builder (config-builder))
        {}))

(-> config
    local-overrides.apply)
