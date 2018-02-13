(deftheme tango
  "Created 2018-02-05.")

(custom-theme-set-variables
 'tango
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"]))

(custom-theme-set-faces
 'tango
 '(cursor ((((class color) (min-colors 89)) (:background "#204a87"))))
 '(fringe ((((class color) (min-colors 89)) (:background "#d3d7cf"))))
 '(highlight ((((class color) (min-colors 89)) (:background "#babdb6"))))
 '(region ((((class color) (min-colors 89)) (:background "#babdb6"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#8cc4ff"))))
 '(isearch ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#ce5c00"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:background "#e9b96e"))))
 '(trailing-whitespace ((((class color) (min-colors 89)) (:background "#ef2929"))))
 '(mode-line ((((class color) (min-colors 89)) (:box (:line-width -1 :style released-button) :background "#d3d7cf" :foreground "#2e3436"))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:box (:line-width -1 :style released-button) :background "#888a85" :foreground "#2e3436"))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:weight bold :foreground "#204a87"))))
 '(escape-glyph ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(error ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "#ce5c00"))))
 '(success ((((class color) (min-colors 89)) (:foreground "#4e9a06"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#75507b"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:slant italic :foreground "#5f615c"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:weight bold :foreground "#204a87"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#346604"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#5c3566"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#204a87"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#b35000"))))
 '(link ((((class color) (min-colors 89)) (:underline t :foreground "#204a87"))))
 '(link-visited ((((class color) (min-colors 89)) (:underline t :foreground "#3465a4"))))
 '(flyspell-duplicate ((((class color) (min-colors 89)) (:underline "#fcaf3e"))))
 '(flyspell-incorrect ((((class color) (min-colors 89)) (:underline "#ef2929"))))
 '(default ((((class color) (min-colors 89)) (:foreground "#2e3436" :background "#eeeeec")))))

(provide-theme 'tango)
