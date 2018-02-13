;-------------------------------------------------------------------------- 
; Jonathan Tow
; jonathantow8@gmail.com
;
;;; Code:


;--------------------------------------------------------------------------
; CURSOR
;
; highlight the current line
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(set-face-background 'hl-line "#feffe8")
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
;--------------------------------------------------------------------------


;-------------------------------------------------------------------------- 
; CUSTOM-SET-FACES
;
(custom-set-faces
 '(font-lock-string-face ((t (:foreground "#1e9e1e"))))
 '(scroll-bar ((t (:background "dim gray")))))					
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; INITIAL FRAME LOCATION
;
(setq initial-frame-alist `((left . 150))) 
(setq default-frame-alist (copy-alist initial-frame-alist))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; INHIBIT STARTUP SCREEN & MESSAGE 
;
(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq auto-fill-mode 1)
;-------------------------------------------------------------------------- 


;-------------------------------------------------------------------------- 
; MENU BAR (OFF)
;
(tool-bar-mode -1)
;--------------------------------------------------------------------------


;-------------------------------------------------------------------------- 
; BELL NOISE (OFF)
;
(setf visible-bell t)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; SCREEN HEIGHT & WIDTH
;
(add-to-list 'default-frame-alist  '(height . 59))
(add-to-list 'default-frame-alist  '(width . 130))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
;
; THEME MODE
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))
;-------------------------------------------------------------------------- 


;--------------------------------------------------------------------------
; POWER-LINE
;
(use-package spaceline
  :ensure t)
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme))
;-------------------------------------------------------------------------- 


;-------------------------------------------------------------------------- 
; FONT
;
;(set-frame-font "DejaVu Sans Mono-12" t)
(set-face-attribute 'default nil :height 140) ; nice tall window
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))					
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; FILL-COLUMN-INDICATOR (at Column 80)
;
 (use-package fill-column-indicator
   :ensure t
   :config
   (setq fci-rule-column 80)
   (add-hook 'prog-mode-hook 'fci-mode))
;--------------------------------------------------------------------------


(provide 'style)
;;; style.el ends here
