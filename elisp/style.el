e;-------------------------------------------------------------------------- 
; Jonathan Tow
; jonathantow8@gmail.com
;


;--------------------------------------------------------------------------
; CURSOR
;
; highlight the current line
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
;--------------------------------------------------------------------------


;-------------------------------------------------------------------------- 
; CUSTOM-SET-FACES
;
(custom-set-faces
 '(font-lock-variable-name-face ((t (:foreground "#ffffff" ))))
 '(font-lock-comment-face ((t (:foreground "wheat4"))))
 '(font-lock-constant-face ((t (:foreground "deep pink"))))
 '(font-lock-keyword-face ((t (:foreground "maroon1"))))
 '(font-lock-preprocessor-face ((t (:inherit maroon1 :foreground "maroon1"))))
 '(font-lock-string-face ((t (:foreground "khaki1"))))
 '(font-lock-type-face ((t (:slant italic))))
 '(scroll-bar ((t (:background "dim gray")))))					
;--------------------------------------------------------------------------


					;--------------------------------------------------------------------------
; INITIAL FRAME LOCATION
;
(setq initial-frame-alist `((left . 400))) 
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
; SCREEN HEIGHT
;
(add-to-list 'default-frame-alist
	     '(height . 59))     ;nice size default frame height.
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------				    
; THEME MODE
;
(add-to-list 'load-path "~/.emacs.d/plugins/colortheme")
(require 'color-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/sublime-themes/themes")
(load-theme 'wheatgrass t)
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


(provide 'style)
;;; style.el ends here
