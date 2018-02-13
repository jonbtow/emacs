;-------------------------------------------------------------------------- 
; Jonathan Tow
; jonathantow8@gmail.com
;

;Allows setf 
(require 'cl) 


;-------------------------------------------------------------------------- 
; REQUIRE MELPA et al
;
(package-initialize) 
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; USE-PACKAGE
;
(package-install 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)
;-------------------------------------------------------------------------- 


;-------------------------------------------------------------------------- 
; ELISP paths
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "style")
(load-library "keys")
(load-library "modes")
;--------------------------------------------------------------------------

;-------------------------------------------------------------------------- 
; PERSONAL INFORMATION
(setf user-full-name "Jonathan Tow")
(setf user-mail-address "jonathantow8@gmail.com")
;--------------------------------------------------------------------------


;; ensure environment variables inside Emacs look the same as in my shell
;(when (memq window-system '(mac ns))
;  (exec-path-from-shell-initialize))


(provide 'init)
;;; init.el ends here
