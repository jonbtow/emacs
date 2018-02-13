;--------------------------------------------------------------------------
; Jonathan Tow
; jonathantow8@gmail.com
;
;;; Code:


;--------------------------------------------------------------------------
; Set column number mode
(setq column-number-mode t)
; Back ups are placed in a  "~/.saves" directory
(setq backup-directory-alist `(("." . "~/.saves")))
; Matches parentheses and such in every mode
(show-paren-mode 1)
; Calender should start on
(setq calendar-week-start-day 1)
; Turn off auto-save
(setq auto-save-default nil)
; Turn off bell
(setq ring-bell-function 'ignore)
; Turn on desktop save to save sessions
(desktop-save-mode 1)
;; Switch typing yes or no to just y or n.
(fset 'yes-or-no-p 'y-or-n-p)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; IDO-MODE
;
(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; LINE NUMBER
;
(global-linum-mode 1)
(global-visual-line-mode 1)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; MAGIT
;
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; E-SHELL
;
(defun my-eshell-prompt ()
  "Highlight eshell pwd and prompt separately."
  (mapconcat
   (lambda (list)
     (propertize (car list)
                 'read-only      t
                 'font-lock-face (cdr list)
                 'front-sticky   '(font-lock-face read-only)
                 'rear-nonsticky '(font-lock-face read-only)))
   `((,(abbreviate-file-name (eshell/pwd)) :foreground "#FF78C4")
     (,(if (zerop (user-uid)) " # " " $ ") :foreground "green"))
   ""))

(setq eshell-highlight-prompt nil
      eshell-prompt-function  #'my-eshell-prompt)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; UNDO-TREE
;
(use-package undo-tree
  :ensure t
  :init
  (global-set-key (kbd "C-z") 'undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") 'redo)
  :config
  (global-undo-tree-mode))
;--------------------------------------------------------------------------

					;--------------------------------------------------------------------------
; SMOOTH SCROLLING SETUP 
;
(use-package smooth-scroll
  :config
  (setq smooth-scroll/vscroll-step-size 5)
  (smooth-scroll-mode 1))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; SPEEDBAR SETUP 
;
(use-package sr-speedbar
  :ensure t
  :init
  (setq
   speedbar-show-unknown-files t        ; Show all files
   sr-speedbar-right-side nil
   sr-speedbar-width 22
   sr-speedbar-width-console 5
   sr-speedbar-max-width 25)
  :config
  (sr-speedbar-open))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; FLYSPELL MODE
;
(use-package flyspell
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
    )
  :config
  ;; Sets flyspell correction to use two-finger mouse click
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  )
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; AUTOPAIR
;
(use-package autopair
  :ensure t
  :config
  (autopair-global-mode 1))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; AUTO-INDENT
;
(electric-indent-mode 1)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; AUTO-LINUM
;
(global-linum-mode t)
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; SCHEME & RACKET SETUP
;
(setq scheme-program-name   "/usr/local/bin/mit-scheme")
(add-to-list 'load-path "~/.emacs.d/plugins/scheme")
(defun load-xscheme () (require 'xscheme))
(add-hook 'scheme-mode-hook 'load-xscheme)
(setq geiser-repl-skip-version-check-p t)
(setq geiser-racket-binary "/Applications/Racket/bin/racket")
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; WEB-DEV SETUP
;
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
;; automatic beautify before saving
(eval-after-load 'js
  '(add-hook 'js-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))
(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))
(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; ORG MODE 
(use-package org
  :mode ("\\.org'" . org-mode)
  :init (setq org-babel-safe-header-args nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (C . t)
     (latex . t)
     (awk . t)
     (python . t)))

  (setq org-babel-python-command "ipython"
	org-export-with-section-numbers nil
        org-export-with-toc nil
        org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-startup-folded 'showall
        org-use-speed-commands t)

  (add-hook 'org-mode-hook #'(lambda () (auto-fill-mode +1)))

  (setenv "PATH" (concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))

  (use-package org-bullets
    :ensure t
    :commands (org-bullets-mode)
    :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))
;--------------------------------------------------------------------------

  
;--------------------------------------------------------------------------
; AUTO-COMPLETION
;
; start auto-complete with emacs
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; YASNIPPET
;
; start yasnippet with emacs
(use-package yasnippet
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-global-mode 1))
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/usr/llvm-gcc-4.2/lib/gcc/i686-apple-da\
rwin11/4.2.1/include"))

(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; FLYCHECK
;
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; C/C++ programming setup
;
; set LD_LIBRARY_PATH
(setenv "LD_LIBRARY_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/")

;-----------------
; IRONY-MODE
; (IF CLONED INSTALL VIA MELPA)
; irony mode auto-completion backend
(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
 )
;-----------------

;-----------------
; COMPANY-MODE
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))
;-----------------

;-----------------
; RTAGS
(use-package rtags			
  :ensure t
  :config
  (progn
    ;; Start rtags upon entering a C/C++ file
    (add-hook
     'c-mode-common-hook
     (lambda () (if (not (is-current-file-tramp))
                    (rtags-start-process-unless-running))))
    (add-hook
     'c++-mode-common-hook
     (lambda () (if (not (is-current-file-tramp))
                    (rtags-start-process-unless-running))))
    ;; Flycheck setup
    (require 'flycheck-rtags)
    (defun my-flycheck-rtags-setup ()
      (flycheck-select-checker 'rtags)
      ;; RTags creates more accurate overlays.
      (setq-local flycheck-highlighting-mode nil)
      (setq-local flycheck-check-syntax-automatically nil))
    ;; c-mode-common-hook is also called by c++-mode
    (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
    ;; Keybindings
    (rtags-enable-standard-keybindings c-mode-base-map "\C-cr")
    )
  )
;-----------------

;-----------------
; LINUX-C-STYLE
(defun linux-c-mode ()
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'c-mode-hook 'linux-c-mode)
(add-hook 'c++-mode-hook 'linux-c-mode)
(setq c-default-style "linux")
;-----------------

;-----------------
; C/C++ START UP TEMPLATE 
;
(auto-insert-mode)
(setq auto-insert-query nil)
;----------------
; C Template

;----------------- 
; C++ Template
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
     '("Short description: "
       "/*\n" 
       "* file: " (file-name-nondirectory (buffer-file-name))
       "\n* brief: " str \n
       "* author: Jonathan Tow"\n
       "*/" > \n \n
       "#include <iostream>" \n 
       "#include <vector>" \n
       "#include <string>" \n
       "#include <algorithm>" \n \n
       "using namespace std;" \n \n
       "int main(int argc, char **argv)\n"
       "{\n"
       > _ \n 
       "return 0;\n"
       "}" > \n)))
;-----------------

;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; LATEX (AUCTEX)
;
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq TeX-auto-save t
	  TeX-parse-self t
	  TeX-save-query nil
	  TeX-PDF-mode t)
    ))
; Use company-auctex
(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; SQL
;
(use-package sql
  :ensure t
  :mode ("\\.sql" . sql-mode))
;--------------------------------------------------------------------------


(provide 'modes)
;;; modes.el ends here
