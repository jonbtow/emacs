;-------------------------------------------------------------------------- 
; Jonathan Tow
; jonathantow8@gmail.com
;
;;; Code:

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
(setq-default ispell-program-name "aspell")
(setq ispell-program-name "/usr/local/bin/aspell")
(setq-default ispell-list-command "list")
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
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
; ORG MODE SPICES
;
;; (eval-after-load "org"
;;   '(progn
;;      ;; Change .pdf association directly within the alist
;;      (setcdr (assoc "\\.pdf\\'" org-file-apps) "skim %s")))
;; (add-to-list 'load-path "~/.emacs.d/plugins/org/")
;; (require 'ob-scheme)
;; ; use bullets instead of asteriks.
;; (require 'org-bullets)
;; ; set preview latex to have a bigger font
;; (set-default 'preview-scale-function 1.2)
;; ; change org stars to bullets
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (require 'ox-latex)
;; ; indent mode on
;; (org-indent-mode 1)

;; (unless (boundp 'org-latex-classes)
;;   (setq org-latex-classes nil))
;; (setq org-export-with-section-numbers nil)
;; (setq org-latex-to-pdf-process (list "latexmk %f"))
;; ; make formulas bigger in latex previews
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
;; (defun do-org-show-all-inline-images ()
;;   (interactive)
;;   (org-display-inline-images t t))
;; (global-set-key (kbd "C-c C-x C v")
;;                 'do-org-show-all-inline-images)
;; (let* ((variable-tuple
;; 	(cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;; 	      ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;; 	      ((x-list-fonts "Verdana")         '(:font "Verdana"))
;; 	      ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;; 	      (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
;  babel set up for languages
;(org-babel-do-load-languages
; 'org-babel-load-languages
; '((R . t)
;   (ditaa . t)
;   (dot . t)
;   (emacs-lisp . t)
;   (gnuplot . t)
;   (haskell . t)
;   (latex . t) 
;   (ocaml . t)
;   (perl . t)
;   (python . t)
;   (ruby . t)
;   (screen . t)
;   (sh . t)
;   (sql . t)
;   (sqlite . t)))
;(require 'ox-md)
;(setq org-src-fontify-natively t)
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; PYTHON IDE SETUP               
;
(use-package elpy
  :ensure t
  :init
;  (setq ipython-command "/Users/drseuss/anaconda/bin/ipython3")
;  (setq elpy-rpc-python-command "/Users/drseuss/anaconda/bin/python3")
;  (elpy-use-ipython "~/anaconda/bin/ipython3")
;  (elpy-use-cpython "~/anaconda/bin/ipython3")
  :config
  (package-initialize)
  (elpy-enable)
  )
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
; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
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
  :defer t
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
; LINUX-C-STYLE
(defun linux-c-mode ()
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode nil)
  (setq tab-width 8))
(add-hook 'c-mode-hook 'linux-c-mode)
(add-hook 'c++-mode-hook 'linux-c-mode)
(setq c-default-style "linux")
;-----------------

;-----------------
; C START UP TEMPLATE 
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.c\\'" . "C skeleton")
     '(
       "Short description: "
       "/**" \n
       " * "\n
       " * Filename      : "      
       (file-name-nondirectory (buffer-file-name)) \n
       " * Description   : "  \n
       " * Author        : Jonathan Tow" \n
       " * Created       : " (format-time-string "%A, %e %B %Y.") \n
       " * Version       : " \n
       " * Compatibility : " \n
       " * Features that might be required by this lirbary: " \n
       " * " \n       
       " *               : None"\n
       " * " \n
       " * " \n
       " * Commentary    : "\n
       " * " \n
       " * " \n
       " * " \n
       " */" > \n \n
       "#include <stdio.h>" \n
       "#include <stdlib.h>" \n
       "#include <string.h>" \n
       "#include <math.h>" \n \n       
       "#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".h\"" \n \n
       "int main(int argc, char *argv[])" \n
       "{" > \n
        \n
       "return 0;" \n
       "}" str  \n)))
;-----------------

;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
; set column number mode
(setq column-number-mode t)
;no more creating ".~" files (backup files)
(setq make-backup-files nil)    
; Matches parentheses and such in every mode
(show-paren-mode 1)
; Calender should start on
(setq calendar-week-start-day 1)
; Turn off auto-save
(setq auto-save-default nil)
;--------------------------------------------------------------------------


(provide 'modes)
;;; modes.el ends here
