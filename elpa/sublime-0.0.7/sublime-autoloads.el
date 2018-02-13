;;; sublime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "sublime" "sublime.el" (23159 59812 0 0))
;;; Generated autoloads from sublime.el

(autoload 'sublime-escape-quit "sublime" "\
Forcefully closes anything which keeps the minibuffer busy.

\(fn)" t nil)

(autoload 'sublime-kill-current-buffer "sublime" "\
Kills the current buffer.

\(fn)" t nil)

(autoload 'sublime-open-file "sublime" "\
Forces menu-find-file-existing to show a GUI dialog box

\(fn)" t nil)

(autoload 'sublime-open-recent-file "sublime" "\
Integrates `ido-completing-read' with `recentf-mode'

\(fn)" t nil)

(autoload 'sublime-indent-buffer "sublime" "\
Re-indents the current buffer.

\(fn)" t nil)

(autoload 'sublime-setup-electric "sublime" "\
Enables automatic matching of parentheses.

\(fn)" nil nil)

(autoload 'sublime-setup-clipboard "sublime" "\
Improve interaction with X11 clipboard giving Emacs the 'feel'
of a modern X11 application.

\(fn)" t nil)

(autoload 'sublime-setup-elpa-repositories "sublime" "\
Configure ELPA to use the GNU and Marmalade repositories.

\(fn)" nil nil)

(autoload 'sublime-setup-file-hooks "sublime" "\


\(fn)" nil nil)

(autoload 'sublime-setup-indentation "sublime" "\
Homogeneous indentation level for various modes.

\(fn)" t nil)

(autoload 'sublime-setup-mode-assoc "sublime" "\
Configures file-extension -> mode association.

\(fn)" t nil)

(autoload 'sublime-setup-recentf "sublime" "\
Configures `recentf' for use in combination with `ido-mode'

\(fn)" nil nil)

(autoload 'sublime-setup-snippets "sublime" "\
Enables emacs-wise snippets support using YASnippet

\(fn)" t nil)

(autoload 'sublime-setup-go-to-anything "sublime" "\
Emulates SublimeText `Go-To Anything' feature using IDO and SMEX.
It binds C-S-p to `SMEX' and C-p to `FIND-FILE-IN-PROJECT'.

\(fn)" t nil)

(autoload 'sublime-setup-keybindings "sublime" "\
Additional keybindings Setup additional CUA keybindings.

\(fn)" t nil)

(autoload 'sublime-setup-font "sublime" "\
Chooses a font native to the platform (if available).

\(fn)" t nil)

(autoload 'sublime-setup-ui "sublime" "\
Various user interface customizations.

\(fn)" t nil)

(autoload 'sublime-activate "sublime" "\
Enables all customization options provided by `sublime.el'.

\(fn)" t nil)

(sublime-activate)

;;;***

;;;### (autoloads nil nil ("sublime-pkg.el") (23159 59812 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sublime-autoloads.el ends here
