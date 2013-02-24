;; Trick to get the filename of the installation directory
(defconst emacs-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)
(add-to-list 'load-path emacs-dir)

;; Add my personal directory to the path 
(setq load-path (cons (concat emacs-dir "lib/") load-path))

(setenv "PYTHONPATH" (concat emacs-dir "pythonlib/"))

(require 'jhc-edit)
(require 'jhc-backups)
(require 'jhc-navigation)
(require 'jhc-buffers)
(require 'jhc-orgmode)
(require 'jhc-appearance)
(require 'jhc-completion)
(require 'jhc-python)
(require 'jhc-spellcheck)

;(require 'template)
;(template-initialize)

;(require 'pdb)

(setq auto-mode-alist
      (append '(
                ("\\.css\\'"                           . css-mode)
                ("\\.\\(htm\\|html\\|xhtml\\)$"        . html-mode)
                ("\\.sql$"                             . sql-mode)
                ("\\.js$"                              . js-mode)
                ("\\.json$"                            . js-mode)
                ("\\.js$"                              . js-mode)
                ("\\.py"                               . python-mode)
                ;; sorted by chapter
                ("\\.\\(diffs?\\|patch\\|rej\\)\\'"    . diff-mode)
                ("\\.txt$"                             . org-mode)
                ("\\.dat$"                             . ledger-mode)

                ("\\.log$"                             . text-mode)
                ("\\.tex$"                             . LaTeX-mode)
                ("\\.tpl$"                             . LaTeX-mode)
                ("\\.cgi$"                             . perl-mode)
                ("[mM]akefile"                         . makefile-mode)
                ("\\.bash$"                            . shell-script-mode)
                ("\\.expect$"                          . tcl-mode)

                (".ssh/config\\'"                      . ssh-config-mode)
                ("sshd?_config\\'"                     . ssh-config-mode)
                ) auto-mode-alist))



; My references database
(setq reftex-default-bibliography   (quote       ("~/refs.bib")))       

;; ;;;;;;;;;;;; TRAMP
(setq tramp-default-method "scp")
(setq tramp-default-user "jhuerta")
(setq tramp-auto-save-directory "/tmp/")
;; "turn off" the effect of `backup-directory-alist' for TRAMP files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))



;; CUSTOM COLORS & STYLE


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "grey10" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(ac-candidate-face ((t (:background "lightgrey" :foreground "black"))))
 '(ac-completion-face ((t (:foreground "dimgray" :underline t))))
 '(ac-gtags-selection-face ((t (:background "steelblue" :foreground "white"))))
 '(flymake-errline ((((class color)) (:underline "dark red" :slant italic))) t)
 '(flymake-warnline ((((class color)) (:underline "grey52"))) t)
 '(flyspell-duplicate ((t (:underline t :slant italic))))
 '(flyspell-incorrect ((t (:underline t :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "cornflower blue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "indianred"))))
 '(font-lock-comment-face ((t (:foreground "#ad5c5c" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "turquoise4"))))
 '(font-lock-doc-face ((t (:foreground "lemon chiffon"))))
 '(font-lock-function-name-face ((t (:foreground "medium sea green" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#5591af"))))
 '(font-lock-string-face ((t (:foreground "palegreen4"))))
 '(font-lock-type-face ((t (:foreground "snow2" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#ccc18f"))))
 '(isearch ((t (:back "grey"))))
 '(lazy-highlight ((t (:background "goldenrod" :foreground "black"))))
 '(link ((t (:foreground "cyan" :underline t))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(outline-2 ((t (:foreground "steelblue1"))) t)
 '(popup-tip-face ((t (:background "darkslategray" :foreground "white2"))))
 '(region ((t (:background "grey20"))))
 '(rst-level-1-face ((t (:background "darkred"))) t)
 '(rst-level-2-face ((t nil)) t)
 '(rst-level-3-face ((t (:background "darkgreen"))) t)
 '(rst-level-4-face ((t (:background "grey20"))) t)
 '(show-paren-match ((t (:background "grey20" :foreground "yellow")))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(global-font-lock-mode t nil (font-lock))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(inverse-video nil)
 '(line-number-display-limit nil)
 '(menu-bar-mode nil)
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(truncate-partial-width-windows t)
 '(use-file-dialog nil)
 '(visible-bell t)
 '(x-select-enable-clipboard t)
 '(x-stretch-cursor nil))
