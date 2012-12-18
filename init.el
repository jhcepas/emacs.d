;; Trick to get the filename of the installation directory
(defconst emacs-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)
(add-to-list 'load-path emacs-dir)

;; Add my personal directory to the path 
;(setq load-path (cons (concat emacs-dir "lib/") load-path))

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
 '(default ((t (:stipple nil :background "grey10" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flyspell-duplicate ((t (:underline t :slant italic))))
 '(flyspell-incorrect ((t (:underline t :weight bold))))

 '(flymake-errline ((((class color)) (:underline "dark red" :slant italic))))
 '(flymake-warnline ((((class color)) (:underline "grey52"))))

 '(font-lock-comment-face ((t (:foreground "indianred"))))
 '(font-lock-keyword-face ((t (:foreground "turquoise3"))))
 '(font-lock-string-face ((t (:foreground "palegreen4"))))
 '(outline-2 ((t (:foreground "steelblue1"))))
 '(rst-level-1-face ((t (:background "darkred"))) t)
 '(rst-level-2-face ((t (:background "grey78"))) t)
 '(rst-level-3-face ((t (:background "darkgreen"))) t)
 '(rst-level-4-face ((t (:background "grey20"))) t))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
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
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "grey10" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flymake-errline ((((class color)) (:underline "dark red" :slant italic))))
 '(flymake-warnline ((((class color)) (:underline "grey52"))))
 '(flyspell-duplicate ((t (:underline t :slant italic))))
 '(flyspell-incorrect ((t (:underline t :weight bold))))
 '(font-lock-comment-face ((t (:foreground "indianred"))))
 '(font-lock-keyword-face ((t (:foreground "turquoise3"))))
 '(font-lock-string-face ((t (:foreground "palegreen4"))))
 '(outline-2 ((t (:foreground "steelblue1"))))
 '(rst-level-1-face ((t (:background "darkred"))) t)
 '(rst-level-2-face ((t (:foreground "indianred"))) t)
 '(rst-level-3-face ((t (:background "grey30" :foreground "lightblue"))) t)
 '(rst-level-4-face ((t (:background "grey20"))) t))

