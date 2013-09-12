;; Live completion with auto-complete
;; (see http://cx4a.org/software/auto-complete/)
(add-to-list 'load-path "~/.emacs.d/lib/auto-complete")    
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(require 'auto-complete-config)
(ac-config-default)


(setq ac-dwim nil)
(ac-config-default)

;; set also the completion for eshell
(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
;(define-key ac-complete-mode-map "\t" 'ac-expand)
;(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

(setq ac-auto-start t)
;;(define-key ac-mode-map "\S-TAB" 'auto-complete)

;; custom keybindings to use tab, enter and up and down arrows
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\t" 'ac-expand)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Do not complete with RET/Enter
(define-key ac-completing-map "\r" nil)

;; I prefer to have instant suggestions
(setq ac-auto-show-menu t)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1)
(setq ac-delay 0.3)

;; Smaller menu
(setq ac-menu-height 5)
(setq ac-sources '(ac-source-filename 
                   ac-source-words-in-buffer                   
                   ac-source-features
                   ac-source-functions 
                   ac-source-variables
                   ac-source-symbols 
                   ac-source-abbrev 
                   ac-source-dictionary
                   ac-source-words-in-same-mode-buffers
                   ))
;;A way of delaying processes of flyspell-mode disables auto
;;completion. You can avoid this problem with the following option
(ac-flyspell-workaround)

(provide 'jhc-completion)

