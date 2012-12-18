; basics
(setq python-indet-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default python-indent 4)

(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

; auto-complete
(setq load-path (cons "~/.emacs.d/lib/auto-complete" load-path))
(setq load-path (cons "~/.emacs.d/lib/emacs-ctable" load-path))
(setq load-path (cons "~/.emacs.d/lib/emacs-deferred" load-path))
(setq load-path (cons "~/.emacs.d/lib/emacs-epc" load-path))
(setq load-path (cons "~/.emacs.d/lib/emacs-jedi" load-path))

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

(provide 'jhc-python)
