;; syntax-highlight aggressively
(setq font-lock-maximum-decoration t)

;; avoid Emacs hanging for a while changing default font
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-variables
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

;; show column number
(column-number-mode t)


;; If possible set up a custom color scheme, otherwise turn colors off
(autoload 'custom-set-faces "font-lock" "Set the color scheme" t)
(autoload 'font-lock-fontify-buffer "font-lock" "Fontify Buffer" t)
(condition-case err
    (progn (apply 'custom-set-faces lconfig-font-lock-faces)
           )
  (error (progn
           (message "Could not customize colors, disabling colored fonts.")
           (setq-default font-lock-auto-fontify t))))

;; Using cursor color to indicate some modes. If you sometimes find
;; yourself inadvertently overwriting some text because you are in
;; overwrite mode but you didn't expect so, this might prove as useful
;; to you as it is for me. It changes cursor color to indicate
;; read-only, insert and overwrite modes:
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;;set-cursor-color is somewhat costly, so we only call it when
  ;;needed:
  (let ((color
	 (if buffer-read-only "blue"
	   (if overwrite-mode "red"
	     "SpringGreen"))))
    (unless (and
	     (string= color hcz-set-cursor-color-color)
	     (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
(add-hook 'after-make-frame-functions 'set-background)
(defun set-background(frame)
  ;; must be current for local ctheme
  (select-frame frame)
  ;; test winsystem
  (set-background-color "grey10")
  (set-foreground-color "grey85")
)

; Window takes the name of buffer
(setq frame-title-format "%b [%f]");

;;  Avoid anoying lag in syntax highlighting. 
(cond ((< emacs-major-version 22)
       (setq font-lock-support-mode 'lazy-lock-mode)
       (setq lazy-lock-defer-contextually t)
       (setq lazy-lock-defer-time 0)
       ))
(cond ((>= emacs-major-version 22)
       (setq jit-lock-contextually t)
       (setq jit-lock-context-time 0)
       ))

;(require 'color-theme-zenburn)
;(color-theme-zenburn)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/lib/color-theme-solarized/")
;(load-theme 'solarized-dark t)
(provide 'jhc-appearance)
