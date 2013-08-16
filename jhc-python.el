
(setq load-path (cons "~/.emacs.d/lib/python-mode" load-path))
(require 'python-mode)
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "M-<right>")
	      'py-shift-region-right)
	    (define-key python-mode-map (kbd "M-<left>")
	      'py-shift-region-left))
	  )

(require 'ipython)

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

;; do not interfere with flyspell 
(defcustom jedi:key-goto-definition (kbd "C-,")
  "Keybind for command `jedi:goto-definition'."
  :group 'jedi)


(defvar python-last-buffer nil
  "Name of the Python buffer that last invoked `toggle-between-python-buffers'")
 
(make-variable-buffer-local 'python-last-buffer)
 
(defun toggle-between-python-buffers ()
  "Toggles between a `python-mode' buffer and its inferior Python process
 
When invoked from a `python-mode' buffer it will switch the
active buffer to its associated Python process. If the command is
invoked from a Python process, it will switch back to the `python-mode' buffer."
  (interactive)
  ;; check if `major-mode' is `python-mode' and if it is, we check if
  ;; the process referenced in `python-buffer' is running
  (if (and (eq major-mode 'python-mode)
           (processp (get-buffer-process python-buffer)))
      (progn
        ;; store a reference to the current *other* buffer; relying
        ;; on `other-buffer' alone wouldn't be wise as it would never work
        ;; if a user were to switch away from the inferior Python
        ;; process to a buffer that isn't our current one. 
        (switch-to-buffer python-buffer)
        (setq python-last-buffer (other-buffer)))
    ;; switch back to the last `python-mode' buffer, but only if it
    ;; still exists.
    (when (eq major-mode 'inferior-python-mode)
      (if (buffer-live-p python-last-buffer)
           (switch-to-buffer python-last-buffer)
        ;; buffer's dead; clear the variable.
        (setq python-last-buffer nil)))))
 
;(define-key inferior-python-mode-map (kbd "<f12>") 'toggle-between-python-buffers)
;(define-key python-mode-map (kbd "<f12>") 'toggle-between-python-buffers)

(provide 'jhc-python)
