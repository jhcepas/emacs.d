;; Open special buffers in new window
(setq special-display-buffer-names
      ;'("*Completions*" )
      )
;; swap 2 windows
(defun swap-buffers ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )
(global-set-key (kbd "<f12>") 'swap-buffers)

;; operate on buffers like Dired
;; completely replaces `list-buffer'
(defalias 'ibuffer-list-buffers 'list-buffer)

;; Enable ibuffer-filter-by-filename to filter on directory names too.
(eval-after-load "ibuf-ext"
  '(define-ibuffer-filter filename
     "Toggle current view to buffers with file or directory name matching QUALIFIER."
     (:description "filename"
                   :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
     (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                        (buffer-local-value 'dired-directory buf))
                    (string-match qualifier it))))


(setq ibuffer-show-empty-filter-groups nil)
;(require 'ibuffer-path)
(require 'ibuffer-vc)

(defun ibuffer-set-filter-groups ()
  (interactive)
  (ibuffer)
  (ibuffer-vc-set-filter-groups-by-vc-root)
  ;(setq ibuffer-filter-groups (ibuffer-generate-filter-groups-by-path))
  ;(ibuffer-do-sort-by-filename/process)
  (ibuffer-update nil t))

(global-set-key (kbd "C-x C-b") 'ibuffer-set-filter-groups)


;; (setq ibuffer-saved-filter-groups
;;       '(("default"
;;          ("VERSION CONTROL" (or (mode . svn-status-mode)
;;                                 (mode . svn-log-edit-mode)
;;                                     (name . "^\\*svn-")
;;                                     (name . "^\\*vc\\*$")
;;                                     (name . "^\\*Annotate")
;;                                     (name . "^\\*git-")
;;                                     (name . "^\\*vc-")))
;;          ("PYTHON" (or (mode . python-mode)
;;                            ))
;;          ("web" (or (mode . html-mode)
;;                     (mode . espresso-mode)
;;                     (mode . css-mode)
;;                       ))
;;          ("C" (or (mode . c-mode)
;;                    (mode espresso-mode)
;;                    ))
;;          ("TXT" (or (mode . text-mode)
;;                       ))
;;           ("emacs" (or (name . "^\\*scratch\\*$")
;;                        (name . "^\\*Messages\\*$")
;;                        (name . "^TAGS\\(<[0-9]+>\\)?$")
;;                        (name . "^\\*Help\\*$")
;;                        (name . "^\\*info\\*$")
;;                        (name . "^\\*Occur\\*$")
;;                        (name . "^\\*grep\\*$")
;;                        (name . "^\\*Compile-Log\\*$")
;;                        (name . "^\\*Backtrace\\*$")
;;                        (name . "^\\*Process List\\*$")
;;                        (name . "^\\*gud\\*$")
;;                        (name . "^\\*Kill Ring\\*$")
;;                        (name . "^\\*Completions\\*$")
;;                        (name . "^\\*tramp")
;;                        (name . "^\\*shell\\*$")
;;                        (name . "^\\*compilation\\*$")))
;;           ("Lisp" (or (mode . emacs-lisp-mode)
;;                               ))
;;           ("agenda" (or (name . "^\\*Calendar\\*$")
;;                         (name . "^diary$")
;;                         (name . "^\\*Agenda")
;;                         (name . "^\\*org-")
;;                         (name . "^\\*Org")
;;                         (mode . org-mode)
;;                         (mode . muse-mode)))
;;           ("latex" (or (mode . latex-mode)
;;                        (mode . LaTeX-mode)
;;                        (mode . bibtex-mode)
;;                        (mode . reftex-mode)))
;;           ("Files" (or (mode . dired-mode))))))

;; (add-hook 'ibuffer-mode-hook
;;           (lambda ()
;;             (ibuffer-switch-to-saved-filter-groups "default")))

;; ;; Order the groups so the order is : [Default], [agenda], [emacs]
;; (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
;;                                                  activate)
;;   (setq ad-return-value (nreverse ad-return-value)))

;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)
;; ;(global-set-key (kbd "s-s") 'speedbar-get-focus)
;; (setq speedbar-show-unknown-files t)
;; (setq sr-speedbar-right-side t)


(provide 'jhc-buffers)
