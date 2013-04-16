(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defmacro bind (key fn)
  "shortcut for my-keys binding"
  `(define-key my-keys-minor-mode-map
     (kbd ,key)
     ;; handle unquoted function names and lambdas
     ,(if (listp fn)
          fn 
        `',fn)))


;; Perl based replace function
(defun perl-replace (start end)
  "Replace a text pattern in a  region using perl expressions"
  (interactive "*r")
  (setq regexp (read-string "Regexp: "))
  (setq to-string (read-string (concat "[" regexp "] Replacement: ")))
  (setq command (concat "perl -e 's/" regexp "/" to-string "/g' -p"))
  (print command)
  (shell-command-on-region start end command nil 1)
  )
(global-set-key "\M-r" 'perl-replace)


;; ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; add spaces around operators
;(require 'smart-operator)

;; 
;(require 'open-next-line)

;; code borrowed from http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the
original" (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)

;; duplicate a line and comment the first
(global-set-key (kbd "C-c c")(lambda()(interactive)(djcb-duplicate-line t)))

;; Mark whole line
(defun mark-line (&optional arg)
  "Marks a line"
  (interactive "p")
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

(global-set-key (kbd "C-c l") 'mark-line)

; code copied from http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

; patches by balle
; http://www.datenterrorist.de
(defun balle-python-shift-left ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-shift-left start end))
  (setq deactivate-mark nil)
)

(defun balle-python-shift-right ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-shift-right start end))
  (setq deactivate-mark nil)
)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "M-<right>")
	      'balle-python-shift-right)
	    (define-key python-mode-map (kbd "M-<left>")
	      'balle-python-shift-left))
	  )


; delete seleted text when typing
(delete-selection-mode 1)

;; highlight current line
;(global-hl-line-mode 1)
;(set-face-background 'hl-line "grey12") ;; Nice color

; highlight brackets
(show-paren-mode t)

;; Line numbering
;;(setq linum-format "%4d")
;;(global-linum-mode 1)

; enable cua mode, but do not touch my key-bindings
;(cua-selection-mode nil)
; enable full cua mode
(when window-system
  (cua-mode))

;;; start search at top of buffer
(bind "C-S-s" (lambda () (interactive) (beginning-of-buffer) (isearch-forward)))

(defun jsj-ac-show-help ()
  "show docs for symbol at point or at beginning of list if not on a symbol"
  (interactive)
  (let ((s (save-excursion
             (or (symbol-at-point)
                 (progn (backward-up-list)
                        (forward-char)
                        (symbol-at-point))))))
    (pos-tip-show (or (if (equal major-mode 'emacs-lisp-mode)
                          (ac-symbol-documentation s)
                        (ac-slime-documentation (symbol-name s))) "no docs")
                  'popup-tip-face
                  ;; 'alt-tooltip
                  (point)
                  nil
                  -1)))

(define-key lisp-mode-shared-map (kbd "C-c C-p") 'jsj-ac-show-help)

;;; a better auto-fill mode for programming
(require 'fillcode)
(add-hook 'c-mode-common-hook 'fillcode-mode)
(add-hook 'perl-mode-hook 'fillcode-mode)
(add-hook 'python-mode-hook 'fillcode-mode)
(add-hook 'shell-script-mode-hook 'fillcode-mode)
(add-hook 'sql-mode-hook 'fillcode-mode)
(setq-default fill-column 80)

(provide 'jhc-edit)

