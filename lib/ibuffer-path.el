
(require 'ibuffer)
(require 'ibuf-ext)
(eval-when-compile
  (require 'cl))

(defun ibuffer-by-path (buf)
  (let ((file-name (with-current-buffer buf (or buffer-file-name ""))))
    (cons (file-name-directory file-name) "")))

;;;###autoload
(defun ibuffer-generate-filter-groups-by-path ()
  (let ((roots (ibuffer-remove-duplicates
                (delq nil (mapcar 'ibuffer-by-path (buffer-list))))))
    (mapcar (lambda (tramp-connection)
              (cons (format "%s%s" (car tramp-connection) (cdr tramp-connection))
                    `((tramp-connection . ,tramp-connection))))
            roots)))

(define-ibuffer-filter tramp-connection
    "Toggle current view to buffers with TRAMP connection QUALIFIER."
  (:description "TRAMP connection"
                :reader (read-from-minibuffer "Filter by TRAMP connection (regexp): "))
  (ibuffer-awhen (ibuffer-by-path buf)
    (equal qualifier it)))

(require 'ibuffer-vc)
;;;###autoload
(defun ibuffer-set-filter-groups-by-path ()
  (interactive)
  (ibuffer)
  (setq ibuffer-filter-groups (ibuffer-generate-filter-groups-by-path))
  (ibuffer-do-sort-by-filename/process)
  (ibuffer-update nil t))

(provide 'ibuffer-path)

