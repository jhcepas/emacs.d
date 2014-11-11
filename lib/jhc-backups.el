;; Enable backups 
(setq vc-make-backup-files t)
(setq make-backup-files t) 

;; I do not want backups of files in the same directory 
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("./" . "~/.emacs-backups/"))   ; don't litter my fs tree
    delete-old-versions t    ; don't ask me
    kept-new-versions 20
    kept-old-versions 10
    version-control t        ; use versioned backups

    auto-save-default t      ; auto-save every buffer that visits a file
    auto-save-timeout 30     ; number of seconds idle time before auto-save (default: 30)
    auto-save-interval 300   ; number of keystrokes between auto-saves
)

'(auto-save-file-name-transforms '((".*" "~/.emacs-backups/\\1" t)))
; backup tramp files
(setq tramp-auto-save-directory "~/.emacs-backups/")
(setq tramp-backup-directory-alist backup-directory-alist)

;; "turn off" the effect of `backup-directory-alist' for TRAMP files
;(add-to-list 'backup-directory-alist
;             (cons tramp-file-name-regexp nil))


(provide 'jhc-backups)