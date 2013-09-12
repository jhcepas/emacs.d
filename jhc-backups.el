;; Enable backups 
(setq vc-make-backup-files t)
(setq make-backup-files t) 

;; I do not want backups of files in the same directory 
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs-backups"))   ; don't litter my fs tree
    delete-old-versions t    ; don't ask me
    kept-new-versions 100
    kept-old-versions 20
    version-control t        ; use versioned backups
)

; backup tramp files
(setq tramp-auto-save-directory "~/.emacs-backups")
(setq tramp-backup-directory-alist backup-directory-alist)

;; "turn off" the effect of `backup-directory-alist' for TRAMP files
;(add-to-list 'backup-directory-alist
;             (cons tramp-file-name-regexp nil))


(provide 'jhc-backups)