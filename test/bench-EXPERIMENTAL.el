
;;; THIS IS A VERY GOOD IMPROVEMENT FOR LISTING FILES
;; projectile-current-project-files in the linux kernel tree in the benchmarks
;; goes from 130seconds downt to 30.
(defun projectile-dir-files-external (root directory)
  "Get the files for ROOT under DIRECTORY using external tools."
  (let ((default-directory directory)
        (files-list nil))
    (setq files-list (-map (lambda (f)
                             ;; (file-relative-name (expand-file-name f directory) root)
                             (s-chop-prefix root (expand-file-name f directory))
                             )
                           (projectile-get-repo-files)))
    files-list))


;; EVEN MORE EXPERIMENATAL STUFF
(defun projectile-file-remote-p (filename)
  "This is an short cut for file-remote-p which has different features.
 Replacing all occurrences in projectile.el will fail."
  (or
   (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p filename))
   (file-remote-p filename)))

(defun projectile-file-relative-name (filename &optional directory)
  "For experimentation..
Problem is that file-relative-name calls file-remote-p and also
does it too many times. It might be a good idea to actaully only
use fully expanded file paths internally in projectile and just
shorten them for display purposes only.
More tests first..."
  (if (projectile-file-remote-p filename)
      filename
    (file-relative-name filename directory)))

(defun replace-string-projectile-bench-experimental ()
  "....."
  (interactive)
  (when (equal (file-name-nondirectory (buffer-file-name)) "projectile.el")
      (save-restriction
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "file-remote-p" nil t)
        (replace-match "projectile-file-remote-p" t nil))))

(save-restriction
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "file-relative-name" nil t)
        (replace-match "projectile-file-relative-name" t nil))))
    )
  )
