(require 'projectile)
(require 'helm-config)

(defun helm-c-projectile-list ()
  "Generates a list of files in the current project"
  (projectile-get-project-files
   (projectile-get-project-root)))

(defvar helm-c-projectile-cache nil)

(defvar helm-c-source-projectile
  `((name . "Projectile")
    (init . (lambda ()
              (setq helm-c-projectile-cache
                    (helm-c-projectile-list))))
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (candidates . helm-c-projectile-cache)
    (volatile)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (match helm-c-match-on-basename)
    (type . file))
  "Helm source definition")

(defun helm-projectile ()
  "Example function for calling Helm with the projectile file source.

Use this function as example and create your own list of Helm sources.
"
  (interactive)
  (helm-other-buffer 'helm-c-source-projectile "*helm projectile*"))
