;;; projectile.el --- Manage and navigate projects in Emacs easily

;; Copyright (C) 2011-2012 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/projectile
;; Version: 0.7
;; Created: 2011-31-07
;; Keywords: project, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:

;; requires
(require 'cl)
(require 'easymenu)
(require 'thingatpt)

(defgroup projectile nil "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience)

(defcustom projectile-enable-caching nil
  "Enable project files caching."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-keymap-prefix (kbd "C-c p")
  "Projectile keymap prefix."
  :group 'projectile
  :type 'sexp)


;; variables
(defvar projectile-project-root-files '(".git" ".hg" ".bzr" "_darcs" ".projectile")
  "A list of files considered to mark the root of a project.")

(defvar projectile-ignored-files '("TAGS")
  "A list of files ignored by projectile.")

(defvar projectile-ignored-directories '(".idea")
  "A list of directories ignored by projectile.")

(defvar projectile-ignored-file-extensions '("class" "o" "so" "elc")
  "A list of file extensions ignored by projectile.")

(defvar projectile-projects-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project file names to speed up related operations.")

(defun projectile-invalidate-cache ()
  "Remove the current project's files from `projectile-projects-cache'."
  (interactive)
  (let ((project-root (projectile-get-project-root)))
    (remhash project-root projectile-projects-cache)
    (message "Invalidated Projectile cache for %s" project-root)))

(defun projectile-get-project-root ()
  "Retrieves the root directory of a project if available."
  (loop for file in projectile-project-root-files
        when (locate-dominating-file default-directory file)
        do (return it)))

(defun projectile-get-project-name ()
  "Return project name."
  (file-name-nondirectory (directory-file-name (projectile-get-project-root))))

(defun projectile-get-project-files (directory)
  "List the files in DIRECTORY and in its sub-directories."
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                     (gethash directory projectile-projects-cache))))
    ;; cache miss
    (unless files-list
      ;; while we are in the current directory
      (dolist (current-file (file-name-all-completions "" directory) files-list)
        (cond
         ;; check for directories that are not ignored
         ((and (projectile-string-suffix-p current-file "/")
               (not (or (string= current-file "./") (string= current-file "../")))
               (not (projectile-ignored-p (concat directory current-file)))
               (not (projectile-ignored-directory-p current-file)))
          (setq files-list (append files-list (projectile-get-project-files (concat directory current-file)))))
         ;; check for regular files that are not ignored
         ((and (not (or (string= current-file "./") (string= current-file "../")))
               (not (projectile-string-suffix-p current-file "/"))
               (not (projectile-ignored-extension-p current-file))
               (not (projectile-ignored-file-p current-file)))
          (setq files-list (cons (expand-file-name (concat directory current-file)) files-list)))))
      ;; cache the resulting list of files
      (when (and projectile-enable-caching (string= directory (projectile-get-project-root)))
        (puthash directory files-list projectile-projects-cache)))
    files-list))

(defun projectile-string-suffix-p (string suffix)
  "Check whether STRING ends with SUFFIX."
  (string= (substring string (- (length string) (length suffix))) suffix))

(defun projectile-get-project-buffers ()
  "Get a list of project buffers."
  (let ((project-files (projectile-get-project-files (projectile-get-project-root)))
        (buffer-files (mapcar 'buffer-file-name (buffer-list))))
    (mapcar 'get-file-buffer (intersection project-files buffer-files :test 'string=))))

(defun projectile-get-project-buffer-names ()
  "Get a list of project buffer names."
  (mapcar 'buffer-name (projectile-get-project-buffers)))

(defun projectile-prepend-project-name (string)
  (format "[%s] %s" (projectile-get-project-name) string))

(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (switch-to-buffer
   (ido-completing-read
    (projectile-prepend-project-name "Switch to buffer: ")
    (projectile-get-project-buffer-names))))

(defun projectile-multi-occur ()
  "Do a `multi-occur' in the project's buffers."
  (interactive)
  (multi-occur (projectile-get-project-buffers)
               (car (occur-read-primary-args))))

(defun projectile-hashify-files (files-list)
  "Make the list of project files FILES-LIST ido friendly."
  (let ((files-table (make-hash-table :test 'equal))
        (files-to-uniquify nil))
    (dolist (current-file files-list files-table)
      (let ((basename (file-name-nondirectory current-file)))
        (if (gethash basename files-table)
            (progn
              (puthash (uniquify-file current-file) current-file files-table)
              (when basename (push basename files-to-uniquify)))
          (puthash basename current-file files-table))))
    ;; uniquify remaining files
    (dolist (current-file (remove-duplicates files-to-uniquify :test 'string=))
      (puthash (uniquify-file (gethash current-file files-table)) (gethash current-file files-table) files-table)
      (remhash current-file files-table))
    files-table))

(defun uniquify-file (filename)
  "Create an unique version of a FILENAME."
  (let ((filename-parts (reverse (split-string filename "/"))))
    (format "%s/%s" (second filename-parts) (first filename-parts))))

(defun projectile-ignored-p (file)
  "Check if FILE should be ignored."
  (loop for ignored in projectile-project-root-files
        when (string= (expand-file-name (concat (projectile-get-project-root) ignored "/")) (expand-file-name file))
        do (return t)
        finally (return nil)))

(defun projectile-ignored-directory-p (file)
  "Check if FILE should be ignored."
  (loop for ignored in projectile-ignored-directories
        when (string= file (concat ignored "/"))
        do (return t)
        finally (return nil)))

(defun projectile-ignored-file-p (file)
  "Check if FILE should be ignored."
  (member file projectile-ignored-files))

(defun projectile-ignored-extension-p (file)
  "Check if FILE should be ignored based on its extension."
  (let ((ext (file-name-extension file)))
    (member ext projectile-ignored-file-extensions)))

(defun projectile-find-file ()
  "Jump to a project's file using ido."
  (interactive)
  (let* ((project-files (projectile-hashify-files
                         (projectile-get-project-files (projectile-get-project-root))))
         (file (ido-completing-read (projectile-prepend-project-name "File file: ")
                                    (loop for k being the hash-keys in project-files collect k))))
    (find-file (gethash file project-files))))

(defun projectile-grep ()
  "Perform rgrep in the project."
  (interactive)
  (let ((search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string (projectile-prepend-project-name "Grep for: ") (thing-at-point 'symbol))))
        (root-dir (projectile-get-project-root)))
    (require 'grep)
    (let ((grep-find-ignored-directories (append projectile-ignored-directories grep-find-ignored-directories))
          (grep-find-ignored-files (append projectile-ignored-files grep-find-ignored-files)))
      (grep-compute-defaults)
      (rgrep search-regexp "* .*" root-dir))))

(defun projectile-regenerate-tags ()
  "Regenerate the project's etags using ctags."
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-get-project-root)))
    (cd project-root)
    (shell-command (format "ctags -Re %s" project-root))
    (cd current-dir)
    (visit-tags-table project-root)))

(defun projectile-replace ()
  "Replace a string in the project using perl."
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-get-project-root))
        (old-text (read-string "Replace: " (thing-at-point 'symbol)))
        (new-text (read-string "With: ")))
    (shell-command (format "find %s -type d -name .git -prune -o -print| xargs perl -p -i -e 's/%s/%s/g'" project-root old-text new-text))))

(defun projectile-kill-buffers ()
  "Kill all project buffers."
  (interactive)
  (let* ((buffers (projectile-get-project-buffer-names))
         (question
          (format
           "Are you sure you want to kill %d buffer(s) for '%s'? "
           (length buffers)
           (projectile-get-project-name))))
    (if (yes-or-no-p question)
        (mapc 'kill-buffer buffers))))

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
      (let ((prefix-map (make-sparse-keymap)))
        (define-key prefix-map (kbd "f") 'projectile-find-file)
        (define-key prefix-map (kbd "g") 'projectile-grep)
        (define-key prefix-map (kbd "b") 'projectile-switch-to-buffer)
        (define-key prefix-map (kbd "o") 'projectile-multi-occur)
        (define-key prefix-map (kbd "r") 'projectile-replace)
        (define-key prefix-map (kbd "i") 'projectile-invalidate-cache)
        (define-key prefix-map (kbd "t") 'projectile-regenerate-tags)
        (define-key prefix-map (kbd "k") 'projectile-kill-buffers)

        (define-key map projectile-keymap-prefix prefix-map)

        ;; shortcuts without prefices
        )
      map)
  "Keymap for Projectile mode.")

(easy-menu-define projectile-mode-menu projectile-mode-map
  "Menu for Projectile mode"
  '("Projectile"
    ("Navigating"
     ["File file" projectile-find-file]
     ["Switch to buffer" projectile-switch-to-buffer])

    ("Find & Replace"
     ["Find in project" projectile-grep]
     ["Replace in project" projectile-replace]
     ["Multi-occur in project" projectile-multi-occur])

    ("General"
     ["Invalidate cache" projectile-invalidate-cache]
     ["Regenerate etags" projectile-regenerate-tags])))

;; define minor mode
;;;###autoload
(define-globalized-minor-mode projectile-global-mode projectile-mode projectile-on)

(defun projectile-on ()
  "Enable Projectile."
  (when (projectile-get-project-root)
    (projectile-mode 1)))

(defun projectile-off ()
  "Disable Projectile."
  (easy-menu-remove))

;;;###autoload
(define-minor-mode projectile-mode "Minor mode to assist project management and navigation."
  :lighter " Projectile"
  :keymap projectile-mode-map
  :group 'projectile
  (if projectile-mode
      ;; on start
      (easy-menu-add projectile-mode-menu projectile-mode-map)
    ;; on stop
    (projectile-off)))

(provide 'projectile)
;;; projectile.el ends here
