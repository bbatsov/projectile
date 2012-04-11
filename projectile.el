;;; projectile.el --- Manage and navigate projects in Emacs easily

;; Copyright (C) 2011-2012 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/projectile
;; Version: 0.6
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

;; variables
(defvar projectile-project-root-files '(".git" ".hg" ".bzr" ".projectile")
  "A list of files considered to mark the root of a project.")

(defvar projectile-ignored-file-extensions '("class" "o" "so" "elc")
  "A list of file extensions ignored by projectile.")

(defvar projectile-projects-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project file names to speed up related operations.")

(defun projectile-invalidate-project-cache ()
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

(defun projectile-get-project-files (directory)
  "List the files in `DIRECTORY' and in its sub-directories."
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
               (not (projectile-ignored-p (concat directory current-file))))
          (setq files-list (append files-list (projectile-get-project-files (concat directory current-file)))))
         ;; check for regular files that are not ignored
         ((and (not (or (string= current-file "./") (string= current-file "../")))
               (not (projectile-string-suffix-p current-file "/"))
               (not (projectile-ignored-extension-p current-file)))
          (setq files-list (cons (expand-file-name (concat directory current-file)) files-list)))))
      ;; cache the resulting list of files
      (when (and projectile-enable-caching (string= directory (projectile-get-project-root)))
        (puthash directory files-list projectile-projects-cache)))
    files-list))

(defun projectile-string-suffix-p (string suffix)
  "Check whether `STRING' ends with `SUFFIX'."
  (string= (substring string (- (length string) (length suffix))) suffix))

(defun projectile-get-project-buffers ()
  "Get a list of project buffers."
  (let ((project-files (projectile-get-project-files (projectile-get-project-root)))
        (buffer-files (mapcar 'buffer-file-name (buffer-list))))
    (mapcar 'get-file-buffer (intersection project-files buffer-files :test 'string=))))

(defun projectile-get-project-buffer-names ()
  "Get a list of project buffer names."
  (mapcar 'buffer-name (projectile-get-project-buffers)))

(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (switch-to-buffer (ido-completing-read "Jump to project buffer: " (projectile-get-project-buffer-names))))

(defun projectile-multi-occur ()
  "Do a `multi-occur' in the project's buffers."
  (interactive)
  (multi-occur (projectile-get-project-buffers)
               (car (occur-read-primary-args))))

(defun projectile-hashify-files (files-list)
  "Make the list of project files `FILES-LIST' ido friendly."
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
  "Create an unique version of a `FILENAME'."
  (let ((filename-parts (reverse (split-string filename "/"))))
    (format "%s/%s" (second filename-parts) (first filename-parts))))

(defun projectile-ignored-p (file)
  "Check if `FILE' should be ignored."
  (loop for ignored in projectile-project-root-files
        when (string= (expand-file-name (concat (projectile-get-project-root) ignored "/")) (expand-file-name file))
        do (return t)
        finally (return nil)))

(defun projectile-ignored-extension-p (file)
  "Check if `FILE' should be ignored based on its extension."
  (let ((ext (file-name-extension file)))
    (member ext projectile-ignored-file-extensions)))

(defun projectile-jump-to-project-file ()
  "Jump to a project's file using ido."
  (interactive)
  (let* ((project-files (projectile-hashify-files
                         (projectile-get-project-files (projectile-get-project-root))))
         (file (ido-completing-read "Jump to project file: "
                                    (loop for k being the hash-keys in project-files collect k))))
    (find-file (gethash file project-files))))

(defun projectile-grep-in-project ()
  "Perform rgrep in the project."
  (interactive)
  (let ((search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search for: " (thing-at-point 'symbol))))
        (root-dir (projectile-get-project-root)))
    (grep-compute-defaults)
    (rgrep search-regexp "* .*" root-dir)))

(defun projectile-regenerate-tags ()
  "Regenerate the project's etags using ctags."
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-get-project-root)))
    (cd project-root)
    (shell-command (format "ctags -Re %s" project-root))
    (cd current-dir)
    (visit-tags-table project-root)))

(defun projectile-replace-in-project ()
  "Replace a string in the project using perl."
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-get-project-root))
        (old-text (read-string "Replace: " (thing-at-point 'symbol)))
        (new-text (read-string "With: ")))
    (shell-command (format "find %s -type d -name .git -prune -o -print| xargs perl -p -i -e 's/%s/%s/g'" project-root old-text new-text))))

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p j") 'projectile-jump-to-project-file)
    (define-key map (kbd "C-c p f") 'projectile-grep-in-project)
    (define-key map (kbd "C-c p b") 'projectile-switch-to-buffer)
    (define-key map (kbd "C-c p o") 'projectile-multi-occur)
    (define-key map (kbd "C-c p r") 'projectile-replace-in-project)
    (define-key map (kbd "C-c p i") 'projectile-invalidate-project-cache)
    (define-key map (kbd "C-c p t") 'projectile-regenerate-tags)
    map)
  "Keymap for Projectile mode.")

(easy-menu-define projectile-mode-menu projectile-mode-map
  "Menu for Projectile mode"
  '("Projectile"
    ("Navigating"
     ["Jump to file" projectile-jump-to-project-file]
     ["Jump to buffer" projectile-switch-to-buffer])

    ("Find & Replace"
     ["Find in project" projectile-grep-in-project]
     ["Replace in project" projectile-replace-in-project]
     ["Multi-occur in project" projectile-multi-occur])

    ("General"
     ["Invalidate cache" projectile-invalidate-project-cache]
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
  (if projectile-mode
      ;; on start
      (easy-menu-add projectile-mode-menu projectile-mode-map)
    ;; on stop
    (projectile-off)))

(provide 'projectile)
;;; projectile.el ends here
