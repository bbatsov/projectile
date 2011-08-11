;;; projectile.el --- Manage and navigate projects in Emacs easily

;; Copyright (C) 2011 
;; Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Projectile
;; Git: git://github.com/bbatsov/projectile.git
;; Version: 0.1
;; Created: 2011-31-07
;; Keywords: project, convenience
;; EmacsWiki: Projectile

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

;; This library provides easy project management and navigation.
(require 'cl)
(require 'easymenu)
(require 'thingatpt)

(defvar projectile-project-root-files '(".git" ".hg" ".bzr" ".projectile"))

(defvar projectile-projects-cache (make-hash-table :test 'equal))

(defun projectile-invalidate-project-cache ()
  (interactive)
  (remhash (projectile-get-project-root) projectile-projects-cache))

(defun projectile-get-project-root ()
  (loop for file in projectile-project-root-files
        when (locate-dominating-file default-directory file)
        do (return it)))

(defun projectile-get-project-files (directory)
  "List the files in DIRECTORY and in its sub-directories."
  ;; while we are in the current directory
  (if (gethash directory projectile-projects-cache)
      (gethash directory projectile-projects-cache)
    (let (files-list) 
      (dolist (current-file (directory-files directory t) files-list)
        (cond
         ((and (file-directory-p current-file)
               (string= (expand-file-name current-file) current-file)
               (not (projectile-ignored-p current-file)))
          (setq files-list (append files-list (projectile-get-project-files current-file))))
         ((and (string= (expand-file-name current-file) current-file)
               (not (file-directory-p current-file))) (setq files-list (cons current-file files-list)))))
      (puthash directory files-list projectile-projects-cache)
      files-list)))

(defun projectile-get-project-buffers ()
  (let ((project-files (projectile-get-project-files (projectile-get-project-root)))
        (buffer-files (mapcar 'buffer-file-name (buffer-list))))
    (mapcar 'get-file-buffer (intersection project-files buffer-files :test 'string=))
    ))

(defun projectile-get-project-buffer-names ()
  (mapcar 'buffer-name (projectile-get-project-buffers)))

(defun projectile-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (ido-completing-read "Jump to project buffer: " (projectile-get-project-buffer-names))))

(defun projectile-multi-occur ()
  (interactive)
  (multi-occur (projectile-get-project-buffers)
               (car (occur-read-primary-args))))

(defun projectile-hashify-files (files-list)
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
  (let ((filename-parts (reverse (split-string filename "/")))) 
    (format "%s/%s" (second filename-parts) (first filename-parts))))

(defun projectile-ignored-p (file)
  (loop for ignored in projectile-project-root-files
        when (string= (expand-file-name (concat (projectile-get-project-root) ignored)) file) 
        do (return t) 
        finally (return nil)))

(defun projectile-jump-to-project-file ()
  (interactive)
  (let* ((project-files (projectile-hashify-files 
                         (projectile-get-project-files (projectile-get-project-root))))
         (file (ido-completing-read "Jump to project file: "
                                    (loop for k being the hash-keys in project-files collect k))))
    (find-file (gethash file project-files))))

(defun projectile-grep-in-project ()
  (interactive)
  (let ((search-regexp (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search for: " (thing-at-point 'word))))
        (root-dir (projectile-get-project-root)))
    (grep-compute-defaults)
    (rgrep search-regexp "* .*" root-dir)))

(defun projectile-regenerate-tags ()
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-get-project-root)))
    (cd project-root)
    (shell-command (format "ctags -Re %s" project-root))
    (cd current-dir)
    (visit-tags-table project-root)))

(defun projectile-replace-in-project ()
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-get-project-root))
        (old-text (read-string "Replace: " (thing-at-point 'word)))
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
    map)
  "Keymap for Projectile mode."
  )

(easy-menu-define projectile-mode-menu projectile-mode-map
  "Menu for Projectile mode"
  '("Projectile"
    ("Navigating"
     ["Jump to file" projectile-jump-to-project-file])

    ("Search & Replace"
     ["Search in project" projectile-grep-in-project])))

;; define minor mode
(define-globalized-minor-mode projectile-global-mode projectile-mode projectile-on)

(defun projectile-on () 
  (when (projectile-get-project-root)
    (projectile-mode 1)))

(defun projectile-off ()
  (easy-menu-remove)
  )

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
