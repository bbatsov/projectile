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

(defvar projectile-project-root-files '(".git" ".hg" ".bzr"))

(defun projectile-get-project-root ()
  (loop for file in projectile-project-root-files
        when (locate-dominating-file default-directory file)
        do (return it)))

(defun projectile-get-project-files (directory)
  "List the files in DIRECTORY and in its sub-directories."
  ;; while we are in the current directory
  (let (files-list) 
    (dolist (current-file (directory-files directory t) files-list)
      (cond
       ((and (file-directory-p current-file)
             (string= (expand-file-name current-file) current-file)
             (not (projectile-ignored-p current-file)))
        (setq files-list (append files-list (projectile-get-project-files current-file))))
       ((and (string= (expand-file-name current-file) current-file)
             (not (file-directory-p current-file))) (setq files-list (cons current-file files-list)))))))

(defun projectile-hashify-files (files-list)
  (let ((files-table (make-hash-table :test 'equal)))
    (dolist (current-file files-list files-table)
      (let ((basename (file-name-nondirectory current-file)))
        (if (gethash basename files-table)
            (puthash (uniquify-file current-file) current-file files-table)
          (puthash basename current-file files-table))))))

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

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p j") 'projectile-jump-to-file)
    map)
  "Keymap for Projectile mode."
  )

(easy-menu-define projectile-mode-menu projectile-mode-map
  "Menu for Projectile mode"
  '("Projectile"
    ("Navigating"
     ["Jump to file" projectile-jump-to-project-file])

    ("Test")))


;; define minor mode
(define-globalized-minor-mode projectile-global-mode projectile-mode projectile-on)

(defun projectile-on () 
  (when (projectile-get-project-root) 
    (projectile-mode 1)))

(define-minor-mode projectile-mode "Minor mode to assist project management and navigation."
  :lighter " Projectile"
  :keymap projectile-mode-map)

(provide 'projectile)
;;; projectile.el ends here
