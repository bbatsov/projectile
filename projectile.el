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

(defvar projectile-project-root-files '(".git" ".hg" ".bzr"))

(defun projectile-get-project-root ()
  (loop for file in ffip-top-level-project-files
        when (ffip-locate-dominating-file default-directory file)
        do (return it)))

(defun projectile-get-project-files (directory)
  "List the files in DIRECTORY and in its sub-directories."
  ;; while we are in the current directory
  (let (files-list) 
    (dolist (current-file (directory-files directory t) files-list)
      (message current-file)
      (cond
       ((and (file-directory-p current-file)
             (string= (expand-file-name current-file) current-file)
             (not (projectile-ignored-p current-file)))
        (setq files-list (append files-list (projectile-get-project-files current-file))))
       ((and (string= (expand-file-name current-file) current-file)
             (not (file-directory-p current-file))) (setq files-list (cons current-file files-list)))))))

(defun projectile-hashify-files (files-list)
  (loop for file in files-list collect (list (file-name-nondirectory file) file)))

(defun projectile-ignored-p (file)
  (loop for ignored in projectile-project-root-files
        when (string= (expand-file-name (concat (projectile-get-project-root) ignored)) file) 
        do (return t) 
        finally (return nil)))

(defun projectile-jump-to-project-file ()
  (interactive)
  (let* ((project-files (projectile-hashify-files (projectile-get-project-files (projectile-get-project-root))))
         (file (if (and (boundp 'ido-mode) ido-mode)
                   (ido-completing-read "Open file: "
                                        (mapcar 'car project-files))
                 (completing-read "Open file: "
                                  (mapcar 'car project-files)))))
    (find-file (car (cdr (assoc file project-files))))))

(provide 'projectile)
;;; projectile.el ends here
