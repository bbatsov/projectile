;;; helm-projectile.el --- Helm integration for projectile

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
;; This library provides easy project management and navigation. The
;; concept of a project is pretty basic - just a folder containing
;; special file. Currently git, mercurial and bazaar repos are
;; considered projects by default. If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it. See the README for more details.
;;
;;; Code:

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

(provide 'helm-projectile)
;;; helm-projectile.el ends here
