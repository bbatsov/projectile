;;; helm-projectile.el --- Helm integration for Projectile

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/projectile
;; Created: 2011-31-07
;; Keywords: project, convenience
;; Version: 1.0.0-cvs
;; Package-Requires: ((helm "1.4.0") (projectile "0.9.2"))

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

(require 'projectile)
(require 'helm-config)
(require 'helm-locate)
(require 'helm-buffers)

(defvar helm-source-projectile-files-list
  `((name . "Projectile Files")
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'global (projectile-current-project-files))))
    (candidates-in-buffer)
    (candidate-number-limit . 15)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)
    (action . (lambda (candidate)
                (find-file (projectile-expand-root candidate)))))
  "Helm source definition.")

(defvar helm-source-projectile-buffers-list
  `((name . "Projectile Buffers")
    (init . (lambda ()
              ;; Issue #51 Create the list before `helm-buffer' creation.
              (setq helm-projectile-buffers-list-cache (projectile-project-buffer-names))
              (let ((result (loop for b in helm-projectile-buffers-list-cache
                                  maximize (length b) into len-buf
                                  maximize (length (with-current-buffer b
                                                     (symbol-name major-mode)))
                                  into len-mode
                                  finally return (cons len-buf len-mode))))
                (unless helm-buffer-max-length
                  (setq helm-buffer-max-length (car result)))
                (unless helm-buffer-max-len-mode
                  ;; If a new buffer is longer that this value
                  ;; this value will be updated
                  (setq helm-buffer-max-len-mode (cdr result))))))
    (candidates . helm-projectile-buffers-list-cache)
    (type . buffer)
    (match helm-buffer-match-major-mode)
    (persistent-action . helm-buffers-list-persistent-action)
    (keymap . ,helm-buffer-map)
    (volatile)
    (no-delay-on-input)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

(defvar helm-source-projectile-recentf-list
  `((name . "Projectile Recent Files")
    ;; Needed for filenames with capitals letters.
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'global (projectile-recentf-files))))
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)
    (action . (lambda (candidate)
                (find-file (projectile-expand-root candidate)))))
  "Helm source definition.")

;;;###autoload
(defun helm-projectile ()
  "Use projectile with Helm instead of ido."
  (interactive)
  (helm :sources '(helm-source-projectile-files-list
                   helm-source-projectile-buffers-list
                   helm-source-projectile-recentf-list)
        :buffer "*helm projectile*"
        :prompt (projectile-prepend-project-name "pattern: ")))

;;;###autoload
(eval-after-load 'projectile
    '(define-key projectile-mode-map (kbd "C-c p h") 'helm-projectile))

(provide 'helm-projectile)
;;; helm-projectile.el ends here
