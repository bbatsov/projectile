;;; helm-projectile.el --- Helm integration for Projectile

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/projectile
;; Created: 2011-31-07
;; Keywords: project, convenience
;; Version: 0.11.0
;; Package-Requires: ((helm "1.4.0") (projectile "0.11.0") (cl-lib "0.3"))

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
(require 'helm-files)
(require 'cl-lib)

(declare-function eshell "eshell")

(defgroup helm-projectile nil
  "Helm support for projectile."
  :prefix "helm-projectile-"
  :group 'projectile
  :link `(url-link :tag "helm-projectile homepage" "https://github.com/bbatsov/projectile"))

(defvar helm-projectile-current-project-root)

(defun helm-projectile-coerce-file (candidate)
  (with-current-buffer (helm-candidate-buffer)
    (expand-file-name candidate helm-projectile-current-project-root)))

(defun helm-projectile-init-buffer-with-files (project-root files)
  (with-current-buffer (helm-candidate-buffer project-root)
    (set (make-local-variable 'helm-projectile-current-project-root)
         project-root)
    (dolist (file files)
      (insert (concat file "\n")))))

(defmacro helm-projectile-define-key (map key fun)
  (declare (indent 1))
  `(define-key ,map ,key
     (lambda ()
       (interactive)
       (helm-quit-and-execute-action ,fun))))

(defun helm-projectile-switch-to-eshell (dir)
  (interactive)
  (with-helm-default-directory (expand-file-name dir (projectile-project-root))
      (eshell)))

(defun helm-projectile-vc (dir)
  (interactive)
  (with-helm-default-directory (expand-file-name dir (projectile-project-root))
      (projectile-vc)))

(defvar helm-source-projectile-projects
  `((name . "Projectile projects")
    (candidates . (lambda ()
                    (if (projectile-project-p)
                        (cons (abbreviate-file-name (projectile-project-root))
                              (projectile-relevant-known-projects))
                      projectile-known-projects)))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (helm-projectile-define-key map (kbd "C-d") 'dired)
                 (helm-projectile-define-key map
                   (kbd "M-g") 'helm-projectile-vc)
                 (helm-projectile-define-key map
                   (kbd "M-e") 'helm-projectile-switch-to-eshell)
                 (helm-projectile-define-key map
                   (kbd "C-s") 'helm-find-files-grep)
                 map))
    (action . (("Switch to project" .
                (lambda (project)
                  (let ((projectile-completion-system 'helm))
                    (projectile-switch-project-by-name project))))
               ("Open Dired in project's directory `C-d'" . dired)
               ("Open project root in vc-dir or magit `M-g'" .
                helm-projectile-vc)
               ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
               ("Grep in projects `C-s C-u Recurse'" . helm-find-files-grep))))
  "Helm source for known projectile projects.")

(defvar helm-source-projectile-files-list
  `((name . "Projectile Files")
    (init . (lambda ()
              (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                      (projectile-current-project-files))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . (("Find file" . (lambda (files) (helm-find-many-files files)))
               ("Find file other window" . (lambda (file) (find-file-other-window file)))
               ("Open dired in file's directory" . helm-open-dired))))
  "Helm source definition.")

(defun helm-projectile-find-dir (dir)
  "Jump to a selected directory DIR from helm-projectile."
  (dired (expand-file-name dir (projectile-project-root)))
  (run-hooks 'projectile-find-dir-hook))

(defvar helm-source-projectile-directories-list
  `((name . "Projectile Directories")
    (candidates . (lambda ()
              (if projectile-find-dir-includes-top-level
                  (append '("./") (projectile-current-project-dirs))
                (projectile-current-project-dirs))))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (helm-projectile-define-key map (kbd "C-d") 'helm-projectile-find-dir)
                 (helm-projectile-define-key map
                   (kbd "M-e") 'helm-projectile-switch-to-eshell)
                 (helm-projectile-define-key map
                   (kbd "C-s") 'helm-find-files-grep)
                 map))
    (action . (("Open Dired in project's directory `C-d'" . helm-projectile-find-dir)
               ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
               ("Grep in projects `C-s C-u Recurse'" . helm-find-files-grep))))
  "Helm source for listing project directories")

(defvar helm-source-projectile-buffers-list
  `((name . "Projectile Buffers")
    (init . (lambda ()
              ;; Issue #51 Create the list before `helm-buffer' creation.
              (setq helm-projectile-buffers-list-cache (projectile-project-buffer-names))
              (let ((result (cl-loop for b in helm-projectile-buffers-list-cache
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
    (match helm-buffers-list--match-fn)
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
              (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                      (projectile-recentf-files))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . (lambda (file) (find-file file))))
  "Helm source definition.")

(defcustom helm-projectile-sources-list
  '(helm-source-projectile-projects
    helm-source-projectile-buffers-list
    helm-source-projectile-recentf-list
    helm-source-projectile-files-list
    helm-source-projectile-directories-list
    )
  "Default sources for `helm-projectile'."
  :group 'helm-projectile)

;;;###autoload
(defun helm-projectile (&optional arg)
  "Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.
If invoked outside of a project, displays a list of known projects to jump."
  (interactive "P")
  (if (projectile-project-p)
      (projectile-maybe-invalidate-cache arg))
  (let ((helm-ff-transformer-show-only-basename nil)
        (src (if (projectile-project-p)
                 helm-projectile-sources-list
               helm-source-projectile-projects)))
    (helm :sources src
          :buffer "*helm projectile*"
          :prompt (projectile-prepend-project-name (if (projectile-project-p)
                                                       "pattern: "
                                                     "Switch to project: ")))))

;;;###autoload
(eval-after-load 'projectile
  '(progn
     (define-key projectile-command-map (kbd "h") 'helm-projectile)))

(provide 'helm-projectile)

;;; helm-projectile.el ends here
