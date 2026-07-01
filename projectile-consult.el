;;; projectile-consult.el --- Consult integration for Projectile -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience, matching
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (projectile "3.0.0") (consult "2.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; An optional layer that drives a streaming Consult finder from
;; Projectile's own project indexing command (via
;; `projectile-project-files-producer').
;;
;; Unlike `consult-find' / `consult-fd', which run their own find/fd, this
;; reuses whatever indexing Projectile is configured to use - `git ls-files',
;; `fd', the `find' fallback, other VCS commands - so it stays VCS-aware and
;; honours the project's setup.  Candidates stream into the minibuffer as the
;; command runs and are narrowed client-side by your completion style (e.g.
;; orderless), so there's no wait for the whole project to be indexed before
;; you can start typing.
;;
;; Projectile core does NOT load this file; require it yourself (it pulls in
;; `consult'):
;;
;;   (require 'projectile-consult)
;;   (define-key projectile-command-map (kbd "f") #'projectile-consult-find-file)
;;
;; It is intentionally self-contained so it can be split out into its own
;; package in the future.

;;; Code:

(require 'projectile)

;; `consult' is a hard runtime dependency (see Package-Requires), but
;; Projectile core deliberately does not load this file, and external build
;; systems byte-compile it as part of the Projectile package without `consult'
;; on the load path.  Load it softly here (the real `require' happens in the
;; command below) and forward-declare the functions we use, so the file still
;; byte-compiles cleanly when `consult' is absent.  See
;; https://github.com/bbatsov/projectile/issues/2037.
(require 'consult nil t)

(declare-function consult--read "consult")
(declare-function consult--process-collection "consult")
(declare-function consult--async-map "consult")

(defvar projectile-consult-find-file-history nil
  "Minibuffer history for `projectile-consult-find-file'.")

(defun projectile-consult--file-command (command)
  "Wrap a Projectile indexing COMMAND for Consult's process collection.
COMMAND emits NUL-separated paths (Projectile uses -z/-0/-print0), but
Consult's process pipeline splits its input on newlines, so translate
NUL to newline with `tr'.  Returns a command list (PROGRAM ARGS...)
suitable as the return value of a Consult process builder."
  ;; Octal \\000 rather than \\0 so the escape is understood by both GNU and
  ;; BSD `tr'.
  (list shell-file-name shell-command-switch
        (concat command " | tr '\\000' '\\n'")))

(defun projectile-consult--builder (command)
  "Return a Consult process builder that lists the project's files via COMMAND.
The builder ignores its input: Projectile's indexing command lists the
whole project once (Consult does not restart it while the command stays
the same) and Consult narrows the streamed candidates client-side."
  (lambda (_input)
    (projectile-consult--file-command command)))

;;;###autoload
(defun projectile-consult-find-file ()
  "Find a file in the current project with a streaming Consult UI.

The candidate list is produced by Projectile's own indexing command (see
`projectile-project-files-producer'), so it honours the project's VCS and
indexing configuration, and it streams into the minibuffer as the command
runs instead of blocking until the whole project is indexed."
  (interactive)
  (require 'consult)
  (let* ((producer (projectile-project-files-producer))
         (root (plist-get producer :directory))
         (command (plist-get producer :command)))
    (unless command
      (user-error "External-command indexing is disabled for this project"))
    (let* ((default-directory root)
           (selected
            (consult--read
             (consult--process-collection (projectile-consult--builder command)
               ;; List immediately, with no input required.
               :min-input 0
               ;; Match Projectile's own `./'-stripping.
               :transform (consult--async-map
                           (lambda (path) (string-remove-prefix "./" path)))
               ;; Route the process through TRAMP for remote projects.
               :file-handler t)
             :prompt (format "Find file in %s: " (projectile-project-name))
             :sort nil
             :require-match t
             :category 'file
             :history '(:input projectile-consult-find-file-history))))
      (find-file (expand-file-name selected root)))))

(provide 'projectile-consult)

;;; projectile-consult.el ends here
