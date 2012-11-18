;;; projectile.el --- Manage and navigate projects in Emacs easily

;; Copyright (C) 2011-2012 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/projectile
;; Version: 0.7
;; Keywords: project, convenience
;; Package-Requires: ((s "1.0.0"))

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
(require 's)

(defgroup projectile nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience)

(defcustom projectile-enable-caching t
  "Enable project files caching."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-require-project-root t
  "Require the presence of a project root to operate. Otherwise consider
the current directory the project root."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-completion-system 'ido
  "The completion system to be used by Projectile."
  :group 'projectile
  :type 'symbol
  :options '(ido default))

(defcustom projectile-ack-function 'ack-and-a-half
  "The ack function to use."
  :group 'projectile
  :type 'symbol
  :options '(ack-and-a-half default))

(defcustom projectile-keymap-prefix (kbd "C-c p")
  "Projectile keymap prefix."
  :group 'projectile
  :type 'sexp)

(defcustom projectile-cache-file
  (expand-file-name "projectile.cache" user-emacs-directory)
  "The name of Projectile's cache file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-command "ctags -Re %s"
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

(defcustom projectile-replace-command
  "find %s -type d -name .git -prune -o -print | xargs perl -p -i -e 's/%s/%s/g'"
  "The command Projectile's going to use to replace text in the project."
  :group 'projectile
  :type 'string)

;; variables
(defvar projectile-project-root-files
  '(".projectile" ".git" ".hg" ".bzr" "_darcs" "rebar.config" "pom.xml" "build.sbt" "Gemfile")
  "A list of files considered to mark the root of a project.")

(defvar projectile-globally-ignored-files
  '("TAGS")
  "A list of files globally ignored by projectile.")

(defvar projectile-globally-ignored-directories
  '(".idea" ".eunit")
  "A list of directories globally ignored by projectile.")

(defvar projectile-ignored-file-extensions
  '("class" "o" "so" "elc" "beam" "png" "jpg" "jpeg")
  "A list of file extensions ignored by projectile.")

(defvar projectile-project-compilation-commands
  '(("./rebar compile" .
     (lambda (dir)
       (file-exists-p (expand-file-name "rebar" dir))))
    ("rebar compile" .
     (lambda (dir)
       (and (executable-find "rebar")
            (file-exists-p (expand-file-name "rebar.config" dir)))))
    ("make" .
     (lambda (dir)
       (file-exists-p (expand-file-name "Makefile" dir))))
    )
  "A list of pairs of commands and prerequisite lambdas to perform project compilation.")

(defvar projectile-project-test-commands
  '(("./rebar eunit skip_deps=true" .
     (lambda (dir)
       (file-exists-p (expand-file-name "rebar" dir))))
    ("rebar eunit skip_deps=true" .
     (lambda (dir)
       (and (executable-find "rebar")
            (file-exists-p (expand-file-name "rebar.config" dir)))))
    ("make test" .
     (lambda (dir)
       (file-exists-p (expand-file-name "Makefile" dir))))
    )
  "A list of pairs of commands and prerequisite lambdas to perform project compilation.")

(defvar projectile-projects-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project file names to speed up related operations.")

(defun projectile-invalidate-cache ()
  "Remove the current project's files from `projectile-projects-cache'."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (remhash project-root projectile-projects-cache)
    (message "Invalidated Projectile cache for %s" project-root))
  (projectile-serialize-cache))

(defun projectile-reindex-current-project ()
  (interactive)
  (projectile-invalidate-cache)
  (let ((project-root (projectile-project-root)))
    (projectile-cache-project project-root
                              (projectile-index-directory project-root))))

(defun projectile-cache-project (project files)
  (when (and projectile-enable-caching)
    (puthash project files projectile-projects-cache)
    (projectile-serialize-cache)))

(defun projectile-project-root ()
  "Retrieves the root directory of a project if available. The current
directory is assumed to be the project root otherwise."
  (or (loop for file in projectile-project-root-files
            when (locate-dominating-file default-directory file)
            do (return it))
      (if projectile-require-project-root
          (error "You're not into a project.")
        default-directory)))

(defun projectile-project-name ()
  "Return project name."
  (file-name-nondirectory (directory-file-name (projectile-project-root))))

(defun projectile-project-files (directory)
  "List the files in DIRECTORY and in its sub-directories."
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                         (gethash directory projectile-projects-cache))))
    ;; cache disabled or cache miss
    (unless files-list
      (message "Projectile is indexing %s. This may take a while." directory)
      (setq files-list (projectile-index-directory directory))
      ;; cache the resulting list of files
      (projectile-cache-project directory files-list))
    files-list))

(defun projectile-index-directory (directory)
  (let (files-list)
    (dolist (current-file (file-name-all-completions "" directory) files-list)
      (let ((absolute-file (file-name-as-directory (expand-file-name current-file directory))))
        (cond
         ;; check for directories that are not ignored
         ((and (s-ends-with-p "/" current-file)
               (not (or (string= current-file "./") (string= current-file "../")))
               (not (projectile-ignored-p absolute-file))
               (not (projectile-ignored-directory-p absolute-file)))
          (setq files-list (append files-list (projectile-index-directory (expand-file-name current-file directory)))))
         ;; check for regular files that are not ignored
         ((and (not (or (string= current-file "./") (string= current-file "../")))
               (not (s-ends-with-p "/" current-file))
               (not (projectile-ignored-extension-p current-file))
               (not (projectile-ignored-file-p absolute-file))
               (setq files-list (cons (expand-file-name (expand-file-name current-file directory)) files-list)))))))))

(defun projectile-project-buffers ()
  "Get a list of project buffers."
  (let ((project-files (projectile-project-files (projectile-project-root)))
        (buffer-files (mapcar 'buffer-file-name (buffer-list))))
    (mapcar 'get-file-buffer
            (intersection project-files buffer-files :test 'string=))))

(defun projectile-project-buffer-names ()
  "Get a list of project buffer names."
  (mapcar 'buffer-name (projectile-project-buffers)))

(defun projectile-prepend-project-name (string)
  (format "[%s] %s" (projectile-project-name) string))

(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (switch-to-buffer
   (projectile-completing-read
    "Switch to buffer: "
    (projectile-project-buffer-names))))

(defun projectile-multi-occur ()
  "Do a `multi-occur' in the project's buffers."
  (interactive)
  (multi-occur (projectile-project-buffers)
               (car (occur-read-primary-args))))

(defun projectile-hashify-files (files-list)
  "Make the list of project files FILES-LIST ido friendly."
  (let ((files-table (make-hash-table :test 'equal))
        (files-to-uniquify nil))
    (dolist (current-file files-list files-table)
      (let ((basename (file-name-nondirectory current-file)))
        (if (gethash basename files-table)
            (progn
              (puthash
               (projectile-uniquify-file current-file)
               current-file files-table)
              (when basename (push basename files-to-uniquify)))
          (puthash basename current-file files-table))))
    ;; uniquify remaining files
    (dolist (current-file (remove-duplicates files-to-uniquify :test 'string=))
      (puthash
       (projectile-uniquify-file (gethash current-file files-table))
       (gethash current-file files-table) files-table)
      (remhash current-file files-table))
    files-table))

(defun projectile-uniquify-file (filename)
  "Create an unique version of a FILENAME."
  (let ((filename-parts (reverse (split-string filename "/"))))
    (format "%s/%s" (second filename-parts) (first filename-parts))))

(defun projectile-ignored-p (file)
  "Check if FILE should be ignored."
  (find-if
   (lambda (root-file)
     (string= file (projectile-expand-root root-file)))
   projectile-project-root-files))

(defun projectile-ignored-directory-p (directory)
  "Check if DIRECTORY should be ignored."
  (member directory (projectile-ignored-directories)))

(defun projectile-ignored-file-p (file)
  "Check if FILE should be ignored."
  (member file (projectile-ignored-files)))

(defun projectile-ignored-extension-p (file)
  "Check if FILE should be ignored based on its extension."
  (let ((ext (file-name-extension file)))
    (member ext projectile-ignored-file-extensions)))

(defun projectile-ignored-files ()
  "Return list of ignored files."
  (mapcar
   'projectile-expand-root
   (append
    projectile-globally-ignored-files
    (projectile-project-ignored-files))))

(defun projectile-ignored-directories ()
  "Return list of ignored directories."
  (mapcar
   'projectile-expand-root
   (append
    projectile-globally-ignored-directories
    (projectile-project-ignored-directories))))

(defun projectile-project-ignored-files ()
  "Return list of project ignored files."
  (delete-if 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories."
  (delete-if-not 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored ()
  "Return list of project ignored files/directories."
  (let ((patterns (projectile-parse-ignore-file))
        (default-directory (projectile-project-root)))
    (apply 'append
           (mapcar
            (lambda (pattern)
              (file-expand-wildcards pattern t))
            patterns))))

(defun projectile-ignore-file ()
  (expand-file-name ".projectile" (projectile-project-root)))

(defun projectile-parse-ignore-file ()
  "Parse project ignore file and return list of ignores."
  (let ((ignore-file (projectile-ignore-file)))
    (when (file-exists-p ignore-file)
      (with-temp-buffer
        (insert-file-contents-literally ignore-file)
        (let ((split-string-default-separators "[\r\n]"))
          (mapcar 's-trim (delete "" (split-string (buffer-string)))))))))

(defun projectile-expand-root (name)
  "Expand NAME to project root."
  (file-name-as-directory
   (expand-file-name name (projectile-project-root))))

(defun projectile-completing-read (prompt choices)
  (let ((prompt (projectile-prepend-project-name prompt)))
    (cond
     ((eq projectile-completion-system 'ido) (ido-completing-read prompt choices))
     (t (completing-read prompt choices)))))

(defun projectile-current-project-files ()
  (projectile-project-files (projectile-project-root)))

(defun projectile-hash-keys (hash)
  (loop for k being the hash-keys in hash collect k))

(defun projectile-find-file ()
  "Jump to a project's file using completion."
  (interactive)
  (let* ((project-files (projectile-hashify-files
                         (projectile-current-project-files)))
         (file (projectile-completing-read "File file: "
                                           (projectile-hash-keys project-files))))
    (find-file (gethash file project-files))))

(defun projectile-grep ()
  "Perform rgrep in the project."
  (interactive)
  (let ((search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string (projectile-prepend-project-name "Grep for: ") (thing-at-point 'symbol))))
        (root-dir (projectile-project-root)))
    (require 'grep)
    (let ((grep-find-ignored-directories (append  (projectile-ignored-directories) grep-find-ignored-directories))
          (grep-find-ignored-files (append (projectile-ignored-files) grep-find-ignored-files)))
      (grep-compute-defaults)
      (rgrep search-regexp "* .*" root-dir))))

(defun projectile-ack ()
  "Run an `ack-and-a-half' search in the project."
  (interactive)
  (let ((ack-and-a-half-arguments
         (mapcar
          (lambda (path)
            (concat "--ignore-dir=" (file-name-nondirectory (directory-file-name path))))
          (projectile-ignored-directories))))
    (call-interactively projectile-ack-function)))


(defun projectile-regenerate-tags ()
  "Regenerate the project's etags."
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-project-root)))
    (cd project-root)
    (shell-command (format projectile-tags-command project-root))
    (cd current-dir)
    (visit-tags-table project-root)))

(defun projectile-replace ()
  "Replace a string in the project using perl."
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-project-root))
        (old-text (read-string "Replace: " (thing-at-point 'symbol)))
        (new-text (read-string "With: ")))
    (shell-command
     (format projectile-replace-command
             project-root old-text new-text))))

(defun projectile-kill-buffers ()
  "Kill all project buffers."
  (interactive)
  (let* ((buffers (projectile-project-buffer-names))
         (question
          (format
           "Are you sure you want to kill %d buffer(s) for '%s'? "
           (length buffers)
           (projectile-project-name))))
    (if (yes-or-no-p question)
        (mapc 'kill-buffer buffers))))

(defun projectile-dired ()
  "Opens dired at the root of the project"
  (interactive)
  (dired (projectile-project-root)))

(defun projectile-recentf ()
  "Shows a list of recently visited files in a project"
  (interactive)
  (if (boundp 'recentf-list)
      (let ((recent-project-files
             (projectile-hashify-files
              (intersection (projectile-current-project-files)
                            recentf-list
                            :test 'string=))))
        (find-file (gethash
                    (projectile-completing-read "Recently visited files:"
                                                (projectile-hash-keys recent-project-files))
                    recent-project-files)))
    (message "recentf is not enabled")))

(defun projectile-open ()
  (interactive)
  (projectile-completing-read "Open project:"
                              (projectile-hash-keys projectile-projects-cache)))

(defun projectile-serialize-cache ()
  (with-temp-buffer
    (insert (prin1-to-string projectile-projects-cache))
    (when (file-writable-p projectile-cache-file)
      (write-region (point-min)
                    (point-max)
                    projectile-cache-file))))

(defun projectile-load-cache ()
  (when (file-exists-p projectile-cache-file)
    (with-temp-buffer
      (insert-file-contents projectile-cache-file)
      (setq projectile-projects-cache (read (buffer-string))))))

(defun projectile-run-project-command (checks)
  (let* ((dir (or (projectile-project-root)
                  (file-name-directory (buffer-file-name))))
         (pref (concat "cd " dir " && "))
         (cmd (projectile-get-project-compile-command dir checks)))
    (if cmd
        (compilation-start (concat pref cmd)))
    ))

(defun projectile-compile-project ()
  (interactive)
  (projectile-run-project-command projectile-project-compilation-commands)
  )

(defun projectile-test-project ()
  (interactive)
  (projectile-run-project-command projectile-project-test-commands)
  )

(defun projectile-get-project-compile-command (dir checks)
  "Retrieves the root directory of a project if available."
  (loop for (command . check) in checks
        when (funcall check dir)
        do (return command)
        finally (return nil)))

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
      (define-key prefix-map (kbd "d") 'projectile-dired)
      (define-key prefix-map (kbd "e") 'projectile-recentf)
      (define-key prefix-map (kbd "a") 'projectile-ack)
      (define-key prefix-map (kbd "l") 'projectile-compile-project)
      (define-key prefix-map (kbd "p") 'projectile-test-project)

      (define-key map projectile-keymap-prefix prefix-map))
    map)
  "Keymap for Projectile mode.")

(easy-menu-define projectile-mode-menu projectile-mode-map
  "Menu for Projectile mode"
  '("Projectile"
    ("Navigating"
     ["File file" projectile-find-file]
     ["Switch to buffer" projectile-switch-to-buffer])

    ("Find & Replace"
     ["Find in project (grep)" projectile-grep]
     ["Find in project (ack)" projectile-ack]
     ["Replace in project" projectile-replace]
     ["Multi-occur in project" projectile-multi-occur])

    ("General"
     ["Invalidate cache" projectile-invalidate-cache]
     ["Regenerate etags" projectile-regenerate-tags])))

;; define minor mode
;;;###autoload
(define-globalized-minor-mode projectile-global-mode
  projectile-mode
  projectile-on)

(defun projectile-on ()
  "Enable Projectile minor mode."
  (projectile-mode 1))

(defun projectile-off ()
  "Disable Projectile minor mode."
  (projectile-mode -1))

;;;###autoload
(define-minor-mode projectile-mode
  "Minor mode to assist project management and navigation."
  :lighter " Projectile"
  :keymap projectile-mode-map
  :group 'projectile
  (if projectile-mode
      ;; on start
      (progn
        (projectile-load-cache)
        (easy-menu-add projectile-mode-menu projectile-mode-map))
    ;; on stop
    (easy-menu-remove)))

(provide 'projectile)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; projectile.el ends here
