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

(defmacro helm-projectile-define-key (keymap key def &rest bindings)
  "In KEYMAP, define key sequence KEY1 as DEF1, KEY2 as DEF2 ..."
  (declare (indent defun))
  (let ((ret '(progn)))
    (while key
      (push
       `(define-key ,keymap ,key
          (lambda ()
            (interactive)
            (helm-quit-and-execute-action ,def)))
       ret)
      (setq key (pop bindings)
            def (pop bindings)))
    (reverse ret)))

(defun helm-projectile-vc (dir)
  "A Helm action for jumping to project root using `vc-dir' or Magit.
DIR is a directory to be switched"
  (let ((projectile-require-project-root nil))
    (projectile-vc dir)))

(defun helm-projectile-compile-project (dir)
  "A Helm action for compile a project.
DIR is the project root."
  (let ((helm--reading-passwd-or-string t)
        (default-directory dir))
    (projectile-compile-project helm-current-prefix-arg dir)))

(defun helm-projectile-remove-known-project (_ignore)
  "Delete selected projects.
_IGNORE means the argument does not matter.
It is there because Helm requires it."
  (let* ((projects (helm-marked-candidates :with-wildcard t))
         (len (length projects)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      projects
      (if (not (y-or-n-p (format "Delete *%s Projects(s)" len)))
          (message "(No deletion performed)")
        (progn
          (mapc (lambda (p)
                  (delete p projectile-known-projects))
                projects)
          (projectile-save-known-projects))
        (message "%s Projects(s) deleted" len)))))

(defvar helm-source-projectile-projects
  `((name . "Projectile projects")
    (candidates . (lambda ()
                    (if (projectile-project-p)
                        (cons (abbreviate-file-name (projectile-project-root))
                              (projectile-relevant-known-projects))
                      projectile-known-projects)))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (helm-projectile-define-key map
                   (kbd "C-d") 'dired
                   (kbd "M-g") 'helm-projectile-vc
                   (kbd "M-e") 'helm-projectile-switch-to-eshell
                   (kbd "C-s") 'helm-find-files-grep
                   (kbd "M-c") 'helm-projectile-compile-project
                   (kbd "M-D") 'helm-projectile-remove-known-project)
                 map))
    (action . (("Switch to project" .
                (lambda (project)
                  (let ((projectile-completion-system 'helm))
                    (projectile-switch-project-by-name project))))
               ("Open Dired in project's directory `C-d'" . dired)
               ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
               ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
               ("Grep in projects `C-s'.  With C-u, recurse" . helm-find-files-grep)
               ("Compile project `M-c'. With C-u, new compile command"
                . helm-projectile-compile-project)
               ("Remove project(s) `M-D'" . helm-projectile-remove-known-project))))
  "Helm source for known projectile projects.")

(defun helm-projectile-init-buffer-with-files (project-root files)
  (with-current-buffer (helm-candidate-buffer project-root)
    (set (make-local-variable 'helm-projectile-current-project-root)
         project-root)
    (dolist (file files)
      (insert (concat file "\n")))))

(define-key helm-etags-map (kbd "C-c p f") (lambda ()
                                             (interactive)
                                             (helm-run-after-quit 'helm-projectile-find-file nil)))

(defun helm-projectile-find-files-eshell-command-on-file-action (_candidate)
  (interactive)
  (let* ((helm-ff-default-directory (file-name-directory _candidate)))
    (helm-find-files-eshell-command-on-file _candidate)))

(defun helm-projectile-ff-etags-select-action (_candidate)
  (interactive)
  (let* ((helm-ff-default-directory (file-name-directory _candidate)))
    (helm-ff-etags-select _candidate)))

(defun helm-projectile-switch-to-eshell (dir)
  (interactive)
  (let* ((helm-ff-default-directory (file-name-directory dir)))
    (helm-ff-switch-to-eshell dir)))

(defun helm-projectile-files-in-current-dired-buffer ()
  "Return a list of files (only) in the current dired buffer."
  (let (flist)
    (cl-flet ((fpush (fname) (push fname flist)))
      (save-excursion
        (let (file buffer-read-only)
          (goto-char (point-min))
          (while (not (eobp))
            (save-excursion
              (and (not (eolp))
                   (setq file (dired-get-filename t t)) ; nil on non-file
                   (progn (end-of-line)
                          (funcall #'fpush file))))
            (forward-line 1)))))
    (mapcar 'file-truename (nreverse flist))))

(defun helm-projectile-all-dired-buffers ()
  "Get all current Dired buffers."
  (mapcar (lambda (b)
            (with-current-buffer b (buffer-name)))
          (filter (lambda (b)
                    (with-current-buffer b
                      (and (eq major-mode 'dired-mode)
                           (buffer-name))))
                  (buffer-list))))

(defun helm-projectile-dired-files-new-action (candidate)
  "Create a Dired buffer from chosen files.
CANDIDATE is the selected file, but choose the marked files if available."
  (let ((new-name (completing-read "Select a Dired buffer:"
                                   (helm-projectile-all-dired-buffers)))
        (helm--reading-passwd-or-string t)
        (files (mapcar (lambda (file)
                         (replace-regexp-in-string (projectile-project-root) "" file))
                       (helm-marked-candidates :with-wildcard t)))
        (default-directory (projectile-project-root)))
    ;; create a unique buffer that is unique to any directory in default-directory
    ;; or opened buffer; when Dired is passed with a non-existence directory name,
    ;; it only creates a buffer and insert everything. If a new name user supplied
    ;; exists as default-directory, Dired throws error when insert anything that
    ;; does not exist in current directory.
    (with-current-buffer (dired (cons (make-temp-name new-name)
                                      (if files
                                          files
                                        (list candidate))))
      (when (get-buffer new-name)
        (kill-buffer new-name))
      (rename-buffer new-name))))

(defun helm-projectile-dired-files-add-action (candidate)
  "Add files to a Dired buffer.
CANDIDATE is the selected file. Used when no file is explicitly marked."
  (if (eq (with-helm-current-buffer major-mode) 'dired-mode)
      (let* ((helm--reading-passwd-or-string t)
             (root (projectile-project-root)) ; store root for later use
             (dired-buffer-name (or (and (eq major-mode 'dired-mode) (buffer-name))
                                    (completing-read "Select a Dired buffer:"
                                                     (helm-projectile-all-dired-buffers))))
             (dired-files (with-current-buffer dired-buffer-name
                            (helm-projectile-files-in-current-dired-buffer)))
             (marked-files (helm-marked-candidates :with-wildcard t))
             (files (sort (mapcar (lambda (file)
                                    (replace-regexp-in-string (projectile-project-root) "" file))
                                  (cl-nunion (if marked-files
                                                 marked-files
                                               (list candidate))
                                             dired-files
                                             :test #'string-equal))
                          'string-lessp)))
        (kill-buffer dired-buffer-name)
        ;; Rebind default-directory because after killing a buffer, we
        ;; could be in any buffer and default-directory is set to that
        ;; random buffer
        ;;
        ;; Also use saved root directory, because after killing a buffer,
        ;; we could be outside of current project
        (let ((default-directory root))
          (dired (cons dired-buffer-name files))))
    (error "You're not in a Dired buffer to add.")))

(defun helm-projectile-dired-files-delete-action (candidate)
  "Delete selected entries from a Dired buffer.
CANDIDATE is the selected file.  Used when no file is explicitly marked."
  (let* ((helm--reading-passwd-or-string t)
         (root (projectile-project-root))
         (dired-buffer-name (with-helm-current-buffer (buffer-name)))
         (dired-files (with-current-buffer dired-buffer-name
                        (helm-projectile-files-in-current-dired-buffer)))
         (files (sort (cl-set-exclusive-or (helm-marked-candidates :with-wildcard t)
                                           dired-files
                                           :test #'string-equal) #'string-lessp)))
    (kill-buffer dired-buffer-name)
    ;; similar reason to `helm-projectile-dired-files-add-action'
    (let ((default-directory root))
      (dired (cons dired-buffer-name
                   (if files
                       (mapcar (lambda (file)
                                 (replace-regexp-in-string (projectile-project-root) "" file))
                               files)
                     (list candidate)))))))

(defvar helm-projectile-find-file-map
  (let ((map (copy-keymap helm-find-files-map)))
    (define-key map (kbd "<left>") 'helm-previous-source)
    (define-key map (kbd "<right>") 'helm-next-source)
    (helm-projectile-define-key map
      (kbd "C-c f") 'helm-projectile-dired-from-files
      (kbd "M-e") 'helm-projectile-switch-to-eshell
      (kbd "M-.") 'helm-projectile-ff-etags-select-action
      (kbd "M-!") 'helm-projectile-find-files-eshell-command-on-file-action)
    map))

(defvar helm-projectile-file-actions
  (helm-make-actions
   "Find File" 'helm-find-file-or-marked
   "Find file in Dired" 'helm-point-file-in-dired
   (lambda () (and (locate-library "elscreen") "Find file in Elscreen"))
   'helm-elscreen-find-file
   "Create Dired buffer from files `C-c f'" 'helm-projectile-dired-files-new-action
   "Add files to Dired buffer `C-c a'" 'helm-projectile-dired-files-add-action
   "View file" 'view-file
   "Checksum File" 'helm-ff-checksum
   "Query replace on marked" 'helm-ff-query-replace-on-marked
   "Serial rename files" 'helm-ff-serial-rename
   "Serial rename by symlinking files" 'helm-ff-serial-rename-by-symlink
   "Serial rename by copying files" 'helm-ff-serial-rename-by-copying
   "Open file with default tool" 'helm-open-file-with-default-tool
   "Find file in hex dump" 'hexl-find-file
   "Insert as org link `C-c @'" 'helm-files-insert-as-org-link
   "Open file externally `C-c C-x, C-u to choose'" 'helm-open-file-externally
   "Grep File(s) `C-s, C-u Recurse'" 'helm-find-files-grep
   "Zgrep File(s) `M-g z, C-u Recurse'" 'helm-ff-zgrep
   "Switch to Eshell `M-e'" 'helm-projectile-switch-to-eshell
   "Etags `M-., C-u reload tag file'" 'helm-projectile-ff-etags-select-action
   "Eshell command on file(s) `M-!, C-u take all marked as arguments.'" 'helm-projectile-find-files-eshell-command-on-file-action
   "Find file as root `C-c r'" 'helm-find-file-as-root
   "Ediff File `C-='" 'helm-find-files-ediff-files
   "Ediff Merge File `C-c ='" 'helm-find-files-ediff-merge-files
   "Delete File(s) `M-D'" 'helm-delete-marked-files
   "Copy file(s) `M-C, C-u to follow'" 'helm-find-files-copy
   "Rename file(s) `M-R, C-u to follow'" 'helm-find-files-rename
   "Symlink files(s) `M-S, C-u to follow'" 'helm-find-files-symlink
   "Relsymlink file(s) `C-u to follow'" 'helm-find-files-relsymlink
   "Hardlink file(s) `M-H, C-u to follow'" 'helm-find-files-hardlink
   "Find file other window `C-c o'" 'find-file-other-window
   "Switch to history `M-p'" 'helm-find-files-switch-to-hist
   "Find file other frame `C-c C-o'" 'find-file-other-frame
   "Print File `C-c p, C-u to refresh'" 'helm-ff-print
   "Locate `C-x C-f, C-u to specify locate db'" 'helm-ff-locate)
  "Action for files.")

(defvar helm-source-projectile-files-dwim-list
  `((name . "Projectile Files")
    (init . (lambda ()
              (let* ((project-files (projectile-current-project-files))
                     (files (projectile-select-files project-files)))
                (cond
                 ((= (length files) 1)
                  (find-file (expand-file-name (car files) (projectile-project-root)))
                  (helm-exit-minibuffer))
                 ((> (length files) 1)
                  (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                          files))
                 (t
                  (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                          project-files))))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,(let ((map (copy-keymap helm-find-files-map)))
                 (define-key map (kbd "<left>") 'helm-previous-source)
                 (define-key map (kbd "<right>") 'helm-next-source)
                 map))
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . ,helm-projectile-file-actions))
  "Helm source definition for Projectile files based on context.")

(defvar helm-source-projectile-files-list
  `((name . "Projectile Files")
    (init . (lambda ()
              (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                      (projectile-current-project-files))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,(let ((map (copy-keymap helm-find-files-map)))
                 (helm-projectile-define-key map
                   (kbd "C-c f") 'helm-projectile-dired-files-new-action
                   (kbd "C-c a") 'helm-projectile-dired-files-add-action)
                 map))
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . ,helm-projectile-file-actions))
  "Helm source definition for Projectile files.")

(defvar helm-source-projectile-files-in-all-projects-list
  `((name . "Projectile Files in All Projects")
    (init . (lambda ()
              (let ((projectile-require-project-root nil))
                (helm-projectile-init-buffer-with-files ""
                                                        (projectile-all-project-files)))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,helm-projectile-find-file-map)
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . ,helm-projectile-file-actions)))

(defvar helm-source-projectile-dired-files-list
  `((name . "Projectile Files in current Dired buffer")
    (init . (lambda ()
              (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                      (mapcar (lambda (file)
                                                                (replace-regexp-in-string (projectile-project-root) "" file))
                                                              (helm-projectile-files-in-current-dired-buffer)))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (helm-projectile-define-key map
                   (kbd "C-c o") 'find-file-other-window
                   (kbd "C-c d") 'helm-projectile-dired-files-delete-action)
                 map))
    (help-message . "Select entries to remove in current Dired buffer (actual files won't be deleted)")
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . (("Find File" . helm-find-file-or-marked)
               ("Find file other window `C-c o'" . find-file-other-window)
               ("Remove entry(s) from Dired buffer `C-c d'" . helm-projectile-dired-files-delete-action))))
  "Helm source definition for Projectile delete files.")

(defun helm-projectile-dired-find-dir (dir)
  "Jump to a selected directory DIR from helm-projectile."
  (dired (expand-file-name dir (projectile-project-root)))
  (run-hooks 'projectile-find-dir-hook))

(defun helm-projectile-dired-find-dir-other-window (dir)
  "Jump to a selected directory DIR from helm-projectile."
  (dired-other-window (expand-file-name dir (projectile-project-root)))
  (run-hooks 'projectile-find-dir-hook))

(defvar helm-source-projectile-directories-list
  `((name . "Projectile Directories")
    (candidates . (lambda ()
                    (if projectile-find-dir-includes-top-level
                        (append '("./") (projectile-current-project-dirs))
                      (projectile-current-project-dirs))))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (helm-projectile-define-key map
                   (kbd "C-c o") 'helm-projectile-dired-find-dir-other-window
                   (kbd "M-e")   'helm-projectile-switch-to-eshell
                   (kbd "C-c f") 'helm-projectile-dired-files-new-action
                   (kbd "C-c a") 'helm-projectile-dired-files-add-action
                   (kbd "C-s")   'helm-find-files-grep)
                 map))
    (action . (("Open Dired" . helm-projectile-dired-find-dir)
               ("Open Dired in other window`C-c o'" . helm-projectile-dired-find-dir)
               ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
               ("Create Dired buffer from files `C-c f'" . helm-projectile-dired-files-new-action)
               ("Add files to Dired buffer `C-c a'" . helm-projectile-dired-files-add-action)
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
    (action . ,(cdr (helm-get-actions-from-type
                     helm-source-locate))))
  "Helm source definition.")

(defvar helm-source-projectile-files-and-dired-list
  '(helm-source-projectile-dired-files-list
    helm-source-projectile-files-list))

(defvar helm-source-projectile-directories-and-dired-list
  '(helm-source-projectile-dired-files-list
    helm-source-projectile-directories-list))

(defcustom helm-projectile-sources-list
  '(helm-source-projectile-projects
    helm-source-projectile-buffers-list
    helm-source-projectile-recentf-list
    helm-source-projectile-files-list
    helm-source-projectile-directories-list
    )
  "Default sources for `helm-projectile'."
  :group 'helm-projectile)


(defmacro helm-projectile-command (command source prompt)
  "Template for generic helm-projectile commands.
COMMAND is a command name to be appended with \"helm-projectile\" prefix.
SOURCE is a Helm source that should be Projectile specific.
PROMPT is a string for displaying as a prompt."
  `(defun ,(intern (concat "helm-projectile-" command)) (&optional arg)
     "Use projectile with Helm for finding files in project

With a prefix ARG invalidates the cache first."
     (interactive "P")
     (if (projectile-project-p)
         (projectile-maybe-invalidate-cache arg))
     (let ((helm-ff-transformer-show-only-basename nil)
           ;; for consistency, we should just let Projectile take care of ignored files
           (helm-boring-file-regexp-list nil))
       (helm :sources ,source
             :buffer "*helm projectile*"
             :prompt (projectile-prepend-project-name ,prompt)))))

(helm-projectile-command "switch-project" 'helm-source-projectile-projects "Switch to project: ")
(helm-projectile-command "find-file" helm-source-projectile-files-and-dired-list "Find file: ")
(helm-projectile-command "find-file-in-known-projects" 'helm-source-projectile-files-in-all-projects-list "Find file in projects: ")
(helm-projectile-command "find-file-dwim" 'helm-source-projectile-files-dwim-list "Find file: ")
(helm-projectile-command "find-dir" helm-source-projectile-directories-and-dired-list "Find dir: ")
(helm-projectile-command "recentf" 'helm-source-projectile-recentf-list "Recently visited file: ")
(helm-projectile-command "switch-to-buffer" 'helm-source-projectile-buffers-list "Switch to buffer: ")

(defun helm-projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions using Helm.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (-if-let (other-files (projectile-get-other-files (buffer-file-name)
                                                    (projectile-current-project-files)
                                                    flex-matching))
      (if (= (length other-files) 1)
          (find-file (expand-file-name (car other-files) (projectile-project-root)))
        (progn
          (let* ((helm-ff-transformer-show-only-basename nil))
            (helm :sources `((name . "Projectile Other Files")
                             (init . (lambda ()
                                       (helm-projectile-init-buffer-with-files (projectile-project-root)
                                                                               other-files)))
                             (coerce . helm-projectile-coerce-file)
                             (candidates-in-buffer)
                             (keymap . ,(let ((map (copy-keymap helm-find-files-map)))
                                          (define-key map (kbd "<left>") 'helm-previous-source)
                                          (define-key map (kbd "<right>") 'helm-next-source)
                                          map))
                             (help-message . helm-find-file-help-message)
                             (mode-line . helm-ff-mode-line-string)
                             (type . file)
                             (action . ,helm-projectile-file-actions))
                  :buffer "*helm projectile*"
                  :prompt (projectile-prepend-project-name "Find other file: ")))))
    (error "No other file found")))

(defun helm-projectile-grep-or-ack (&optional use-ack-p ack-ignored-pattern ack-executable)
  "Perform helm-grep at project root.
USE-ACK-P indicates whether to use ack or not.
ACK-IGNORED-PATTERN is a file regex to exclude from searching.
ACK-EXECUTABLE is the actual ack binary name.
It is usually \"ack\" or \"ack-grep\".
If it is nil, or ack/ack-grep not found then use default grep command."
  (let* ((default-directory (projectile-project-root))
         (helm-ff-default-directory (projectile-project-root))
         (follow (and helm-follow-mode-persistent
                      (assoc-default 'follow helm-source-grep)))
         (helm-grep-in-recurse t)
         (grep-find-ignored-files (-union (-map (lambda (dir) (s-chop-suffix "/" (file-relative-name dir default-directory)))
                                                (cdr (projectile-ignored-directories))) grep-find-ignored-directories))
         (grep-find-ignored-directories (-union (-map (lambda (dir) (s-chop-suffix "/" (file-relative-name dir default-directory)))
                                                      (cdr (projectile-ignored-directories))) grep-find-ignored-directories))
         (helm-grep-default-command (if use-ack-p
                                        (concat ack-executable " -H --smart-case --no-group --no-color " ack-ignored-pattern " %p %f")
                                      "grep -a -d recurse %e -n%cH -e %p %f"))
         (helm-source-grep
          (helm-build-async-source
              (capitalize (helm-grep-command t))
            :header-name (lambda (name)
                           (let ((name (if use-ack-p
                                           "Ack"
                                         "Grep")))
                             (concat name " " "(C-c ? Help)")))
            :candidates-process 'helm-grep-collect-candidates
            :filter-one-by-one 'helm-grep-filter-one-by-one
            :candidate-number-limit 9999
            :nohighlight t
            :mode-line helm-grep-mode-line-string
            ;; We need to specify keymap here and as :keymap arg [1]
            ;; to make it available in further resuming.
            :keymap helm-grep-map
            :history 'helm-grep-history
            :action (helm-make-actions
                     "Find File" 'helm-grep-action
                     "Find file other frame" 'helm-grep-other-frame
                     (lambda () (and (locate-library "elscreen")
                                     "Find file in Elscreen"))
                     'helm-grep-jump-elscreen
                     "Save results in grep buffer" 'helm-grep-save-results
                     "Find file other window" 'helm-grep-other-window)
            :persistent-action 'helm-grep-persistent-action
            :persistent-help "Jump to line (`C-u' Record in mark ring)"
            :requires-pattern 2)))
    (helm
     :sources 'helm-source-grep
     :input (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'symbol))
     :buffer (format "*helm %s*" (if use-ack-p
                                     "ack"
                                   "grep"))
     :default-directory (projectile-project-root)
     :keymap helm-grep-map 
     :history 'helm-grep-history
     :truncate-lines t)))

(defun helm-projectile-on ()
  "Turn on helm-projectile key bindings."
  (interactive)
  (message "Turn on helm-projectile key bindings")
  (helm-projectile-toggle 1))

(defun helm-projectile-off ()
  "Turn off helm-projectile key bindings."
  (interactive)
  (message "Turn off helm-projectile key bindings")
  (helm-projectile-toggle -1))

(defun helm-projectile-grep ()
  "Helm version of projectile-grep."
  (interactive)
  (helm-projectile-grep-or-ack nil))

(defun helm-projectile-ack ()
  "Helm version of projectile-ack."
  (interactive)
  (let ((ack-ignored (mapconcat
                      'identity
                      (-union (-map (lambda (path)
                                      (concat "--ignore-dir=" (file-name-nondirectory (directory-file-name path))))
                                    (projectile-ignored-directories))
                              (-map (lambda (path)
                                      (concat "--ignore-file=is:" (file-relative-name path default-directory)))
                                    (projectile-ignored-files))) " "))
        (helm-ack-grep-executable (cond
                                   ((executable-find "ack") "ack")
                                   ((executable-find "ack-grep") "ack-grep")
                                   (t (error "ack or ack-grep is not available.")))))
    (helm-projectile-grep-or-ack t ack-ignored helm-ack-grep-executable)))


(defun helm-projectile-ag ()
  "Helm version of projectile-ag."
  (interactive)
  (unless (executable-find "ag")
    (error "ag not available"))
  (if (require 'helm-ag nil  'noerror)
      (let ((helm-ag-insert-at-point 'symbol))
        (helm-do-ag (projectile-project-root)))
    (error "helm-ag not available")))

(defun helm-projectile-commander-bindings ()
  (def-projectile-commander-method ?a
    "Run ack on project."
    (call-interactively 'helm-projectile-ack))

  (def-projectile-commander-method ?A
    "Find ag on project."
    (call-interactively 'helm-projectile-ag))

  (def-projectile-commander-method ?f
    "Find file in project."
    (helm-projectile-find-file))

  (def-projectile-commander-method ?b
    "Switch to project buffer."
    (helm-projectile-switch-to-buffer))

  (def-projectile-commander-method ?d
    "Find directory in project."
    (helm-projectile-find-dir))

  (def-projectile-commander-method ?g
    "Run grep on project."
    (helm-projectile-grep))

  (def-projectile-commander-method ?s
    "Switch project."
    (helm-projectile-switch-project))

  (def-projectile-commander-method ?e
    "Find recently visited file in project."
    (helm-projectile-recentf)))

(defun helm-projectile-toggle (toggle)
  "Toggle Helm version of Projectile commands."
  (if (> toggle 0)
      (progn
        (define-key projectile-command-map (kbd "a") 'helm-projectile-find-other-file)
        (define-key projectile-command-map (kbd "f") 'helm-projectile-find-file)
        (define-key projectile-command-map (kbd "F") 'helm-projectile-find-file-in-known-projects)
        (define-key projectile-command-map (kbd "g") 'helm-projectile-find-file-dwim)
        (define-key projectile-command-map (kbd "d") 'helm-projectile-find-dir)
        (define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)
        (define-key projectile-command-map (kbd "e") 'helm-projectile-recentf)
        (define-key projectile-command-map (kbd "b") 'helm-projectile-switch-to-buffer)
        (define-key projectile-command-map (kbd "s a") 'helm-projectile-ack)
        (define-key projectile-command-map (kbd "s g") 'helm-projectile-grep)
        (define-key projectile-command-map (kbd "s s") 'helm-projectile-ag)
        (helm-projectile-commander-bindings))
    (progn
      (define-key projectile-command-map (kbd "a") 'projectile-find-other-file)
      (define-key projectile-command-map (kbd "f") 'projectile-find-file)
      (define-key projectile-command-map (kbd "F") 'projectile-find-file-in-known-projects)
      (define-key projectile-command-map (kbd "g") 'helm-projectile-find-file-dwim)
      (define-key projectile-command-map (kbd "d") 'projectile-find-dir)
      (define-key projectile-command-map (kbd "p") 'projectile-switch-project)
      (define-key projectile-command-map (kbd "e") 'projectile-recentf)
      (define-key projectile-command-map (kbd "b") 'projectile-switch-to-buffer)
      (define-key projectile-command-map (kbd "s a") 'projectile-ack)
      (define-key projectile-command-map (kbd "s g") 'projectile-grep)
      (define-key projectile-command-map (kbd "s s") 'projectile-ag)
      (projectile-commander-bindings))))

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
