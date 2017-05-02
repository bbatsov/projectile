;;; ibuffer-projectile.el --- Group ibuffer's list by PROJECTILE project, or show PROJECTILE status
;;
;; Copyright (C) 2011-2014 Steve Purcell (ibuffer-vc)
;; Copyright (C) 2014 Thomas Frössman
;;
;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: project, convenience, ibuffer
;; Package-Requires: ((cl-lib "0.2"))
;; URL: https://github.com/bbatsov/projectile
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Adds functionality to ibuffer for grouping buffers by their projectile root
;; directory, and for displaying and/or sorting by the projectile status of
;; listed files.
;;
;;; Use:
;;
;; To group buffers by projectile parent dir:
;;
;;   M-x ibuffer-projectile-set-filter-groups-by-projectile-root
;;
;; or, make this the default:
;;
;;   (add-hook 'ibuffer-hook
;;     (lambda ()
;;       (ibuffer-projectile-set-filter-groups-by-projectile-root)
;;       (unless (eq ibuffer-sorting-mode 'alphabetic)
;;         (ibuffer-do-sort-by-alphabetic))))
;;
;; Alternatively, use `ibuffer-projectile-generate-filter-groups-by-projectile-root'
;; to programmatically obtain a list of filter groups that you can
;; combine with your own custom groups.
;;
;;
;;; Code:

;; requires

(require 'ibuffer)
(require 'ibuf-ext)
(require 'projectile)
(require 'cl-lib)


(defgroup ibuffer-projectile nil
  "Group ibuffer entries according to their projectile project root."
  :prefix "ibuffer-projectile-"
  :group 'convenience)

(defcustom ibuffer-projectile-skip-if-remote t
  "If non-nil, don't query the PROJECTILE status of remote files."
  :type 'boolean
  :group 'ibuffer-projectile)

(defcustom ibuffer-projectile-include-function 'identity
  "A function which tells whether a given file should be grouped.

The function is passed a filename, and should return non-nil if the file
is to be grouped.

This option can be used to exclude certain files from the grouping mechanism."
  :type 'function
  :group 'ibuffer-projectile)

;;; Group and filter ibuffer entries by parent projectile directory

(defun ibuffer-projectile--include-file-p (file)
  "Return t iff FILE should be included in ibuffer-projectile's filtering."
  (and file
       (or (null ibuffer-projectile-skip-if-remote)
           (not (file-remote-p file)))
       (file-readable-p file)
       (funcall ibuffer-projectile-include-function file)))


(defvar ibuffer-projectile-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `projectile-project-root`.")

(defun ibuffer-projectile-project-root (file-name)
  (let* ((default-directory file-name)
         (projectile-enable-caching t)
         (cache-key file-name)
         (cache-value (gethash cache-key ibuffer-projectile-project-root-cache)))
    (if cache-value
        (if (eq cache-value 'no-project-root)
            nil
          cache-value)
      (let ((value (projectile-project-p)))
        (puthash cache-key (or value 'no-project-root) ibuffer-projectile-project-root-cache)
        value))))

(defadvice projectile-invalidate-cache (after clear-ibuffer-cache activate)
  (setq ibuffer-projectile-project-root-cache (make-hash-table :test 'equal)))

(defun ibuffer-projectile-root (buf)
  "Return a cons cell (project-name . root-dir) for BUF.
If the file is not under a project root, nil is returned instead."
  (let* ((file-name (with-current-buffer buf (or buffer-file-name default-directory))))
    (when (ibuffer-projectile--include-file-p file-name)
      (let* ((project-root (ibuffer-projectile-project-root file-name))
             (project-name (if project-root (file-name-nondirectory
                                             (directory-file-name project-root)))))
        (if project-root
            (cons project-name project-root))))))

(define-ibuffer-filter projectile-root
    "Toggle current view to buffers with projectile root dir QUALIFIER."
  (:description "projectile root dir"
                :reader (read-from-minibuffer "Filter by projectile root dir (regexp): "))
  (ibuffer-awhen (ibuffer-projectile-root buf)
    (equal qualifier it)))

;;;###autoload
(defun ibuffer-projectile-generate-filter-groups-by-projectile-root ()
  "Create a set of ibuffer filter groups based on the projectile root dirs of buffers."
  (let ((roots (ibuffer-remove-duplicates
                (delq nil (mapcar 'ibuffer-projectile-root (buffer-list))))))
    (mapcar (lambda (projectile-root)
              (cons (format "%s:%s" (car projectile-root) (cdr projectile-root))
                    `((projectile-root . ,projectile-root))))
            roots)))

;;;###autoload
(defun ibuffer-projectile-set-filter-groups-by-projectile-root ()
  "Set the current filter groups to filter by projectile root dir."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-projectile-generate-filter-groups-by-projectile-root))
  (message "ibuffer-projectile: groups set")
  (let ((ibuf (get-buffer "*Ibuffer*")))
    (when ibuf
        (with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t)))))


;;;; ADDIOTIONAL UTILITY FUNCTIONS


;; example usage: I use RET to find file under point i ibuffer, f instead
;; brings up projectile-find-file for project of buffer under point.
;;
;; NOTE: Currently this is straight from my init.el, was written as quick as
;; possible and the implementation will be rewritten in a better way.
;;
;;  (bind-key "f" 'ibuffer-projectile-find-file ibuffer-mode-map)
(defun ibuffer-projectile-find-file ()
  "projectile-find-file using item under point in ibuffer"
  (interactive)
  (--when-let (get-buffer "*Ibuffer*")
    (with-current-buffer it
      (let* ((selected-buffer (ibuffer-current-buffer))
             (buffer-path (with-current-buffer
                              selected-buffer
                            (or (buffer-file-name)
                               list-buffers-directory
                               default-directory)))
             (default-directory
               (if (file-regular-p buffer-path)
                   (file-name-directory buffer-path)
                 buffer-path)))
        (projectile-find-file)))))


;; example usage: Open project without leaving ibuffer which means that the
;; project root directory is inserted under point while not leaving ibuffer.
;;
;; NOTE: Currently this is straight from my init.el, was written as quick as
;; possible and the implementation will be rewritten in a better way.
;;
;; (bind-key "o" 'ibuffer-projectile-dired-known-projects-root ibuffer-mode-map)
;;
(defun ibuffer-projectile-dired-known-projects-root (&optional arg)
  "Insert projectile project root dir under point in ibuffer"
  (interactive "P")
  (use-package projectile)
  (let ((project-to-switch
         (projectile-completing-read "Switch to project: "
                                     projectile-known-projects)))
    (dired project-to-switch)
    (ibuffer)))



(provide 'ibuffer-projectile)
;;; ibuffer-projectile.el ends here
