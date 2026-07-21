;;; projectile-bookmarks-test.el --- Tests for the project bookmarks -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; Tests for the project-scoped bookmark commands, which are a thin project
;; filter over the built-in `bookmark.el'.  Every spec runs with a private,
;; empty `bookmark-alist' and saving disabled, so the user's real bookmark
;; file is never read nor written.

;;; Code:

(require 'projectile-test-helpers)
(require 'bookmark)
(require 'cl-lib)

(defmacro projectile-test-with-bookmarks (bookmarks &rest body)
  "Evaluate BODY with `bookmark-alist' bound to BOOKMARKS.
Bookmark persistence is neutralised for the duration, so the specs never
touch `bookmark-default-file'."
  (declare (indent 1) (debug (form &rest form)))
  `(let ((bookmark-alist ,bookmarks)
         ;; never save, never annotate, never fontify
         (bookmark-save-flag nil)
         (bookmark-watch-bookmark-file nil)
         (bookmark-use-annotations nil)
         (bookmark-fringe-mark nil)
         (bookmark-current-bookmark nil)
         (bookmark-history nil)
         (bookmark-alist-modification-count 0)
         ;; keep the rebuild that `bookmark-store'/`bookmark-delete' do
         ;; away from a real `*Bookmark List*' buffer
         (bookmark-bmenu-buffer " *projectile-test-bookmarks*"))
     ;; The loader is what would read `bookmark-default-file'; stubbing it
     ;; out is both the strongest guarantee that no spec ever touches the
     ;; user's bookmarks and the one thing that works across the Emacs
     ;; versions we support (the "already loaded" flag was renamed).
     (cl-letf (((symbol-function 'bookmark-maybe-load-default-file) #'ignore))
       ,@body)))

(defun projectile-test-bookmark (name file)
  "Return a bookmark record named NAME recording FILE."
  `(,name (filename . ,file) (position . 1)))

(describe "projectile-bookmark-set"
  (it "records a bookmark named after the project"
    (projectile-test-with-stub-root "project" ("foo.el")
      (projectile-test-with-bookmarks nil
        (let ((buffer (find-file-noselect "project/foo.el")))
          (unwind-protect
              (with-current-buffer buffer
                (spy-on 'read-string :and-call-fake
                        (lambda (_prompt initial &rest _) initial))
                (call-interactively #'projectile-bookmark-set)
                ;; the suggested name is what the user got offered...
                (expect (spy-calls-args-for 'read-string 0)
                        :to-equal '("Set bookmark: " "project: foo.el"
                                    bookmark-history "project: foo.el"))
                ;; ...and what ended up in the global bookmark list
                (expect (bookmark-all-names) :to-equal '("project: foo.el"))
                (expect (file-truename (bookmark-get-filename "project: foo.el"))
                        :to-equal (expand-file-name
                                   "foo.el" (projectile-project-root))))
            (kill-buffer buffer))))))

  (it "honours a name the user edited"
    (projectile-test-with-stub-root "project" ("foo.el")
      (projectile-test-with-bookmarks nil
        (let ((buffer (find-file-noselect "project/foo.el")))
          (unwind-protect
              (with-current-buffer buffer
                (spy-on 'read-string :and-return-value "my own name")
                (call-interactively #'projectile-bookmark-set)
                (expect (bookmark-all-names) :to-equal '("my own name"))
                ;; still the project's, thanks to the recorded file
                (expect (projectile-bookmark-names (projectile-project-root))
                        :to-equal '("my own name")))
            (kill-buffer buffer))))))

  (it "refuses a buffer bookmark.el cannot record"
    (projectile-test-with-stub-root "project" ("foo.el")
      (projectile-test-with-bookmarks nil
        (with-temp-buffer
          (spy-on 'read-string)
          (expect (call-interactively #'projectile-bookmark-set)
                  :to-throw 'user-error)
          (expect 'read-string :not :to-have-been-called))))))

(describe "projectile-bookmark-names"
  (it "returns only the current project's bookmarks"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "renamed" (expand-file-name "foo.el" root))
                  (projectile-test-bookmark
                   "other: bar.el" (expand-file-name "../other/bar.el" root))
                  ;; no file, but carries the project's name prefix
                  `("project: some Info node" (position . 1))
                  `("elsewhere" (position . 1)))
          (expect (projectile-bookmark-names root)
                  :to-have-same-items-as '("renamed" "project: some Info node"))))))

  (it "can be restricted to the recorded file"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "renamed" (expand-file-name "foo.el" root))
                  `("project: some Info node" (position . 1)))
          (let ((projectile-bookmark-scope 'file))
            (expect (projectile-bookmark-names root) :to-equal '("renamed")))))))

  (it "can be restricted to the name prefix"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "renamed" (expand-file-name "foo.el" root))
                  `("project: some Info node" (position . 1)))
          (let ((projectile-bookmark-scope 'name))
            (expect (projectile-bookmark-names root)
                    :to-equal '("project: some Info node"))))))))

(describe "projectile-bookmark-jump"
  (it "completes over the current project's bookmarks only"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "project: foo.el" (expand-file-name "foo.el" root))
                  (projectile-test-bookmark
                   "other: bar.el" (expand-file-name "../other/bar.el" root)))
          (spy-on 'projectile-completing-read :and-return-value "project: foo.el")
          (spy-on 'bookmark-jump)
          (call-interactively #'projectile-bookmark-jump)
          (expect (nth 1 (spy-calls-args-for 'projectile-completing-read 0))
                  :to-equal '("project: foo.el"))
          (expect 'bookmark-jump :to-have-been-called-with "project: foo.el")))))

  (it "advertises the bookmark completion category"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "project: foo.el" (expand-file-name "foo.el" root)))
          (spy-on 'projectile-completing-read :and-return-value "project: foo.el")
          (spy-on 'bookmark-jump)
          (call-interactively #'projectile-bookmark-jump)
          (expect (plist-get (cddr (spy-calls-args-for 'projectile-completing-read 0))
                             :category)
                  :to-be 'bookmark)))))

  (it "errors friendly when the project has no bookmarks"
    (projectile-test-with-stub-root "project" ("foo.el")
      (projectile-test-with-bookmarks nil
        (expect (call-interactively #'projectile-bookmark-jump)
                :to-throw 'user-error
                '("[project] No bookmarks in this project")))))

  (it "refuses to jump to a bookmark whose file is gone"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let* ((root (projectile-project-root))
             (gone (expand-file-name "gone.el" root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark "project: gone.el" gone))
          (spy-on 'projectile-completing-read :and-return-value "project: gone.el")
          (spy-on 'bookmark-jump)
          (expect (call-interactively #'projectile-bookmark-jump) :to-throw 'user-error)
          (expect 'bookmark-jump :not :to-have-been-called)))))

  (it "refuses a name that is not a bookmark"
    ;; the prompt doesn't require a match, so the user can type anything
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "project: foo.el" (expand-file-name "foo.el" root)))
          (spy-on 'projectile-completing-read :and-return-value "typo")
          (spy-on 'bookmark-jump)
          (expect (call-interactively #'projectile-bookmark-jump) :to-throw 'user-error)
          (expect 'bookmark-jump :not :to-have-been-called))))))

(describe "projectile-bookmark-delete"
  (it "deletes the selected project bookmark"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "project: foo.el" (expand-file-name "foo.el" root))
                  (projectile-test-bookmark
                   "other: bar.el" (expand-file-name "../other/bar.el" root)))
          (spy-on 'projectile-completing-read :and-return-value "project: foo.el")
          (call-interactively #'projectile-bookmark-delete)
          (expect (bookmark-all-names) :to-equal '("other: bar.el"))
          (expect (projectile-bookmark-names root) :to-equal nil)))))

  (it "refuses a name that is not a bookmark instead of claiming success"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "project: foo.el" (expand-file-name "foo.el" root)))
          (spy-on 'projectile-completing-read :and-return-value "typo")
          (expect (call-interactively #'projectile-bookmark-delete) :to-throw 'user-error)
          (expect (bookmark-all-names) :to-equal '("project: foo.el"))))))

  (it "errors friendly when the project has no bookmarks"
    (projectile-test-with-stub-root "project" ("foo.el")
      (projectile-test-with-bookmarks nil
        (expect (call-interactively #'projectile-bookmark-delete)
                :to-throw 'user-error
                '("[project] No bookmarks in this project"))))))

(describe "projectile-bookmark-names with remote bookmarks"
  (it "keeps a remote bookmark out of a local project without resolving it"
    (projectile-test-with-stub-root "project" ("foo.el")
      (let ((root (projectile-project-root)))
        (projectile-test-with-bookmarks
            (list (projectile-test-bookmark
                   "remote" (concat "/ssh:nosuchhost:" root "foo.el"))
                  (projectile-test-bookmark
                   "local" (expand-file-name "foo.el" root)))
          ;; resolving the remote name would open a TRAMP connection
          (spy-on 'file-truename :and-call-through)
          (expect (projectile-bookmark-names root) :to-equal '("local"))
          (dolist (call (spy-calls-all-args 'file-truename))
            (expect (file-remote-p (car call)) :to-be nil)))))))

;; The "no project at all" errors live in `projectile-friendly-errors-test',
;; next to every other command's.

(provide 'projectile-bookmarks-test)

;;; projectile-bookmarks-test.el ends here
