;;; projectile-project-group-test.el --- Tests for commands over a group of projects -*- lexical-binding: t -*-

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

;; Tests for `projectile-find-file-in-projects' and
;; `projectile-search-in-projects' - the two commands that take a list of
;; projects rather than working on the one you're in - and for the
;; known-projects and sibling commands built on them.

;;; Code:

(require 'projectile-test-helpers)

(defmacro projectile-group-test--with-projects (&rest body)
  "Evaluate BODY in a sandbox holding two sibling projects.

Creates `group/alpha/' and `group/beta/', each a `.projectile' project
with a file mentioning `needle', plus `group/alpha/skip.log' which
alpha's dirconfig ignores.  BODY runs with `alpha' and `beta' bound to
the two truename'd roots and `parent' to the directory holding them."
  (declare (indent 0) (debug (&rest form)))
  `(projectile-test-with-sandbox
     (projectile-test-with-files
         ("group/alpha/src/" "group/beta/lib/")
       (with-temp-file "group/alpha/.projectile" (insert "-/skip.log\n"))
       (with-temp-file "group/beta/.projectile")
       (with-temp-file "group/alpha/src/a.txt" (insert "alpha needle here\n"))
       (with-temp-file "group/alpha/skip.log" (insert "ignored needle\n"))
       (with-temp-file "group/beta/lib/b.txt" (insert "beta needle there\n"))
       (let* ((parent (file-truename (expand-file-name "group/")))
              (alpha (file-name-as-directory (expand-file-name "alpha" parent)))
              (beta (file-name-as-directory (expand-file-name "beta" parent)))
              (projectile-indexing-method 'native)
              (projectile-projects-cache (make-hash-table :test 'equal))
              (projectile-projects-cache-time (make-hash-table :test 'equal))
              (projectile-enable-caching nil)
              (case-fold-search t))
         (ignore parent alpha beta)
         (unwind-protect
             (progn ,@body)
           (projectile-test-kill-project-buffers parent))))))

(defun projectile-group-test--search (projects term)
  "Run `projectile-search-in-projects' over PROJECTS for literal TERM."
  (spy-on 'read-string :and-call-fake (lambda (&rest _) term))
  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
    (projectile-search-in-projects projects t))
  (get-buffer projectile-search-buffer-name))


;;; The directory a group's file names are shown relative to

(describe "projectile--common-parent"
  (it "returns the project itself for a group of one"
    (expect (projectile--common-parent '("/src/app/"))
            :to-equal "/src/app/"))

  (it "returns the directory holding siblings"
    (expect (projectile--common-parent '("/src/app" "/src/app-docs"))
            :to-equal "/src/"))

  (it "returns the outer project when one contains the other"
    (expect (projectile--common-parent '("/src/app" "/src/app/sub"))
            :to-equal "/src/app/"))

  (it "walks all the way up when the projects share only the root"
    (expect (projectile--common-parent '("/src/app" "/opt/tool"))
            :to-equal "/"))

  (it "does not care which order the projects come in"
    (expect (projectile--common-parent '("/a/b/c" "/a/x")) :to-equal "/a/")
    (expect (projectile--common-parent '("/a/x" "/a/b/c")) :to-equal "/a/"))

  (it "has no answer for an empty group or for unrelated remote projects"
    (expect (projectile--common-parent nil) :to-be nil)
    (expect (projectile--common-parent '("/ssh:h1:/a" "/ssh:h2:/a")) :to-be nil)))


;;; Files across a group

(describe "projectile-project-group-files"
  (it "returns absolute files from every project in the group"
    (projectile-group-test--with-projects
      (let ((files (projectile-project-group-files (list alpha beta))))
        (expect (seq-filter #'file-name-absolute-p files) :to-equal files)
        (expect (sort (mapcar (lambda (f) (file-relative-name f parent)) files)
                      #'string<)
                :to-equal '("alpha/.projectile" "alpha/src/a.txt"
                            "beta/.projectile" "beta/lib/b.txt")))))

  (it "applies each member's own dirconfig ignores, wherever you are"
    ;; alpha's `-/skip.log' has to bite from outside alpha too.  It used
    ;; not to: `projectile-project-files' resolved dirconfig against the
    ;; current project rather than the one it was handed, so listing a
    ;; group filtered every member by the rules of whichever project you
    ;; happened to be visiting - and, with caching on, stored that wrong
    ;; answer under the member's own key.
    (projectile-group-test--with-projects
      (let ((skipped (expand-file-name "skip.log" alpha)))
        (expect (projectile-project-group-files (list alpha beta))
                :not :to-contain skipped)
        (let ((default-directory alpha))
          (spy-on 'projectile-project-root :and-return-value alpha)
          (expect (projectile-project-group-files (list alpha beta))
                  :not :to-contain skipped)))))

  (it "does not cache a member's files under the rules of another project"
    ;; The damaging half: the wrong list used to be stored under the other
    ;; project's key, so an ordinary find-file in it offered ignored files
    ;; afterwards - and with persistent caching that survived a restart.
    (projectile-group-test--with-projects
      (let ((projectile-enable-caching t))
        (spy-on 'projectile-project-root :and-return-value beta)
        (let ((default-directory beta))
          (projectile-project-group-files (list alpha beta)))
        (expect (gethash alpha projectile-projects-cache)
                :not :to-contain "skip.log"))))

  (it "skips a project that has been moved away"
    (projectile-group-test--with-projects
      (expect (projectile-project-group-files
               (list alpha (expand-file-name "gone/" parent)))
              :to-equal (projectile-project-group-files (list alpha)))))

  (it "lists a file once when two members of the group nest"
    ;; A monorepo and a project inside it can both be known projects.  A
    ;; file offered twice is merely untidy here, but the same duplication
    ;; in the search candidates makes the replace reviewer apply every
    ;; replacement twice and corrupt the file.
    (projectile-group-test--with-projects
      (let ((files (projectile-project-group-files (list parent alpha))))
        (expect files :to-equal (delete-dups (copy-sequence files)))))))

(describe "projectile-find-file-in-projects"
  (it "opens a file chosen from any project in the group"
    (projectile-group-test--with-projects
      (let ((wanted (expand-file-name "lib/b.txt" beta)))
        (spy-on 'projectile-completing-read :and-return-value wanted)
        (projectile-find-file-in-projects (list alpha beta))
        (expect (file-truename (buffer-file-name))
                :to-equal (file-truename wanted))))))


;;; Searching a group

(describe "projectile-search-in-projects"
  (it "gathers matches from every project into one buffer"
    (projectile-group-test--with-projects
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-group-test--search (list alpha beta) "needle")))
        (expect (projectile-test-match-files buf)
                :to-equal '("alpha/src/a.txt" "beta/lib/b.txt")))))

  (it "names the matches relative to the directory holding the group"
    (projectile-group-test--with-projects
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-group-test--search (list alpha beta) "needle")))
        (with-current-buffer buf
          (expect (file-truename projectile-replace--root)
                  :to-equal (file-truename parent))
          ;; so each match is labelled with the project it came from
          (expect (buffer-string) :to-match "alpha/src/a\\.txt")
          (expect (buffer-string) :to-match "beta/lib/b\\.txt")))))

  (it "records the group so a re-search covers it again"
    (projectile-group-test--with-projects
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-group-test--search (list alpha beta) "needle")))
        (with-current-buffer buf
          (expect (length projectile-replace--projects) :to-equal 2)
          (let ((before (length projectile-replace--matches)))
            (projectile-replace--regather)
            (expect (length projectile-replace--matches) :to-equal before)
            (expect (projectile-test-match-files buf)
                    :to-equal '("alpha/src/a.txt" "beta/lib/b.txt")))))))

  (it "scopes back to one project when the buffer is reused for a plain search"
    (projectile-group-test--with-projects
      (projectile-test-use-plain-grep)
      (projectile-group-test--search (list alpha beta) "needle")
      ;; an ordinary single-project search must not inherit the old group,
      ;; or its re-search would silently widen back to it
      (let ((default-directory alpha))
        (spy-on 'projectile-project-root :and-return-value alpha)
        (let ((buf (projectile-group-test--search (list alpha) "needle")))
          (with-current-buffer buf
            (expect (length projectile-replace--projects) :to-equal 1)
            (projectile-replace--regather)
            (expect (projectile-test-match-files buf)
                    :to-equal '("src/a.txt")))))))

  (it "scans a file once when two members of the group nest"
    ;; The reason the de-duplication matters: `r' from this buffer hands
    ;; the matches to the replace reviewer, which applies every match in a
    ;; file in one pass - so a doubled match replaces the span the first
    ;; copy just wrote.
    (projectile-group-test--with-projects
      (projectile-test-use-plain-grep)
      (let* ((buf (projectile-group-test--search (list parent alpha) "needle"))
             (files (with-current-buffer buf
                      (mapcar #'projectile-replace--match-file
                              projectile-replace--matches))))
        (expect files :to-equal (delete-dups (copy-sequence files))))))

  (it "survives a group member being moved away while the buffer is open"
    (projectile-group-test--with-projects
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-group-test--search (list alpha beta) "needle")))
        (delete-directory beta t)
        (with-current-buffer buf
          ;; `g' must re-scan what is left, not abort on the missing member
          (projectile-replace--regather)
          (expect (projectile-test-match-files buf)
                  :to-equal '("alpha/src/a.txt"))))))

  (it "refuses an empty group instead of searching the whole filesystem"
    (projectile-group-test--with-projects
      (expect (projectile-search-in-projects nil t) :to-throw 'user-error)
      (expect (projectile-search-in-projects
               (list (expand-file-name "gone/" parent)) t)
              :to-throw 'user-error))))


;;; The commands built on the two above

(describe "projectile-find-file-in-known-projects"
  (it "offers the files of every known project"
    (projectile-group-test--with-projects
      (let ((projectile-known-projects (list alpha beta)))
        (spy-on 'projectile-completing-read :and-return-value
                (expand-file-name "src/a.txt" alpha))
        (spy-on 'find-file)
        (projectile-find-file-in-known-projects)
        (expect (sort (mapcar (lambda (f) (file-relative-name f parent))
                              (cadr (spy-calls-args-for 'projectile-completing-read 0)))
                      #'string<)
                :to-equal '("alpha/.projectile" "alpha/src/a.txt"
                            "beta/.projectile" "beta/lib/b.txt"))))))

(describe "projectile-find-file-in-sibling-projects"
  (it "searches the current project and the ones related to it"
    (projectile-group-test--with-projects
      (spy-on 'projectile-sibling-projects :and-return-value (list alpha beta))
      (spy-on 'projectile-completing-read :and-return-value
              (expand-file-name "src/a.txt" alpha))
      (spy-on 'find-file)
      (projectile-find-file-in-sibling-projects)
      (expect (length (cadr (spy-calls-args-for 'projectile-completing-read 0)))
              :to-equal 4)))

  (it "says so when the project has no siblings"
    (projectile-group-test--with-projects
      (spy-on 'projectile-sibling-projects :and-return-value nil)
      (spy-on 'projectile-acquire-root :and-return-value alpha)
      (expect (projectile-find-file-in-sibling-projects) :to-throw 'user-error))))

(describe "projectile-search-in-sibling-projects"
  (it "searches the current project and the ones related to it"
    (projectile-group-test--with-projects
      (projectile-test-use-plain-grep)
      (spy-on 'projectile-sibling-projects :and-return-value (list alpha beta))
      (spy-on 'read-string :and-call-fake (lambda (&rest _) "needle"))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
        (projectile-search-in-sibling-projects))
      (expect (projectile-test-match-files
               (get-buffer projectile-search-buffer-name))
              :to-equal '("alpha/src/a.txt" "beta/lib/b.txt")))))


;;; Buffers across a group

(describe "projectile-project-group-buffers"
  (it "collects the buffers of every project in the group"
    (projectile-group-test--with-projects
      (let ((ba (find-file-noselect (expand-file-name "src/a.txt" alpha)))
            (bb (find-file-noselect (expand-file-name "lib/b.txt" beta))))
        (expect (projectile-project-group-buffers (list alpha beta))
                :to-contain ba)
        (expect (projectile-project-group-buffers (list alpha beta))
                :to-contain bb))))

  (it "offers a buffer once when two members of the group nest"
    (projectile-group-test--with-projects
      (find-file-noselect (expand-file-name "src/a.txt" alpha))
      (let ((buffers (projectile-project-group-buffers (list parent alpha))))
        (expect buffers :to-equal (delete-dups (copy-sequence buffers)))))))

(describe "projectile-switch-to-buffer-in-projects"
  (it "offers the group's buffers, minus the one you are in"
    (projectile-group-test--with-projects
      (let ((ba (find-file-noselect (expand-file-name "src/a.txt" alpha))))
        (find-file-noselect (expand-file-name "lib/b.txt" beta))
        (spy-on 'projectile-completing-read :and-return-value (buffer-name ba))
        (spy-on 'switch-to-buffer)
        (with-current-buffer ba
          (projectile-switch-to-buffer-in-projects (list alpha beta)))
        (let ((offered (cadr (spy-calls-args-for 'projectile-completing-read 0))))
          (expect offered :to-contain "b.txt")
          (expect offered :not :to-contain (buffer-name ba)))))))


;;; TODOs across a group

(describe "projectile-todos-in-sibling-projects"
  (it "collects annotations from every project in the group"
    (projectile-group-test--with-projects
      (with-temp-file (expand-file-name "src/todo.txt" alpha)
        (insert "TODO: alpha thing\n"))
      (with-temp-file (expand-file-name "lib/todo.txt" beta)
        (insert "FIXME: beta thing\n"))
      (spy-on 'projectile-sibling-projects :and-return-value (list alpha beta))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
        (projectile-todos-in-sibling-projects))
      (expect (projectile-test-match-files
               (get-buffer projectile-search-buffer-name))
              :to-equal '("alpha/src/todo.txt" "beta/lib/todo.txt"))))

  (it "still works on a single project, unchanged"
    (projectile-group-test--with-projects
      (with-temp-file (expand-file-name "src/todo.txt" alpha)
        (insert "TODO: alpha thing\n"))
      (spy-on 'projectile-acquire-root :and-return-value alpha)
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
        (projectile-todos))
      (expect (projectile-test-match-files
               (get-buffer projectile-search-buffer-name))
              :to-equal '("src/todo.txt")))))

(provide 'projectile-project-group-test)

;;; projectile-project-group-test.el ends here
