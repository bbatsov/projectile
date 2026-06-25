;;; projectile-known-projects-test.el --- Tests for known projects bookkeeping -*- lexical-binding: t -*-

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

;; Tests for known projects bookkeeping.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile--cleanup-known-projects"
  (it "removes known projects that don't exist anymore"
    (let* ((projectile-known-projects-file (projectile-test-tmp-file-path))
           (directories (cl-loop repeat 3 collect (make-temp-file "projectile-cleanup" t)))
           (projectile-known-projects directories))
      (unwind-protect
          (progn
            (projectile--cleanup-known-projects)
            (expect projectile-known-projects :to-equal directories)
            (delete-directory (car directories))
            (projectile--cleanup-known-projects)
            (expect projectile-known-projects :to-equal (cdr directories)))
        (mapc (lambda (d) (ignore-errors (delete-directory d))) directories)
        (delete-file projectile-known-projects-file nil)))))

(describe "projectile-forget-zombie-projects"
  (it "is an alias for projectile-cleanup-known-projects"
    (expect (symbol-function 'projectile-forget-zombie-projects)
            :to-be 'projectile-cleanup-known-projects)))

(describe "projectile-forget-projects-under"
  (it "removes only immediate child projects by default"
    (let* ((projectile-known-projects-file (projectile-test-tmp-file-path))
           (projectile-known-projects-on-file nil)
           (parent (file-name-as-directory (make-temp-file "projectile-forget" t)))
           (child-a (file-name-as-directory (concat parent "a")))
           (child-b (file-name-as-directory (concat parent "b")))
           (nested (file-name-as-directory (concat child-a "nested")))
           (outside (file-name-as-directory (make-temp-file "projectile-forget-out" t)))
           (projectile-known-projects (list child-a child-b nested outside)))
      (unwind-protect
          (progn
            (mkdir child-a t) (mkdir child-b t) (mkdir nested t)
            (let ((removed (projectile-forget-projects-under parent)))
              (expect removed :to-equal 2)
              (expect projectile-known-projects :to-equal (list nested outside))))
        (ignore-errors (delete-directory parent t))
        (ignore-errors (delete-directory outside t))
        (delete-file projectile-known-projects-file nil))))

  (it "removes nested projects too when recursive"
    (let* ((projectile-known-projects-file (projectile-test-tmp-file-path))
           (projectile-known-projects-on-file nil)
           (parent (file-name-as-directory (make-temp-file "projectile-forget" t)))
           (child-a (file-name-as-directory (concat parent "a")))
           (nested (file-name-as-directory (concat child-a "nested")))
           (outside (file-name-as-directory (make-temp-file "projectile-forget-out" t)))
           (projectile-known-projects (list child-a nested outside)))
      (unwind-protect
          (progn
            (mkdir child-a t) (mkdir nested t)
            (let ((removed (projectile-forget-projects-under parent t)))
              (expect removed :to-equal 2)
              (expect projectile-known-projects :to-equal (list outside))))
        (ignore-errors (delete-directory parent t))
        (ignore-errors (delete-directory outside t))
        (delete-file projectile-known-projects-file nil))))

  (it "removes projects even when the directory has been deleted"
    (let* ((projectile-known-projects-file (projectile-test-tmp-file-path))
           (projectile-known-projects-on-file nil)
           (parent (file-name-as-directory (make-temp-file "projectile-forget" t)))
           (child-a (file-name-as-directory (concat parent "a")))
           (nested (file-name-as-directory (concat child-a "nested")))
           (outside (file-name-as-directory (make-temp-file "projectile-forget-out" t)))
           (projectile-known-projects (list child-a nested outside)))
      (unwind-protect
          (progn
            (mkdir child-a t) (mkdir nested t)
            (delete-directory parent t)
            (let ((removed (projectile-forget-projects-under parent t)))
              (expect removed :to-equal 2)
              (expect projectile-known-projects :to-equal (list outside))))
        (ignore-errors (delete-directory parent t))
        (ignore-errors (delete-directory outside t))
        (delete-file projectile-known-projects-file nil))))

  (it "returns 0 and keeps the list when nothing matches"
    (let* ((projectile-known-projects-file (projectile-test-tmp-file-path))
           (projectile-known-projects-on-file nil)
           (parent (file-name-as-directory (make-temp-file "projectile-forget" t)))
           (outside (file-name-as-directory (make-temp-file "projectile-forget-out" t)))
           (projectile-known-projects (list outside)))
      (unwind-protect
          (expect (projectile-forget-projects-under parent) :to-equal 0)
        (expect projectile-known-projects :to-equal (list outside))
        (ignore-errors (delete-directory parent t))
        (ignore-errors (delete-directory outside t))
        (delete-file projectile-known-projects-file nil)))))

(describe "projectile-add-known-project"
  :var (projectile-known-projects-file)
  (before-each
   (setq projectile-known-projects-file (projectile-test-tmp-file-path)))

  (after-each
   (delete-file projectile-known-projects-file nil))

  (it "an added project should be added to the list of known projects"
    (let ((projectile-known-projects nil))
      (projectile-add-known-project "~/my/new/project/")
      (expect projectile-known-projects :to-equal '("~/my/new/project/"))))
  (it "adding a project should move it to the front of the list of known projects, if it already existed."
    (let ((projectile-known-projects '("~/b/" "~/a/")))
      (projectile-add-known-project "~/a/")
      (expect projectile-known-projects :to-equal '("~/a/" "~/b/"))))
  (it "~/project and ~/project/ should not be added separately to the known projects list"
    (let ((projectile-known-projects '("~/project/")))
      (projectile-add-known-project "~/project")
      (expect projectile-known-projects :to-equal '("~/project/"))
      (projectile-add-known-project "~/b")
      (projectile-add-known-project "~/b/")
      (expect projectile-known-projects :to-equal '("~/b/" "~/project/")))))

(describe "projectile-load-known-projects"
  (it "loads known projects through serialization functions"
    (let ((projectile-known-projects-file (projectile-test-tmp-file-path)))
      (spy-on 'projectile-unserialize :and-return-value '("a1" "a2"))
      (projectile-load-known-projects)
      (expect 'projectile-unserialize :to-have-been-called-with projectile-known-projects-file)
      (expect projectile-known-projects :to-equal '("a1" "a2")))))

(describe "projectile-merge-known-projects"
  (it "merges known projects"
    (let ((projectile-known-projects nil)
          (projectile-known-projects-file (projectile-test-tmp-file-path)))
      ;; initialize saved known projects and load it from disk
      (projectile-serialize '("a1" "a2" "a3" "a4" "a5")
                            projectile-known-projects-file)
      (projectile-load-known-projects)
      ;; simulate other emacs session changes by: remove a2 a5 and adding b1 b2
      (projectile-serialize '("a3" "b1" "a1" "a4" "b2")
                            projectile-known-projects-file)
      ;; remove a4 and add a6 and merge with disk
      (setq projectile-known-projects '("a6" "a1" "a2" "a3" "a5"))
      (projectile-merge-known-projects)
      (delete-file projectile-known-projects-file nil)
      (expect projectile-known-projects :to-equal '("a6" "a1" "a3" "b1" "b2"))))
  (it "merges known projects to an empty file"
    (let ((projectile-known-projects nil)
          (projectile-known-projects-file (projectile-test-tmp-file-path)))
      ;; initialize saved known projects and load it from disk
      (projectile-serialize '("a1" "a2" "a3" "a4" "a5")
                            projectile-known-projects-file)
      (projectile-load-known-projects)
      ;; empty the on disk known projects list
      (projectile-serialize '() projectile-known-projects-file)
      ;; merge
      (projectile-merge-known-projects)
      (delete-file projectile-known-projects-file nil)
      (expect projectile-known-projects :to-equal '())))
  (it "merges known projects from an empty file"
    (let ((projectile-known-projects nil)
          (projectile-known-projects-file (projectile-test-tmp-file-path)))
      ;; initialize saved known projects and load it from disk
      (projectile-serialize '() projectile-known-projects-file)
      (projectile-load-known-projects)
      ;; empty the on disk known projects list
      (projectile-serialize '("a" "b" "c" "d") projectile-known-projects-file)
      ;; merge
      (projectile-merge-known-projects)
      (delete-file projectile-known-projects-file nil)
      (expect projectile-known-projects :to-equal '("a" "b" "c" "d"))))
  (it "merges known projects while keeping their order"
    (let ((projectile-known-projects nil)
          (projectile-known-projects-file (projectile-test-tmp-file-path)))
      ;; initialize saved known projects and load it from disk
      (projectile-serialize '("a" "b" "c" "d") projectile-known-projects-file)
      (projectile-load-known-projects)
      ;; save the same list in different order
      (projectile-serialize '("d" "c" "b" "a") projectile-known-projects-file)
      ;; merge
      (projectile-merge-known-projects)
      (delete-file projectile-known-projects-file nil)
      (expect projectile-known-projects :to-equal '("a" "b" "c" "d")))))

(describe "projectile-save-known-projects"
  (it "saves known projects through serialization functions"
    (let ((projectile-known-projects-file (projectile-test-tmp-file-path))
          (projectile-known-projects '(floop)))
      (spy-on 'projectile-serialize)
      (projectile-save-known-projects)
      (expect 'projectile-serialize :to-have-been-called-with '(floop) projectile-known-projects-file))))

(describe "projectile-serialization-functions"
  (it "tests that serialization functions can save/restore data to the filesystem"
    (let ((this-test-file (projectile-test-tmp-file-path)))
      (unwind-protect
        (progn
          (projectile-serialize '(some random data) this-test-file)
          (expect (projectile-unserialize this-test-file) :to-equal '(some random data))
          (when (file-exists-p this-test-file)
            (delete-file this-test-file)))))))

(describe "projectile-clear-known-projects"
  (it "clears known projects"
    (let ((projectile-known-projects '("one" "two" "three"))
          (projectile-known-projects-file (projectile-test-tmp-file-path)))
      (projectile-clear-known-projects)
      (expect projectile-known-projects :to-equal nil))))

(describe "projectile-reset-known-projects"
  (it "resets known projects"
    (spy-on 'projectile-clear-known-projects)
    (spy-on 'projectile-discover-projects-in-search-path)
    (projectile-reset-known-projects)
    (expect 'projectile-clear-known-projects :to-have-been-called)
    (expect 'projectile-discover-projects-in-search-path :to-have-been-called)))

(describe "projectile-relevant-known-projects"
  :var 'known-projects
  (before-all
    (setq known-projects '("~/foo/" "~/bar/" "~/baz/")))

  (it "excludes projects matched by projectile-ignored-projects (#1663)"
    (let ((projectile-known-projects '("/projects/a/" "/projects/b/"))
          (projectile-ignored-projects '("/projects/b/"))
          (projectile-current-project-on-switch 'keep))
      (expect (projectile-relevant-known-projects) :to-equal '("/projects/a/"))))

  (it "does not filter (or call the ignore check) without any ignore config (#1663)"
    (let ((projectile-known-projects '("/projects/a/" "/projects/b/"))
          (projectile-ignored-projects nil)
          (projectile-ignored-project-function nil)
          (projectile-current-project-on-switch 'keep))
      (spy-on 'projectile-ignored-project-p)
      (expect (projectile-relevant-known-projects) :to-equal '("/projects/a/" "/projects/b/"))
      (expect 'projectile-ignored-project-p :not :to-have-been-called)))

  (describe "when projectile-current-project-on-switch is 'remove"
    (it "removes the current project"
      (spy-on 'projectile-project-root :and-return-value "~/foo/")
      (let ((projectile-current-project-on-switch 'remove)
            (projectile-known-projects known-projects))
        (expect (projectile-relevant-known-projects) :to-equal '("~/bar/" "~/baz/")))))

  (describe "when projectile-current-project-on-switch is 'move-to-end"
    (it "moves the current project to the end of projectile-known-projects"
      (spy-on 'projectile-project-root :and-return-value "~/foo/")
      (let ((projectile-current-project-on-switch 'move-to-end)
            (projectile-known-projects known-projects))
        (expect (projectile-relevant-known-projects) :to-equal '("~/bar/" "~/baz/" "~/foo/")))))

  (describe "when projectile-current-project-on-switch is 'keep"
    (it "returns projectile-known-projects"
      (spy-on 'projectile-project-root :and-return-value "~/foo/")
      (let ((projectile-current-project-on-switch 'keep)
            (projectile-known-projects known-projects))
        (expect (projectile-relevant-known-projects) :to-equal '("~/foo/" "~/bar/" "~/baz/"))))))

(describe "projectile-relevant-open-projects"
  (describe "when projectile-current-project-on-switch is 'remove"
    (it "removes the current project"
      (spy-on 'projectile-open-projects :and-return-value '("~/foo/" "~/bar/" "~/baz/"))
      (spy-on 'projectile-project-root :and-return-value "~/foo/")
      (let ((projectile-current-project-on-switch 'remove))
        (expect (projectile-relevant-open-projects) :to-equal '("~/bar/" "~/baz/")))))

  (describe "when projectile-current-project-on-switch is 'move-to-end"
    (it "moves the current project to the end of projectile-known-projects"
      (spy-on 'projectile-open-projects :and-return-value '("~/foo/" "~/bar/" "~/baz/"))
      (spy-on 'projectile-project-root :and-return-value "~/foo/")
      (let ((projectile-current-project-on-switch 'move-to-end))
        (expect (projectile-relevant-open-projects) :to-equal '("~/bar/" "~/baz/" "~/foo/")))))

  (describe "when projectile-current-project-on-switch is 'keep"
    (it "returns projectile-open-projects"
      (spy-on 'projectile-project-root :and-return-value "~/foo/")
      (spy-on 'projectile-open-projects :and-return-value '("~/foo/" "~/bar/" "~/baz/"))
      (let ((projectile-current-project-on-switch 'keep))
        (expect (projectile-relevant-open-projects) :to-equal '("~/foo/" "~/bar/" "~/baz/"))))))


;;; Mode line tests
(describe "projectile-load-project-cache"
  (it "does not store nil in cache for corrupt files"
    (let ((projectile-projects-cache (make-hash-table :test 'equal)))
      (spy-on 'projectile-project-cache-file :and-return-value "/tmp/fake-cache.eld")
      (spy-on 'file-exists-p :and-return-value t)
      (spy-on 'projectile-unserialize :and-return-value nil)
      (projectile-load-project-cache "/project/")
      (expect (gethash "/project/" projectile-projects-cache) :to-be nil)))
  (it "records a cache time so TTL checks don't immediately evict the loaded data"
    ;; Without this, `projectile-files-cache-expire' combined with persistent
    ;; caching would re-read the cache file from disk on every call (and never
    ;; reindex), because the TTL check evicts entries that have no recorded time.
    (projectile-test-with-sandbox
      (projectile-test-with-files
       ("project/")
       (let ((cache-file (expand-file-name "project/.projectile-cache.eld"))
             (projectile-projects-cache (make-hash-table :test 'equal))
             (projectile-projects-cache-time (make-hash-table :test 'equal)))
         (with-temp-file cache-file
           (insert (prin1-to-string '("file1.el" "file2.el"))))
         (spy-on 'projectile-project-cache-file :and-return-value cache-file)
         (projectile-load-project-cache "/project/")
         (expect (gethash "/project/" projectile-projects-cache)
                 :to-equal '("file1.el" "file2.el"))
         (expect (gethash "/project/" projectile-projects-cache-time)
                 :to-be-truthy)
         (expect (gethash "/project/" projectile-projects-cache-time)
                 :to-equal
                 (time-convert
                  (file-attribute-modification-time
                   (file-attributes cache-file))
                  'integer)))))))

;;; projectile-known-projects-test.el ends here
