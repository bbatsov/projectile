;;; projectile-core-test.el --- Tests for core utilities (dispatch, naming, paths, mode) -*- lexical-binding: t -*-

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

;; Tests for core utilities (dispatch, naming, paths, mode).

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-dispatch"
  (it "is defined as a command when transient is available"
    (assume (require 'transient nil t) "transient is not available")
    (expect (commandp 'projectile-dispatch) :to-be-truthy)))

(describe "projectile-project-name"
  (it "return projectile-project-name when present"
    (let ((projectile-project-name "name"))
      (expect (projectile-project-name) :to-equal "name")
      (expect (projectile-project-name "other") :to-equal "name")))
  (it "uses projectile-project-name-function to get the project name from the project dir"
    (let ((projectile-project-name-function (lambda (dir) dir)))
      (expect (projectile-project-name "some/dir") :to-equal "some/dir")))
  (it "acts on the current project is not passed a project dir explicitly"
    (spy-on 'projectile-project-root :and-return-value "current/project")
    (let ((projectile-project-name-function (lambda (dir) dir)))
      (expect (projectile-project-name) :to-equal "current/project"))))

(describe "projectile-uniquify-dirname-transform"
  (it "splices the project name into a directory inside a project"
    (spy-on 'projectile-project-root :and-return-value "/home/me/work/checkout-42/")
    (spy-on 'projectile-project-name :and-return-value "myproject")
    (expect (projectile-uniquify-dirname-transform "/home/me/work/checkout-42/src/")
            :to-equal "/home/me/work/checkout-42/myproject/src/"))
  (it "returns the directory unchanged outside a project"
    (spy-on 'projectile-project-root :and-return-value nil)
    (expect (projectile-uniquify-dirname-transform "/tmp/not-a-project/")
            :to-equal "/tmp/not-a-project/")))

(describe "projectile-prepend-project-name"
  (it "prepends the project name to its parameter"
    (spy-on 'projectile-project-name :and-return-value "project")
    (expect (projectile-prepend-project-name "Test") :to-equal "[project] Test")))

(describe "projectile-expand-root"
  (it "expands a relative path into an absolute path within a project"
    (spy-on  'projectile-project-root :and-return-value "/path/to/project")
    (expect (projectile-expand-root "foo") :to-equal "/path/to/project/foo")
    (expect (projectile-expand-root "foo/bar") :to-equal "/path/to/project/foo/bar")
    (expect (projectile-expand-root "./foo/bar") :to-equal "/path/to/project/foo/bar")))

(describe "projectile-expand-file-name-wildcard"
  (it "expands a filename not containing wildcards"
    (expect (projectile-expand-file-name-wildcard "test" "/path/to/project/")
            :to-equal "/path/to/project/test"))
  (it "does not try to resolve wildcards if there are none in the pattern"
    (spy-on 'file-expand-wildcards)
    (expect (projectile-expand-file-name-wildcard "foo" "/path/to/project/")
            :to-equal "/path/to/project/foo")
    (expect (spy-calls-any 'file-expand-wildcards) :to-equal nil))
  (it "returns the first wildcard result if any exist"
    (spy-on 'file-expand-wildcards
            :and-return-value '("/path/to/project/one"
                                "/path/to/project/two"))
    (expect (projectile-expand-file-name-wildcard "*" "/path/to/project")
            :to-equal "/path/to/project/one"))
  (it "returns the expanded result if the are no wildcard results"
    (expect (projectile-expand-file-name-wildcard "*" "/path/to/project-b")
            :to-equal "/path/to/project-b/*")))

(describe "projectile-determine-find-tag-fn"
  (it "falls back to xref-find-definitions when ggtags backend is unavailable"
    (let ((projectile-tags-backend 'ggtags))
      (expect (projectile-determine-find-tag-fn) :to-equal 'xref-find-definitions)))
  (it "falls back to xref-find-definitions when etags-select backend is unavailable"
    (let ((projectile-tags-backend 'etags-select))
      (expect (projectile-determine-find-tag-fn) :to-equal 'xref-find-definitions)))
  (it "returns xref-find-definitions for auto backend without ggtags"
    (let ((projectile-tags-backend 'auto))
      (expect (projectile-determine-find-tag-fn) :to-equal 'xref-find-definitions))))

(describe "projectile-mode"
  (before-each
    (spy-on 'projectile--cleanup-known-projects)
    (spy-on 'projectile-discover-projects-in-search-path))
  (it "sets up hook functions"
    (projectile-mode 1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :to-be-truthy)
    (projectile-mode -1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :not :to-be-truthy)))

(describe "projectile-tags-exclude-patterns"
  (it "returns a string with exclude patterns for ctags"
    (spy-on 'projectile-ignored-directories-rel :and-return-value (list ".git/" ".hg/"))
    (expect (projectile-tags-exclude-patterns) :to-equal "--exclude=\".git\" --exclude=\".hg\"")))

(describe "projectile-default-mode-line"
  (it "includes the project name and type when in a project"
    (spy-on 'projectile-project-name :and-return-value "foo")
    (spy-on 'projectile-project-type :and-return-value "bar")
    (expect (projectile-default-mode-line) :to-equal " Projectile[foo:bar]"))
  (it "returns also a - if called outside a project"
    (spy-on 'projectile-project-name :and-return-value nil)
    (spy-on 'projectile-project-type :and-return-value nil)
    (expect (projectile-default-mode-line) :to-equal " Projectile[-]"))
  (it "respects the value of projectile-mode-line-prefix"
    (spy-on 'projectile-project-name :and-return-value "foo")
    (spy-on 'projectile-project-type :and-return-value "bar")
    (let ((projectile-mode-line-prefix " Pro"))
      (expect (projectile-default-mode-line) :to-equal " Pro[foo:bar]"))))

(describe "projectile-purge-file-from-cache"
  (it "serializes the updated cache without the purged file"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching 'persistent))
      (puthash "/project/" '("foo.el" "bar.el" "baz.el") projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile-serialize)
      (spy-on 'projectile-project-cache-file :and-return-value "/tmp/cache.eld")
      (projectile-purge-file-from-cache "bar.el")
      ;; The in-memory cache should be updated
      (expect (gethash "/project/" projectile-projects-cache) :to-equal '("foo.el" "baz.el"))
      ;; projectile-serialize should be called with the updated list, not the stale one
      (expect 'projectile-serialize :to-have-been-called-with '("foo.el" "baz.el") "/tmp/cache.eld"))))

(describe "projectile-purge-dir-from-cache"
  (it "removes files under the directory from the in-memory cache"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching t))
      (puthash "/project/"
               '("src/foo.el" "src/sub/bar.el" "test/baz.el")
               projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (projectile-purge-dir-from-cache "src/")
      (expect (gethash "/project/" projectile-projects-cache)
              :to-equal '("test/baz.el"))))
  (it "serializes the updated cache when persistent caching is enabled"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching 'persistent))
      (puthash "/project/"
               '("src/foo.el" "src/sub/bar.el" "test/baz.el")
               projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile-serialize)
      (spy-on 'projectile-project-cache-file :and-return-value "/tmp/cache.eld")
      (projectile-purge-dir-from-cache "src/")
      (expect 'projectile-serialize
              :to-have-been-called-with '("test/baz.el") "/tmp/cache.eld")))
  (it "does not touch disk when persistent caching is disabled"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching t))
      (puthash "/project/"
               '("src/foo.el" "test/baz.el")
               projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile-serialize)
      (projectile-purge-dir-from-cache "src/")
      (expect 'projectile-serialize :not :to-have-been-called))))

(describe "projectile-sort-by-modification-time"
  (it "sorts files by modification time in descending order"
    (projectile-test-with-sandbox
      (projectile-test-with-files
        ("old.el" "new.el")
        ;; Touch old.el first, then new.el with a delay to ensure different mtimes
        (spy-on 'projectile-project-root :and-return-value default-directory)
        (let ((now (current-time))
              (old-time (time-subtract (current-time) 100)))
          (spy-on 'file-attributes :and-call-fake
                  (lambda (file &rest _)
                    (let ((attrs (make-list 12 nil)))
                      ;; Set modification time (index 5)
                      (setf (nth 5 attrs) (if (string-match-p "old" file) old-time now))
                      attrs)))
          (expect (projectile-sort-by-modification-time '("old.el" "new.el"))
                  :to-equal '("new.el" "old.el"))))))
  (it "handles deleted files (nil file-attributes) without error"
    (spy-on 'projectile-project-root :and-return-value "/project/")
    (spy-on 'file-attributes :and-return-value nil)
    (expect (projectile-sort-by-modification-time '("gone.el" "also-gone.el"))
            :not :to-throw)))

(describe "projectile-parent"
  (it "returns the parent directory of a file path"
    (expect (projectile-parent "/a/b/c.el") :to-equal "/a/b"))
  (it "returns the parent directory of a directory path with a trailing slash"
    (expect (projectile-parent "/a/b/") :to-equal "/a"))
  (it "treats a slash-less directory the same as one with a trailing slash"
    (expect (projectile-parent "/a/b") :to-equal "/a")))

(describe "projectile--directory-ancestors"
  (it "returns the file's directory and every ancestor, outermost first"
    (expect (projectile--directory-ancestors "src/foo/bar.el")
            :to-equal '("src/" "src/foo/")))
  (it "walks an absolute path up to the root"
    (expect (projectile--directory-ancestors "/a/b/c")
            :to-equal '("/" "/a/" "/a/b/")))
  (it "returns nil for a bare file name with no directory part"
    (expect (projectile--directory-ancestors "foo.el") :to-equal nil)))

(describe "projectile-default-project-name"
  (it "uses the last path segment as the project name"
    (expect (projectile-default-project-name "/home/me/projects/projectile/")
            :to-equal "projectile"))
  (it "does not care whether the root has a trailing slash"
    (expect (projectile-default-project-name "/home/me/projects/projectile")
            :to-equal "projectile")))

(describe "projectile-make-relative-to-root"
  (it "rewrites absolute project files as paths relative to the root"
    (spy-on 'projectile-project-root :and-return-value "/home/me/project/")
    (expect (projectile-make-relative-to-root
             '("/home/me/project/src/foo.el" "/home/me/project/README.md"))
            :to-equal '("src/foo.el" "README.md"))))

;;; projectile-core-test.el ends here
