;;; projectile-test.el

;; Copyright © 2018 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>

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

;; This file is part of Projectile.

;;; Code:

(require 'projectile)
(require 'buttercup)

;;; Test Utilities
(defmacro projectile-test-with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  (declare (indent 0) (debug (&rest form)))
  `(let ((sandbox (expand-file-name
                   (convert-standard-filename "test/sandbox/")
                   (file-name-directory (locate-library "projectile.el" t)))))
     (when (file-directory-p sandbox)
       (delete-directory sandbox t))
     (make-directory sandbox t)
     (let ((default-directory sandbox))
       ,@body)))

(defmacro projectile-test-with-files (files &rest body)
  "Evaluate BODY in the presence of FILES.

You'd normally combine this with `projectile-test-with-sandbox'."
  (declare (indent 1) (debug (sexp &rest form)))
  `(progn
     ,@(mapcar (lambda (file)
                 (if (string-suffix-p "/" file)
                     `(make-directory ,file t)
                   `(with-temp-file ,file)))
               files)
     ,@body))

(defun projectile-test-tmp-file-path ()
  "Return a filename suitable to save data to in the
test temp directory"
  (concat projectile-test-path
          "/tmp/temporary-file-" (format "%d" (random))
          ".eld"))

;;; Tests
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
