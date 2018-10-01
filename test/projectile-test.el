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

;;; Tests
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
