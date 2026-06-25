;;; projectile-project-type-test.el --- Tests for project type registration and detection -*- lexical-binding: t -*-

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

;; Tests for project type registration and detection.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile--combine-plists"
 (it "Items in second plist override elements in first"
   (expect (projectile--combine-plists
            '(:foo "foo" :bar "bar")
            '(:foo "foo" :bar "foo" :foobar "foobar"))
           :to-equal
           '(:foo "foo" :bar "foo" :foobar "foobar")))
 (it "Nil elements in second plist override elements in first"
   (expect (projectile--combine-plists
            '(:foo "foo" :bar "bar")
            '(:foo "foo" :bar nil :foobar "foobar"))
           :to-equal
           '(:foo "foo" :bar nil :foobar "foobar"))))

(describe "projectile-register-project-type"
  (it "prepends new projects to projectile-project-types"
    (projectile-register-project-type 'foo '("Foo"))
    (expect (caar projectile-project-types) :to-equal 'foo)
    (projectile-register-project-type 'bar '("Bar"))
    (expect (caar projectile-project-types) :to-equal 'bar))

  (it "derives project-file from the first marker file and seeds the top-down list"
    (let ((projectile-project-types nil)
          (projectile-project-root-files nil)
          (projectile-project-root-files-bottom-up '(".git")))
      (projectile-register-project-type 'foo '("foo.manifest" "extra-dir"))
      (expect (projectile-project-type-attribute 'foo 'project-file)
              :to-equal "foo.manifest")
      ;; only the first marker is treated as the project file
      (expect (member "foo.manifest" projectile-project-root-files) :to-be-truthy)
      (expect (member "extra-dir" projectile-project-root-files) :not :to-be-truthy)
      ;; manifests never leak into the bottom-up list (VCS markers win)
      (expect (member "foo.manifest" projectile-project-root-files-bottom-up)
              :not :to-be-truthy)))

  (it "honors an explicit project-file over the first marker file"
    (let ((projectile-project-types nil)
          (projectile-project-root-files nil))
      (projectile-register-project-type 'foo '("foo.manifest") :project-file "real.manifest")
      (expect (projectile-project-type-attribute 'foo 'project-file)
              :to-equal "real.manifest")
      (expect (member "real.manifest" projectile-project-root-files) :to-be-truthy)
      (expect (member "foo.manifest" projectile-project-root-files) :not :to-be-truthy)))

  (it "opts out of root-file seeding when project-file is `none'"
    ;; Regression for #1901: bloop's marker also lives in $HOME, so it
    ;; must drive detection without ever anchoring a project root.
    (let ((projectile-project-types nil)
          (projectile-project-root-files nil)
          (projectile-project-root-files-bottom-up '(".git")))
      (projectile-register-project-type 'foo '(".foo/settings.json") :project-file 'none)
      (expect (projectile-project-type-attribute 'foo 'project-file) :to-equal nil)
      (expect (member ".foo/settings.json" projectile-project-root-files)
              :not :to-be-truthy)
      (expect (member ".foo/settings.json" projectile-project-root-files-bottom-up)
              :not :to-be-truthy))))

(describe "projectile-update-project-type"
  :var ((mock-projectile-project-types
         '((foo marker-files ("marker-file")
                project-file "project-file"
                compilation-dir "compilation-dir"
                configure-command "configure"
                compile-command "compile"
                test-command "test"
                install-command "install"
                package-command "package"
                run-command "run"))))
  (it "Updates existing project type in projectile-project-types"
    (let ((projectile-project-types mock-projectile-project-types))
      (projectile-update-project-type
       'foo
       :marker-files '("marker-file2")
       :test-suffix "suffix")
      (expect projectile-project-types :to-equal
              '((foo marker-files ("marker-file2")
                     project-file "project-file"
                     compilation-dir "compilation-dir"
                     configure-command "configure"
                     compile-command "compile"
                     test-command "test"
                     install-command "install"
                     package-command "package"
                     run-command "run"
                     test-suffix "suffix")))))
  (it "Updates existing project type with nil value"
    (let ((projectile-project-types mock-projectile-project-types))
      (projectile-update-project-type
       'foo
       :marker-files '("marker-file2")
       :test-suffix nil)
      (expect projectile-project-types :to-equal
              '((foo marker-files ("marker-file2")
                     project-file "project-file"
                     compilation-dir "compilation-dir"
                     configure-command "configure"
                     compile-command "compile"
                     test-command "test"
                     install-command "install"
                     package-command "package"
                     run-command "run"
                     test-suffix nil)))))
  (it "Updates existing project type using all options"
    (let ((projectile-project-types mock-projectile-project-types)
          (dummy-val "foo"))
      (projectile-update-project-type
       'foo
       :marker-files (list dummy-val)
       :project-file dummy-val
       :compilation-dir dummy-val
       :configure dummy-val
       :compile dummy-val
       :test dummy-val
       :install dummy-val
       :package dummy-val
       :run dummy-val
       :test-suffix dummy-val
       :test-prefix dummy-val
       :src-dir dummy-val
       :test-dir dummy-val
       :related-files-fn dummy-val)
      (expect projectile-project-types :to-equal
              `((foo marker-files (,dummy-val)
                     project-file ,dummy-val
                     compilation-dir ,dummy-val
                     configure-command ,dummy-val
                     compile-command ,dummy-val
                     test-command ,dummy-val
                     install-command ,dummy-val
                     package-command ,dummy-val
                     run-command ,dummy-val
                     test-suffix ,dummy-val
                     test-prefix ,dummy-val
                     src-dir ,dummy-val
                     test-dir ,dummy-val
                     related-files-fn ,dummy-val)))))
  (it "Error when attempt to update nonexistent project type"
    (let ((projectile-project-types mock-projectile-project-types))
      (expect (projectile-update-project-type
               'bar
               :marker-files '("marker-file")
               :test-suffix "suffix")
              :to-throw)))
  (it "changes project type precedence"
    (let ((projectile-project-types
           '((foo marker-files ("foo"))
             (bar marker-files ("foo")))))
      (projectile-test-with-sandbox
        (projectile-test-with-files
            ("projectA/" "projectA/foo")
          (spy-on 'projectile-project-root
            :and-return-value
            (file-truename (expand-file-name "projectA")))
          (expect (projectile-project-type) :to-equal 'foo)
          (projectile-update-project-type 'bar :precedence 'high)
          (expect (projectile-project-type) :to-equal 'bar)
          (projectile-update-project-type 'bar :precedence 'low)
          (expect (projectile-project-type) :to-equal 'foo)))))
  (it "errors if :precedence not valid"
    (let ((projectile-project-types '((bar marker-files ("foo")))))
      (expect
       (projectile-update-project-type 'bar :precedence 'invalid-symbol)
       :to-throw)))
  (it "resets project type cache with correct :test"
    (let ((projectile-project-types '((foo marker-files ("foo"))))
          (projectile-project-type-cache (make-hash-table :test 'equal)))
      (puthash "/path/to/project" 'foo projectile-project-type-cache)
      (projectile-update-project-type 'foo :compile "make")
      ;; Cache should have been reset but still use 'equal test
      (puthash "/path/to/project" 'foo projectile-project-type-cache)
      (expect (gethash "/path/to/project" projectile-project-type-cache) :to-equal 'foo))))

(describe "projectile-remove-project-type"
  (it "removes a registered project type"
    (let ((projectile-project-types '((foo marker-files ("foo"))
                                      (bar marker-files ("bar")))))
      (projectile-remove-project-type 'foo)
      (expect projectile-project-types :to-equal '((bar marker-files ("bar"))))))
  (it "resets the project type cache"
    (let ((projectile-project-types '((foo marker-files ("foo"))))
          (projectile-project-type-cache (make-hash-table :test 'equal)))
      (puthash "/path/to/project" 'foo projectile-project-type-cache)
      (projectile-remove-project-type 'foo)
      (expect (gethash "/path/to/project" projectile-project-type-cache) :to-equal nil)))
  (it "errors when the project type is not registered"
    (let ((projectile-project-types '((foo marker-files ("foo")))))
      (expect (projectile-remove-project-type 'bar) :to-throw))))

(describe "emacs-eask project type"
  (it "uses `eask test' as its test command (#1935)"
    (expect (projectile-default-test-command 'emacs-eask) :to-equal "eask test")))

(describe "projectile-project-type"
  :var ((dir default-directory))
  (it "detects the type of Projectile's project"
    (expect (projectile-project-type) :to-equal 'emacs-eldev))
  (it "caches the project type"
    (expect (gethash (projectile-project-root) projectile-project-type-cache) :to-equal 'emacs-eldev))
  (it "detects the type of Projectile's project when it is passed as args"
    (projectile-test-with-sandbox
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-project-type dir) :to-equal 'emacs-eldev))))
  (describe "override by projectile-project-type"
    (it "is respected when no DIR is passed"
      (let ((projectile-project-type 'python-poetry))
        (expect projectile-project-type :to-equal 'python-poetry)))
    (it "has no effect when DIR is passed"
      (projectile-test-with-sandbox
        (let ((projectile-project-type 'python-poetry))
          (expect (projectile-project-type dir) :to-equal 'emacs-eldev)))))
  (it "passes project-root to detect-project-type to avoid redundant resolution"
    (projectile-test-with-sandbox
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-detect-project-type :and-call-through)
        (projectile-project-type dir)
        (expect 'projectile-detect-project-type
                :to-have-been-called-with dir (projectile-project-root dir))))))

(describe "projectile-detect-project-type"
  (it "detects project-type for rails-like npm tests"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Gemfile"
       "project/app/"
       "project/lib/"
       "project/db/"
       "project/config/"
       "project/spec/"
       "project/package.json")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'rails-rspec)))))
  (it "detects project-type for elisp eldev projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Eldev"
       "project/project.el")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'emacs-eldev)))))
  (it "detects project-type for dotnet sln projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Project.sln")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'dotnet-sln)))))
  (it "detects project-type for dotnet slnx projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Project.slnx")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'dotnet-sln)))))
  (it "detects project-type for Julia PkgTemplates.jl projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/src/"
       "project/Project.toml")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'julia)))))
  (it "detects project-type for Zig projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/src/"
       "project/build.zig.zon")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'zig)))))
  (it "does not match a project type whose marker-files are empty"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/foo")
      (let ((projectile-project-types '((empty marker-files nil)
                                        (real marker-files ("foo"))))
            (projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'real)))))
  (it "falls back to generic when the only type has empty marker-files"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/foo")
      (let ((projectile-project-types '((empty marker-files nil)))
            (projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'generic)))))
  (it "passes the project root to a function marker (#1909)"
    (let ((projectile-project-types
           (list (list 'custom 'marker-files
                       (lambda (root) (and root (string-match-p "subproject/src/foo/?\\'" root))))))
          (projectile-project-type-cache (make-hash-table :test 'equal)))
      (spy-on 'projectile-project-root :and-return-value "/repo/subproject/src/foo/")
      (expect (projectile-detect-project-type) :to-equal 'custom)))
  (it "does not match a function marker when the root doesn't satisfy it (#1909)"
    (let ((projectile-project-types
           (list (list 'custom 'marker-files
                       (lambda (root) (and root (string-match-p "subproject/src/foo/?\\'" root))))))
          (projectile-project-type-cache (make-hash-table :test 'equal)))
      (spy-on 'projectile-project-root :and-return-value "/repo/elsewhere/")
      (expect (projectile-detect-project-type) :to-equal 'generic))))

;;; projectile-project-type-test.el ends here
