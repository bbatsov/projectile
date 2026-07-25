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

  (it "seeds every alternative of a leading (:any ...) marker as a root file"
    (let ((projectile-project-types nil)
          (projectile-project-root-files nil)
          (projectile-project-root-files-bottom-up '(".git")))
      (projectile-register-project-type 'foo '((:any "foo.toml" "foo.json") "src"))
      (expect (projectile-project-type-attribute 'foo 'project-file)
              :to-equal '("foo.toml" "foo.json"))
      (expect (member "foo.toml" projectile-project-root-files) :to-be-truthy)
      (expect (member "foo.json" projectile-project-root-files) :to-be-truthy)
      (expect (member "src" projectile-project-root-files) :not :to-be-truthy)))

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

(describe "rails project types"
  (it "runs the server as the run command, not as the compile command"
    (dolist (type '(rails-test rails-rspec))
      (expect (projectile-default-run-command type)
              :to-equal "bundle exec rails server")
      (expect (projectile-default-compilation-command type)
              :to-equal "bundle exec rake"))))

(describe "python project types"
  ;; Nearly every Python project has a pyproject.toml, so the more
  ;; specific types have to win over it.
  (it "prefers django over the packaging manifests"
    (projectile-test-with-stub-root "project" ("manage.py" "pyproject.toml" "requirements.txt")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'django))))
  (it "prefers poetry over the bare pyproject.toml"
    (projectile-test-with-stub-root "project" ("poetry.lock" "pyproject.toml")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'python-poetry))))
  (it "prefers pipenv over the bare pyproject.toml"
    (projectile-test-with-stub-root "project" ("Pipfile" "pyproject.toml")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'python-pipenv))))
  (it "prefers tox over the bare pyproject.toml"
    (projectile-test-with-stub-root "project" ("tox.ini" "pyproject.toml")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'python-tox))))
  (it "prefers pyproject.toml over setup.py and requirements.txt"
    (projectile-test-with-stub-root "project" ("pyproject.toml" "setup.py" "requirements.txt")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'python-toml))))
  (it "runs the django server as the run command, not as the compile command"
    (expect (projectile-default-run-command 'django)
            :to-equal "python manage.py runserver")))

(describe "php-symfony project type"
  (it "detects a modern Symfony layout, which has no app directory"
    (projectile-test-with-stub-root "project"
        ("composer.json" "src/" "bin/" "bin/console")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'php-symfony))))
  (it "still detects the legacy app/console layout"
    (projectile-test-with-stub-root "project"
        ("composer.json" "src/" "app/" "app/console")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'php-symfony)))))

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
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'rails-rspec)))))
  (it "detects project-type for elisp eldev projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Eldev"
       "project/project.el")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'emacs-eldev)))))
  (it "detects project-type for dotnet sln projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Project.sln")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'dotnet-sln)))))
  (it "detects project-type for dotnet slnx projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Project.slnx")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'dotnet-sln)))))
  (it "detects project-type for Julia PkgTemplates.jl projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/src/"
       "project/Project.toml")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'julia)))))
  (it "detects project-type for Zig projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/src/"
       "project/build.zig.zon")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'zig)))))
  (it "detects a Zig project that has only a build.zig"
    (projectile-test-with-stub-root "project" ("build.zig")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'zig))))
  (it "detects a bzlmod Bazel project (MODULE.bazel)"
    (projectile-test-with-stub-root "project" ("MODULE.bazel")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'bazel))))
  (it "detects a legacy WORKSPACE Bazel project"
    (projectile-test-with-stub-root "project" ("WORKSPACE")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'bazel))))
  (it "detects a Gradle project using the Kotlin DSL"
    (projectile-test-with-stub-root "project" ("build.gradle.kts")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'gradle))))
  (it "detects an Angular project (its two markers are alternatives, not both)"
    (projectile-test-with-stub-root "project" ("angular.json" "package.json")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'angular))))
  (it "detects a Taskfile.yaml go-task project"
    (projectile-test-with-stub-root "project" ("Taskfile.yaml")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (expect (projectile-detect-project-type) :to-equal 'go-task))))
  (it "does not match a project type whose marker-files are empty"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/foo")
      (let ((projectile-project-types '((empty marker-files nil)
                                        (real marker-files ("foo"))))
            (projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'real)))))
  (it "falls back to generic when the only type has empty marker-files"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/foo")
      (let ((projectile-project-types '((empty marker-files nil)))
            (projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'generic)))))
  (it "detects a marker that sits in a subdirectory of the root"
    ;; `debian/control' carries a path separator, so it can't be answered
    ;; from the root's directory listing and exercises the
    ;; `projectile-file-exists-p' fallback inside detection.
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/debian/"
       "project/debian/control")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'debian)))))
  (it "detects project-type for lowercase makefile projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/makefile")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'make)))))
  (it "detects project-type for GNUmakefile projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/GNUmakefile")
      (let ((projectile-project-type-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-project-root :and-return-value (projectile-test-project-root))
        (expect (projectile-detect-project-type) :to-equal 'gnumake)))))
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

(describe "projectile-verify-files"
  (it "requires every plain marker to be present"
    (projectile-test-with-stub-root "proj" ("a" "b")
      (expect (projectile-verify-files '("a" "b")) :to-be-truthy)
      (expect (projectile-verify-files '("a" "c")) :to-be nil)))
  (it "is satisfied by any one file of an (:any ...) clause"
    (projectile-test-with-stub-root "proj" ("b")
      (expect (projectile-verify-files '((:any "a" "b"))) :to-be-truthy)
      (expect (projectile-verify-files '((:any "a" "c"))) :to-be nil)))
  (it "still ANDs an (:any ...) clause with the other markers"
    (projectile-test-with-stub-root "proj" ("b")
      (expect (projectile-verify-files '((:any "a" "b") "src")) :to-be nil)))
  (it "answers an (:any ...) clause from the entry set"
    (let ((entry-set (make-hash-table :test 'equal)))
      (puthash "build.gradle.kts" t entry-set)
      (spy-on 'projectile-file-exists-p :and-return-value nil)
      (expect (projectile-verify-files
               '((:any "build.gradle" "build.gradle.kts")) "/whatever/" entry-set)
              :to-be-truthy)
      (expect 'projectile-file-exists-p :not :to-have-been-called))))

(describe "projectile-verify-file"
  (it "answers a plain-name file from the entry set without touching disk"
    (let ((entry-set (make-hash-table :test 'equal)))
      (puthash "Gemfile" t entry-set)
      (spy-on 'projectile-file-exists-p :and-return-value nil)
      (expect (projectile-verify-file "Gemfile" "/whatever/" entry-set) :to-be-truthy)
      (expect (projectile-verify-file "absent" "/whatever/" entry-set) :not :to-be-truthy)
      (expect 'projectile-file-exists-p :not :to-have-been-called)))
  (it "falls back to projectile-file-exists-p for a marker with a path separator"
    (let ((entry-set (make-hash-table :test 'equal)))
      (puthash "debian" t entry-set)
      (spy-on 'projectile-project-root :and-return-value "/root/")
      (spy-on 'projectile-file-exists-p :and-return-value t)
      (expect (projectile-verify-file "debian/control" nil entry-set) :to-be-truthy)
      (expect 'projectile-file-exists-p :to-have-been-called-with "/root/debian/control"))))

(describe "projectile-cabal-project-p"
  (it "is true for a project with a .cabal file and no stack.yaml"
    (projectile-test-with-stub-root "proj" ("foo.cabal")
      (expect (projectile-cabal-project-p) :to-be-truthy)))
  (it "is false once a stack.yaml is present (it's a Stack project then)"
    (projectile-test-with-stub-root "proj" ("foo.cabal" "stack.yaml")
      (expect (projectile-cabal-project-p) :to-be nil)))
  (it "is false for a project without a .cabal file"
    (projectile-test-with-stub-root "proj" ("README")
      (expect (projectile-cabal-project-p) :to-be nil))))

(describe "projectile-go-project-p"
  (it "is true for a project with a go.mod file"
    (projectile-test-with-stub-root "proj" ("go.mod")
      (expect (projectile-go-project-p) :to-be-truthy)))
  (it "is true for a project that merely contains .go sources"
    (projectile-test-with-stub-root "proj" ("main.go")
      (expect (projectile-go-project-p) :to-be-truthy)))
  (it "is false for a project with neither go.mod nor .go files"
    (projectile-test-with-stub-root "proj" ("README")
      (expect (projectile-go-project-p) :to-be nil))))

;;; projectile-project-type-test.el ends here
