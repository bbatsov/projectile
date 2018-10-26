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

(message "Running tests on Emacs %s" emacs-version)

;; TODO: Revise this init logic
(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Do not load outdated byte code for tests
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "projectile" source-directory))
  (setq projectile-test-path (expand-file-name "test" source-directory)))

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

(describe "projectile-register-project-type"
  (it "prepends new projects to projectile-project-types"
    (projectile-register-project-type 'foo '("Foo"))
    (expect (caar projectile-project-types) :to-equal 'foo)
    (projectile-register-project-type 'bar '("Bar"))
    (expect (caar projectile-project-types) :to-equal 'bar)))

(describe "projectile-project-type"
  (it "detects the type of Projectile's project"
    (expect (projectile-project-type) :to-equal 'emacs-cask))
  (it "caches the project type"
    (expect (gethash (projectile-project-root) projectile-project-type-cache) :to-equal 'emacs-cask)))

(describe "projectile-ignored-directory-p"
  (it "checks if directory should be ignored"
    (spy-on 'projectile-ignored-directories :and-return-value '("/path/to/project/tmp" "/path/to/project/t\\.*"))
    (expect (projectile-ignored-directory-p "/path/to/project/tmp") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/t.ignore") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/log") :not :to-be-truthy)))

(describe "projectile-ignored-file-p"
  (it "checks if file should be ignored"
    (spy-on 'projectile-ignored-files :and-return-value '("/path/to/project/TAGS" "/path/to/project/T.*"))
    (expect (projectile-ignored-file-p "/path/to/project/TAGS") :to-be-truthy)
    (expect (projectile-ignored-file-p "/path/to/project/foo.el") :not :to-be-truthy)))

(describe "projectile-ignored-files"
  (it "returns list of ignored files"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-project-name :and-return-value "project")
    (spy-on 'projectile-project-ignored-files :and-return-value '("foo.js" "bar.rb"))
    (let ((files'("/path/to/project/TAGS"
                  "/path/to/project/foo.js"
                  "/path/to/project/bar.rb"
                  "/path/to/project/file1.log"
                  "/path/to/project/file2.log"))
          (projectile-ignored-files '("TAGS" "file\d+\\.log")))
      (expect (projectile-ignored-files) :not :to-equal files)
      (expect (projectile-ignored-files) :to-equal '("/path/to/project/TAGS"
                                                     "/path/to/project/foo.js"
                                                     "/path/to/project/bar.rb")))))

(describe "projectile-ignored-directories"
  (it "returns list of ignored directories"
    (spy-on 'projectile-project-ignored-directories :and-return-value '("tmp" "log"))
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (let ((paths '("/path/to/project/compiled/"
                   "/path/to/project/ignoreme"
                   "/path/to/project/ignoremetoo"
                   "/path/to/project/tmp"
                   "/path/to/project/log"))
          (projectile-globally-ignored-directories '("compiled" "ignoreme")))
      (expect (projectile-ignored-directories) :not :to-equal paths)
      (expect (projectile-ignored-directories) :to-equal '("/path/to/project/compiled/"
                                                           "/path/to/project/ignoreme/"
                                                           "/path/to/project/tmp/"
                                                           "/path/to/project/log/")))))

(describe "projectile-project-ignored-files"
  (it "returns list of project ignored files"
    (let ((files '("/path/to/project/foo.el" "/path/to/project/foo.elc")))
      (spy-on 'projectile-project-ignored :and-return-value files)
      (spy-on 'file-directory-p :and-return-value nil)
      (expect (projectile-project-ignored-files) :to-equal files)
      (spy-on 'file-directory-p :and-return-value t)
      (expect (projectile-project-ignored-files) :not :to-be-truthy))))

(describe "projectile-project-ignored-directories"
  (it "returns list of project ignored directories"
    (let ((directories '("/path/to/project/tmp" "/path/to/project/log")))
      (spy-on 'projectile-project-ignored :and-return-value directories)
      (spy-on 'file-directory-p :and-return-value t)
      (expect (projectile-project-ignored-directories) :to-equal directories)
      (spy-on 'file-directory-p :and-return-value nil)
      (expect (projectile-project-ignored-directories) :not :to-be-truthy))))

(describe "projectile-project-ignored"
  (it "returns list of ignored files/directories"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-project-name :and-return-value "project")
    (spy-on 'projectile-paths-to-ignore :and-return-value (list "log" "tmp" "compiled"))
    (spy-on 'file-expand-wildcards :and-call-fake
            (lambda (pattern ignored)
              (cond
               ((string-equal pattern "log") "/path/to/project/log")
               ((string-equal pattern "tmp") "/path/to/project/tmp")
               ((string-equal pattern "compiled") "/path/to/project/compiled"))))
    (let* ((file-names '("log" "tmp" "compiled"))
           (files (mapcar 'projectile-expand-root file-names)))
      (expect (projectile-project-ignored) :to-equal files))))

(describe "projectile-remove-ignored"
  (it "removes ignored folders and files"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-project-name :and-return-value "project")
    (spy-on 'projectile-ignored-files-rel)
    (spy-on 'projectile-ignored-directories-rel)
    (let* ((file-names '("foo.c" "foo.o" "foo.so" "foo.o.gz" "foo.tar.gz" "foo.tar.GZ"))
           (files (mapcar 'projectile-expand-root file-names)))
      (let ((projectile-globally-ignored-file-suffixes '(".o" ".so" ".tar.gz")))
        (expect (projectile-remove-ignored files) :to-equal (mapcar 'projectile-expand-root '("foo.c" "foo.o.gz")))))))

(describe "projectile-add-unignored"
  (it "requires explicitly unignoring files inside ignored paths"
    (spy-on 'projectile-get-repo-ignored-files :and-return-value '("unignored-file" "path/unignored-file2"))
    (let ((projectile-globally-unignored-files '("unignored-file")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "unignored-file")))
    (let ((projectile-globally-unignored-files '("unignored-file" "path/unignored-file2")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "unignored-file" "path/unignored-file2"))))
  (it "returns the list of globally unignored files on an unsupported VCS"
    (spy-on 'projectile-project-vcs :and-return-value 'none)
    (let ((projectile-globally-unignored-files '("unignored-file")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file"))))
  (it "requires explicitly unignoring ignored files inside unignored paths"
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (spy-on 'projectile-get-repo-ignored-files :and-return-value '("path/unignored-file"))
    (spy-on 'projectile-get-repo-ignored-directory :and-call-fake
            (lambda (project vcs dir)
              (list (concat dir "unignored-file"))))
    (let ((projectile-globally-unignored-directories '("path")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "path/unignored-file"))
      (let ((projectile-globally-ignored-files '("unignored-file")))
        (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file"))
        (let ((projectile-globally-unignored-files '("path/unignored-file")))
          (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "path/unignored-file")))))))

(describe "projectile-parse-dirconfig-file"
  (it "parses dirconfig and returns directories to ignore and keep"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'file-truename :and-call-fake (lambda (filename) filename))
    (spy-on 'insert-file-contents :and-call-fake
            (lambda (filename)
              (save-excursion (insert "\n-exclude\n+include\nno-prefix\n left-wspace\nright-wspace\t\n"))))
    (expect (projectile-parse-dirconfig-file) :to-equal '(("include/")
                                                          ("exclude" "no-prefix" "left-wspace" "right-wspace")
                                                          nil))))

(describe "projectile-get-project-directories"
  (it "gets the list of project directories"
    (spy-on 'projectile-project-root :and-return-value "/my/root/")
    (spy-on 'projectile-parse-dirconfig-file :and-return-value '(nil))
    (expect (projectile-get-project-directories "/my/root") :to-equal '("/my/root")))
  (it "gets the list of project directories with dirs to keep"
    (spy-on 'projectile-project-root :and-return-value "/my/root/")
    (spy-on 'projectile-parse-dirconfig-file :and-return-value '(("foo" "bar/baz")))
    (expect (projectile-get-project-directories "/my/root/") :to-equal '("/my/root/foo" "/my/root/bar/baz"))))

(describe "projectile-dir-files"
  (it "lists the files in directory and sub-directories"
    (spy-on 'projectile-patterns-to-ignore)
    (spy-on 'projectile-index-directory :and-call-fake (lambda (dir patterns progress-reporter)
                                                         (expect dir :to-equal "/my/root/")
                                                         '("/my/root/a/b/c" "/my/root/a/d/e")))
    (spy-on 'projectile-dir-files-alien :and-return-value '("a/b/c" "a/d/e"))
    (spy-on 'cd)
    (let ((projectile-indexing-method 'native))
      (expect (projectile-dir-files "/my/root/") :to-equal '("a/b/c" "a/d/e")))
    (let ((projectile-indexing-method 'alien))
      (expect (projectile-dir-files "/my/root/") :to-equal '("a/b/c" "a/d/e")))))

(describe "projectile-get-sub-projects-command"
  (it "gets sub projects command for git"
    (expect (string-prefix-p "git" (projectile-get-sub-projects-command 'git)) :to-be-truthy))
  (it "returns empty when vcs is not supported"
    (expect (string-empty-p (projectile-get-sub-projects-command 'none)) :to-be-truthy)))

(describe "projectile-files-via-ext-command"
  (it "returns nil when command is nil or empty"
    (expect (projectile-files-via-ext-command "" "") :not :to-be-truthy)
    (expect (projectile-files-via-ext-command "" nil) :not :to-be-truthy)))

(describe "projectile-mode"
  (it "sets up hook functions"
    (spy-on 'projectile--cleanup-known-projects)
    (spy-on 'projectile-discover-projects-in-search-path)
    (projectile-mode 1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :to-be-truthy)
    (projectile-mode -1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :not :to-be-truthy)))

(describe "projectile-relevant-known-projects"
  (it "returns a list of known projects"
    (let ((projectile-known-projects '("/path/to/project1" "/path/to/project2")))
      (spy-on 'projectile-project-root :and-return-value "/path/to/project1")
      (expect (projectile-relevant-known-projects) :to-equal '("/path/to/project2")))))

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
        (mapc (lambda (d) (ignore-errors (delete-directory it))) directories)
        (delete-file projectile-known-projects-file nil)))))

(describe "projectile-project-root"
  (it "returns the absolute root directory of a project"
    (let* ((root-directory (make-temp-file "projectile-absolute" t))
           (root-file (concat root-directory "/.projectile"))
           (deep-directory (concat root-directory "/foo/bar/baz"))
           (project-file (concat deep-directory "/tmp.txt")))
      (unwind-protect
          (progn
            (mkdir deep-directory t)
            (with-temp-file root-file)
            (with-temp-file project-file)
            (with-current-buffer (find-file-noselect project-file t)
              (expect (file-name-absolute-p (projectile-project-root)) :to-be-truthy)))
        (ignore-errors (delete-directory root-directory t))))))

(describe "projectile-tags-exclude-patterns"
  (it "returns a string with exclude patterns for ctags"
    (spy-on 'projectile-ignored-directories-rel :and-return-value (list ".git/" ".hg/"))
    (expect (projectile-tags-exclude-patterns) :to-equal "--exclude=\".git\" --exclude=\".hg\"")))

(describe "projectile-maybe-invalidate-cache"
  (it "should not invalidate cache if dirconfig is older than cache"
    (spy-on 'projectile-invalidate-cache :and-return-value t)
    (expect (projectile-maybe-invalidate-cache nil) :not :to-be-truthy))
  (it "should invalidate cache if force is t"
    (spy-on 'projectile-invalidate-cache :and-return-value t)
    (expect (projectile-maybe-invalidate-cache t) :to-be-truthy))
  (it "should invalidate cache if dirconfig is newer than cache"
    (spy-on 'projectile-invalidate-cache :and-return-value t)
    (spy-on 'file-newer-than-file-p :and-return-value t)
    (expect (projectile-maybe-invalidate-cache nil) :to-be-truthy)))

(describe "projectile-root-top-down"
  (it "identifies the root directory of a project by top-down search"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/.git/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/src/framework.conf"
       "projectA/src/html/index.html")
      (expect (projectile-root-top-down "projectA/src/framework/lib" '("framework.conf" ".git"))
              :to-equal
              (expand-file-name "projectA/src/"))
      (expect (projectile-root-top-down "projectA/src/framework/lib" '(".git" "framework.conf"))
              :to-equal
              (expand-file-name "projectA/src/"))
      (expect (projectile-root-top-down "projectA/src/html/" '(".svn"))
              :to-equal
              (expand-file-name "projectA/src/html/"))))))

(describe "projectile-root-top-down-recurring"
  (it "identifies the root directory of a project by recurring top-down search"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/.git/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/src/framework/framework.conf"
       "projectA/src/html/index.html"
       ".projectile")
      (expect (projectile-root-top-down-recurring "projectA/src/html/" '("something" ".svn" ".git"))
              :to-equal
              (expand-file-name "projectA/"))
      (expect (projectile-root-top-down-recurring "projectA/src/html/" '(".git"))
              :to-equal
              (expand-file-name "projectA/"))
      (expect (projectile-root-top-down-recurring "projectA/src/html/" '("elusivefile"))
              :not :to-be-truthy)))))

(describe "projectile-root-bottom-up"
  (it "identifies the root directory of a project by bottom-up search"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/.git/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/src/framework/framework.conf"
       "projectA/src/html/index.html"
       "projectA/.projectile")
      (expect (projectile-root-bottom-up "projectA/src/framework/lib" '(".git" ".svn"))
              :to-equal
              (expand-file-name "projectA/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '(".git" ".svn"))
              :to-equal
              (expand-file-name "projectA/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '(".svn" ".git"))
              :to-equal
              (expand-file-name "projectA/src/html/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '(".projectile" "index.html"))
              :to-equal
              (expand-file-name "projectA/"))))))

(describe "projectile-project-root"
  (defun projectile-test-should-root-in (root directory)
    (let ((projectile-project-root-cache (make-hash-table :test 'equal)))
      (expect (let ((default-directory
                             (expand-file-name
                              (file-name-as-directory directory))))
                       (file-truename (projectile-project-root)))
              :to-equal
              (file-truename (file-name-as-directory root)))))

  (it "returns the root directory of a project"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/build/framework/lib/"
       "projectA/requirements/a/b/c/d/e/f/g/"
       "projectA/src/framework/framework.conf"
       "projectA/requirements/a/b/c/requirements.txt"
       "projectA/src/html/index.html"
       "projectA/.projectile"
       "override")
      (let ((projectile-project-root-files-bottom-up '("somefile" ".projectile"))
            (projectile-project-root-files '("otherfile" "framework.conf" "requirements.txt"))
            (projectile-project-root-files-top-down-recurring '(".svn" ".foo"))
            (projectile-project-root-files-functions '(projectile-root-bottom-up
                                                       projectile-root-top-down
                                                       projectile-root-top-down-recurring)))
        (projectile-test-should-root-in "projectA" "projectA/requirements/a/b/c/d/e/f/g")
        (projectile-test-should-root-in "projectA" "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA" "projectA/src/html")

        (setq projectile-project-root-files-functions '(projectile-root-top-down
                                                        projectile-root-top-down-recurring
                                                        projectile-root-bottom-up))
        (projectile-test-should-root-in "projectA/requirements/a/b/c"
                                        "projectA/requirements/a/b/c/d/e/f/g")
        (projectile-test-should-root-in "projectA/src/framework"
                                        "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA/src"
                                        "projectA/src/html"))

      (let ((projectile-project-root-files-bottom-up '("somefile" ".projectile"))
            (projectile-project-root-files '("otherfile" "noframework.conf"))
            (projectile-project-root-files-top-down-recurring '(".svn" ".foo"))
            (projectile-project-root-files-functions '(projectile-root-top-down-recurring
                                                       projectile-root-bottom-up
                                                       projectile-root-top-down)))
        (projectile-test-should-root-in "projectA/src" "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA/src" "projectA/src/html")
        (projectile-test-should-root-in "projectA/" "projectA/build/framework/lib"))

      (let ((projectile-project-root-files-bottom-up '("somefile" "override"))
            (projectile-project-root-files '("otherfile" "anotherfile"))
            (projectile-project-root-files-top-down-recurring '("someotherfile" "yetanotherfile"))
            (projectile-project-root-files-functions '(projectile-root-bottom-up
                                                       projectile-root-top-down
                                                       projectile-root-top-down-recurring)))
        (projectile-test-should-root-in default-directory "projectA/src/framework/lib")
        (projectile-test-should-root-in default-directory "projectA/src/html"))

      (let ((projectile-project-root-files-bottom-up '("somecoolfile"))
            (projectile-project-root-files nil)
            (projectile-project-root-files-top-down-recurring '(".svn"))
            (projectile-project-root-files-functions '(projectile-root-bottom-up
                                                       projectile-root-top-down
                                                       projectile-root-top-down-recurring)))
        (projectile-test-should-root-in "projectA/src/" "projectA/src/")
        (projectile-test-should-root-in "projectA/src/" "projectA/src/html"))))))

(describe "projectile-file-exists-p"
  (it "returns t if file exists"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/dirA/dirB/"
       "project/fileA")
      (let ((projectile-file-exists-local-cache-expire nil)
            (projectile-file-exists-remote-cache-expire nil))
        (expect (projectile-file-exists-p "project/fileA") :to-be-truthy)
        (expect (projectile-file-exists-p "project/dirA/dirB") :to-be-truthy)
        (expect (projectile-file-exists-p "project/dirA/fileB") :not :to-be-truthy)
        (with-temp-file "project/dirA/fileB")
        (expect (projectile-file-exists-p "project/dirA/fileB") :to-be-truthy)
        (expect (projectile-file-exists-p "project/nofile") :not :to-be-truthy)
        (delete-file "project/fileA")
        (expect (projectile-file-exists-p "project/fileA") :not :to-be-truthy)))))
  (it "caches the results"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("dirA/dirB/"
       "fileA")
      (let ((initial-time (current-time))
            (projectile-file-exists-local-cache-expire 100)
            (projectile-file-exists-remote-cache-expire nil))

        (spy-on 'run-with-timer :and-return-value 'nooptimer)
        (spy-on 'current-time :and-return-value initial-time)
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (expect (projectile-file-exists-p "dirA/dirB") :to-be-truthy)
        (expect (projectile-file-exists-p "dirA/fileB") :not :to-be-truthy)
        (with-temp-file "dirA/fileB")
        (expect (projectile-file-exists-p "dirA/fileB") :not :to-be-truthy)
        (delete-file "fileA")
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 50)))
        (projectile-file-exists-cache-cleanup)
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (expect (projectile-file-exists-p "dirA/fileB") :not :to-be-truthy)
        (expect (projectile-file-exists-p "fileC") :not :to-be-truthy)
        (with-temp-file "fileC")
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 120)))
        (projectile-file-exists-cache-cleanup)
        (expect (projectile-file-exists-p "dirA/fileB") :to-be-truthy)
        (expect (projectile-file-exists-p "fileA") :not :to-be-truthy)
        (expect (projectile-file-exists-p "fileC") :not :to-be-truthy)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 220)))
        (projectile-file-exists-cache-cleanup)
        (expect (projectile-file-exists-p "fileC") :to-be-truthy)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 1000)))
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :not :to-be-truthy))))))

(describe "projectile-project-root"
  (it "caches the current file"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile"
       "project/file1.el"
       "project/file2.el"
       "project/file3.el"
       "project/file4.el")
      (cd "project")
      (let ((projectile-projects-cache (make-hash-table :test #'equal))
            (projectile-projects-cache-time (make-hash-table :test #'equal))
            (projectile-enable-caching t))
        (puthash (projectile-project-root)
                 '("file1.el")
                 projectile-projects-cache)
        (spy-on 'projectile-project-root :and-call-fake (lambda () (file-truename default-directory)))
        (spy-on 'projectile-project-vcs :and-return-value 'none)
        (with-current-buffer (find-file-noselect  "file2.el" t)
          (projectile-cache-current-file)
          (dolist (f '("file1.el" "file2.el"))
            (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy)))
        (with-current-buffer (find-file-noselect "file3.el" t)
          (projectile-cache-current-file)
          (dolist (f '("file1.el" "file2.el" "file3.el"))
            (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy)))
        (with-current-buffer (find-file-noselect "file4.el" t)
          (projectile-cache-current-file)
          (dolist (f '("file1.el" "file2.el" "file3.el" "file4.el"))
            (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy)))))))
  (it "ensures that we update the cache if it's expired"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile"
       "project/file1.el"
       "project/file2.el")
      (cd "project")
      (let ((projectile-projects-cache (make-hash-table :test #'equal))
            (projectile-projects-cache-time (make-hash-table :test #'equal))
            (projectile-enable-caching t)
            (projectile-files-cache-expire 10))
        ;; Create a stale cache with only one file in it.
        (puthash (projectile-project-root)
                 '("file1.el")
                 projectile-projects-cache)
        (puthash (projectile-project-root)
                 0 ;; Cached 1st of January 1970.
                 projectile-projects-cache-time)

        (spy-on 'projectile-project-root :and-call-fake (lambda () (file-truename default-directory)))
        (spy-on 'projectile-project-vcs :and-return-value 'none)
        ;; After listing all the files, the cache should have been updated.
        (projectile-current-project-files)
        ;; find returns the leading ./ therefore the somewhat odd notation here
        (dolist (f '("./file1.el" "./file2.el"))
          (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy))))))
  (it "ensures that we don't cache a project root if the path has changed."
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile")
      (cd "project")
      (let ((projectile-project-root-cache (make-hash-table :test #'equal))
            (correct-project-root (projectile-project-root)))
        ;; If this project has been moved, then we will have stale
        ;; paths in the cache.
        (puthash
         (format "projectile-root-bottom-up-%s" correct-project-root)
         "/this/path/does/not/exist"
         projectile-project-root-cache)
        (expect (projectile-project-root) :to-equal correct-project-root))))))

(describe "projectile-grep"
  (it "grep a git project using default files"
    (require 'vc-git)
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/c/src/"
       "project/c/include/"
       "project/go/src/package1/"
       "project/.projectile")
      (cd "project")
      (with-temp-file "go/src/package1/x.go" (insert "foo(bar)"))
      (with-temp-file "c/include/x.h" (insert "typedef struct bar_t"))
      (with-temp-file "c/src/x.c" (insert "struct bar_t *x"))
      (dolist (test '(("go/src/package1/x.go" "foo" "*.go")
                      ("c/src/x.c" "bar_t" "*.[ch]")
                      ("c/include/x.h" "bar_t" "*.[ch]")))
        (let ((projectile-use-git-grep t)
              (current-prefix-arg '-)
              (sym (cadr test)))
          (spy-on 'projectile-project-vcs :and-return-value 'git)
          (spy-on 'read-string :and-call-fake
                  (lambda (prompt initial-input history default-value &rest args)
                    (if (should (equal sym default-value)) default-value)))
          (spy-on 'vc-git-grep :and-call-fake
                  (lambda (regexp files dir)
                    (progn (expect regexp :to-equal sym)
                           (expect files :to-equal (car (last test)))
                           (expect (projectile-project-root) :to-equal dir))))
          (with-current-buffer (find-file-noselect (car test) t)
            (save-excursion
              (re-search-forward sym)
              (projectile-grep nil ?-)))))))))

(describe "projectile-switch-project"
  (it "fails if there are no projects"
    (let ((projectile-known-projects nil))
      (expect (projectile-switch-project) :to-throw))))

(describe "projectile-ignored-buffer-p"
  (it "checks if buffer should be ignored"
    (let ((projectile-globally-ignored-buffers '("*nrepl messages*" "*something*")))
      (expect (projectile-ignored-buffer-p (get-buffer-create "*nrepl messages*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "*something*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "test")) :not :to-be-truthy))))

(describe "projectile-get-other-files"
  (it "returns files with same names but different extensions"
    (let ((projectile-other-file-alist '(;; handle C/C++ extensions
                                         ("cpp" . ("h" "hpp" "ipp"))
                                         ("ipp" . ("h" "hpp" "cpp"))
                                         ("hpp" . ("h" "ipp" "cpp"))
                                         ("cxx" . ("hxx" "ixx"))
                                         ("ixx" . ("cxx" "hxx"))
                                         ("hxx" . ("ixx" "cxx"))
                                         ("c" . ("h"))
                                         ("m" . ("h"))
                                         ("mm" . ("h"))
                                         ("h" . ("c" "cpp" "ipp" "hpp" "m" "mm"))
                                         ("cc" . ("hh"))
                                         ("hh" . ("cc"))

                                         ;; vertex shader and fragment shader extensions in glsl
                                         ("vert" . ("frag"))
                                         ("frag" . ("vert"))

                                         ;; handle files with no extension
                                         (nil . ("lock" "gpg"))
                                         ("lock" . (""))
                                         ("gpg" . (""))

                                         ;; handle files with nested extensions
                                         ("service.js" . ("service.spec.js"))
                                         ("js" . ("js"))))
          (source-tree '("src/test1.c"
                       "src/test2.c"
                       "src/test+copying.m"
                       "src/test1.cpp"
                       "src/test2.cpp"
                       "src/Makefile"
                       "src/test.vert"
                       "src/test.frag"
                       "src/same_name.c"
                       "src/some_module/same_name.c"
                       "include1/same_name.h"
                       "include1/test1.h"
                       "include1/test1.h~"
                       "include1/test2.h"
                       "include1/test+copying.h"
                       "include1/test1.hpp"
                       "include2/some_module/same_name.h"
                       "include2/test1.h"
                       "include2/test2.h"
                       "include2/test2.hpp"

                       "src/test1.service.js"
                       "src/test2.service.spec.js"
                       "include1/test1.service.spec.js"
                       "include2/test1.service.spec.js"
                       "include1/test2.js"
                       "include2/test2.js")))

      (expect (projectile-get-other-files "src/test1.c" source-tree) :to-equal '("include1/test1.h" "include2/test1.h"))
      (expect (projectile-get-other-files "src/test1.cpp" source-tree) :to-equal '("include1/test1.h" "include2/test1.h" "include1/test1.hpp"))
      (expect (projectile-get-other-files "test2.c" source-tree) :to-equal '("include1/test2.h" "include2/test2.h"))
      (expect (projectile-get-other-files "test2.cpp" source-tree) :to-equal '("include1/test2.h" "include2/test2.h" "include2/test2.hpp"))
      (expect (projectile-get-other-files "test1.h" source-tree) :to-equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp"))
      (expect (projectile-get-other-files "test2.h" source-tree) :to-equal '("src/test2.c" "src/test2.cpp" "include2/test2.hpp"))
      (expect (projectile-get-other-files "include1/test1.h" source-tree t) :to-equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp"))
      (expect (projectile-get-other-files "Makefile.lock" source-tree) :to-equal '("src/Makefile"))
      (expect (projectile-get-other-files "include2/some_module/same_name.h" source-tree) :to-equal '("src/some_module/same_name.c" "src/same_name.c"))
      ;; nested extensions
      (expect (projectile-get-other-files "src/test1.service.js" source-tree) :to-equal '("include1/test1.service.spec.js" "include2/test1.service.spec.js"))
      ;; fallback to outer extensions if no rule for nested extension defined
      (expect (projectile-get-other-files "src/test2.service.spec.js" source-tree) :to-equal '("include1/test2.js" "include2/test2.js"))
      (expect (projectile-get-other-files "src/test+copying.m" source-tree) :to-equal '("include1/test+copying.h")))))

(describe "projectile-compilation-dir"
  (it "returns the compilation directory for a project"
    (defun helper (project-root rel-dir)
      (spy-on 'projectile-project-root :and-return-value project-root)
      (spy-on 'projectile-project-type :and-return-value 'generic)
      (let ((projectile-project-compilation-dir rel-dir))
        (projectile-compilation-dir)))
    (expect (helper "/root/" "build") :to-equal "/root/build/")
    (expect (helper "/root/" "build/") :to-equal "/root/build/")
    (expect (helper "/root/" "./build") :to-equal "/root/build/")
    (expect (helper "/root/" "local/build") :to-equal "/root/local/build/"))
  (it "returns the default compilation dir based on project-type"
    (projectile-register-project-type 'default-dir-project '("file.txt")
                                      :compilation-dir "build")
    (defun helper (project-root &optional rel-dir)
      (spy-on 'projectile-project-root :and-return-value project-root)
      (spy-on 'projectile-project-type :and-return-value 'default-dir-project)
      (if (null rel-dir)
          (projectile-compilation-dir)
        (let ((projectile-project-compilation-dir rel-dir))
          (projectile-compilation-dir))))
    (expect (helper "/root/") :to-equal "/root/build/")
    (expect (helper "/root/" "buildings") :to-equal "/root/buildings/"))
  (it "should not fail on bad compilation dir config"
    (defun -compilation-test-function ()
      1)
    (let ((projectile-project-type 'has-command-at-point)
          (projectile-project-compilation-dir nil))
      (projectile-register-project-type 'has-command-at-point '("file.txt")
                                        :compile (-compilation-test-function))
      (expect (projectile-compilation-dir) :to-equal (concat (expand-file-name ".") "/")))))

(describe "projectile-default-compilation-command"
  (it "returns the default compilation command for project-type"
    (defun -compilation-test-function ()
      (if (= (point) 1)
          "my-make"
        "./run-extra"))
    (projectile-register-project-type 'has-command-at-point '("file.txt")
                                      :compile '-compilation-test-function)
    (expect (projectile-default-compilation-command 'has-command-at-point) :to-equal "my-make")
    (with-temp-buffer
      (insert "ABCDE")
      (goto-char 2)
      (expect (projectile-default-compilation-command 'has-command-at-point) :to-equal "./run-extra")))
  (it "fails on bad project-type config"
    (defun -compilation-test-function ()
      1)
    (projectile-register-project-type 'has-command-at-point '("file.txt")
                                      :compile (-compilation-test-function))
    (expect (projectile-default-compilation-command 'has-command-at-point) :to-throw)))

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
        (expect (projectile-detect-project-type) :to-equal 'rails-rspec))))))

(describe "projectile-dirname-matching-count"
  (it "counts matching dirnames ascending file paths"
    (expect (projectile-dirname-matching-count "src/food/sea.c" "src/food/cat.c") :to-equal 2)
    (expect (projectile-dirname-matching-count "src/weed/sea.c" "src/food/sea.c") :to-equal 0)
    (expect (projectile-dirname-matching-count "test/demo-test.el" "demo.el") :to-equal 0)))

(describe "projectile-find-matching-test"
  (it "finds matching test or file"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/app/models/weed/"
       "project/app/models/food/"
       "project/spec/models/weed/"
       "project/spec/models/food/"
       "project/app/models/weed/sea.rb"
       "project/app/models/food/sea.rb"
       "project/spec/models/weed/sea_spec.rb"
       "project/spec/models/food/sea_spec.rb")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-type :and-return-value 'rails-rspec)
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-find-matching-test "app/models/food/sea.rb") :to-equal "spec/models/food/sea_spec.rb")
        (expect (projectile-find-matching-file "spec/models/food/sea_spec.rb") :to-equal "app/models/food/sea.rb")))))
  (it "finds matching test or file in a custom project"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/src/foo/"
       "project/src/bar/"
       "project/test/foo/"
       "project/test/bar/"
       "project/src/foo/foo.service.js"
       "project/src/bar/bar.service.js"
       "project/test/foo/foo.service.spec.js"
       "project/test/bar/bar.service.spec.js")
      (let ((projectile-indexing-method 'native))
        (projectile-register-project-type 'npm-project '("somefile") :test-suffix ".spec")
        (spy-on 'projectile-project-type :and-return-value 'npm-project)
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-find-matching-test "src/foo/foo.service.js") :to-equal "test/foo/foo.service.spec.js")
        (expect (projectile-find-matching-file "test/bar/bar.service.spec.js") :to-equal "src/bar/bar.service.js")))))
  (it "finds matching test or file in a custom project with dirs"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/source/foo/"
       "project/source/bar/"
       "project/spec/foo/"
       "project/spec/bar/"
       "project/source/foo/foo.service.js"
       "project/source/bar/bar.service.js"
       "project/spec/foo/foo.service.spec.js"
       "project/spec/bar/bar.service.spec.js")
      (let ((projectile-indexing-method 'native))
        (projectile-register-project-type 'npm-project '("somefile")
                                          :test-suffix ".spec"
                                          :test-dir "spec/"
                                          :src-dir "source/")
        (spy-on 'projectile-project-type :and-return-value 'npm-project)
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-find-matching-test "source/foo/foo.service.js") :to-equal "spec/foo/foo.service.spec.js")
        (expect (projectile-find-matching-file "spec/bar/bar.service.spec.js") :to-equal "source/bar/bar.service.js"))))))

(describe "projectile-get-all-sub-projects"
  (it "excludes out-of-project submodules"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      (;; VCS root is here
       "project/"
       "project/.git/"
       "project/.gitmodules"
       ;; Current project root is here:
       "project/web-ui/"
       "project/web-ui/.projectile"
       ;; VCS git submodule will return the following submodules,
       ;; relative to current project root, 'project/web-ui/':
       "project/web-ui/vendor/client-submodule/"
       "project/server/vendor/server-submodule/")
      (let ((project (file-truename (expand-file-name "project/web-ui"))))
        (spy-on 'projectile-files-via-ext-command :and-call-fake
                (lambda (dir vcs)
                  (when (string= dir project)
                    '("vendor/client-submodule"
                      "../server/vendor/server-submodule"))))
        (spy-on 'projectile-project-root :and-return-value project)
        ;; assert that it only returns the submodule 'project/web-ui/vendor/client-submodule/'
        (expect (projectile-get-all-sub-projects project) :to-equal
                (list (expand-file-name "vendor/client-submodule/" project))))))))

(describe "projectile-configure-command"
  (it "configure command for generic project type"
    (spy-on 'projectile-default-configure-command :and-return-value nil)
    (spy-on 'projectile-project-type :and-return-value 'generic)
    (expect (projectile-configure-command "fsdf") :to-equal nil)))

;;; known projects tests

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

(describe "projectile-test-ignored-directory-p"
  (it "ignores specified literal directory values"
    (spy-on 'projectile-ignored-directories :and-return-value '("/path/to/project/tmp"))
    (expect (projectile-ignored-directory-p "/path/to/project/tmp") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/log") :not :to-be-truthy))
  (it "ignores specified regex directory values"
    (spy-on 'projectile-ignored-directories :and-return-value '("/path/to/project/t\\.*"))
    (expect (projectile-ignored-directory-p "/path/to/project/tmp") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/log") :not :to-be-truthy)))

(describe "projectile-relevant-known-projects"
  :var 'known-projects
  (before-all
    (setq known-projects '("~/foo/" "~/bar/" "~/baz/")))

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
