;;; projectile-test.el --- Projectile's test suite -*- lexical-binding: t -*-

;; Copyright © 2011-2022 Bozhidar Batsov

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

;; Projectile's Buttercup-powered test suite.  You can run it either
;; interactively or via the command-line (e.g. via eldev test).

;;; Code:

(require 'projectile)
(require 'buttercup)

;; Useful debug information
(message "Running tests on Emacs %s" emacs-version)

;; TODO: Revise this init logic
(defvar projectile-test-path (let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
                                    (source-directory (locate-dominating-file current-file "Eldev"))
                                    ;; Do not load outdated byte code for tests
                                    (load-prefer-newer t))
                               ;; Load the file under test
                               (load (expand-file-name "projectile" source-directory))
                               (expand-file-name "test" source-directory)))

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

(defmacro projectile-test-with-files-using-custom-project (files project-options &rest body)
  "Evaluate BODY with the custom project having PROJECT-OPTIONS with FILES."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  `(let ((projectile-indexing-method 'native)
         (projectile-projects-cache (make-hash-table :test 'equal))
         (projectile-projects-cache-time (make-hash-table :test 'equal))
         (projectile-enable-caching t))
     ,@(mapcar (lambda (file)
                 (let* ((path (concat "project/" file))
                        (dir (file-name-directory path)))
                   (if (string-suffix-p "/" file)
                       `(make-directory ,path t)
                     `(progn
                        (make-directory ,dir t)
                        (with-temp-file ,path)))))
               files)
     (projectile-register-project-type 'sample-project '("somefile") ,@project-options)
     (spy-on 'projectile-project-type :and-return-value 'sample-project)
     (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
     ,@body))

;;; To avoid a macro, we could write a single "it" and iterate over a list of
;;; functions. However, it would require manually rescuing the error and manually
;;; re-throwing the error to include the function name in the error string, to
;;; make it clear which function we're dealing with.  That could be avoided if
;;; buttercup allowed us to specify a custom error message like this:
;;;
;;; (expect (funcall 'foo) :to-throw 'error nil "Custom error message here")
(defmacro assert-friendly-error-when-no-project (fn)
  "Write a test that ensures FN throws a friendly error when called without a project."
  (let ((description (concat "when calling " (symbol-name fn) " without a project")))
    `(describe
      ,description
      :var ((fn ',fn))
      (it "throws a friendly error"
          (projectile-test-with-sandbox
           (projectile-test-with-files
            ("index.html")
            (find-file-noselect "index.html" t)
            ;; Avoid "the current buffer is not visiting a file" error
            (write-file "index.html")
            (spy-on 'projectile-project-root :and-return-value nil)
            (let ((projectile-require-project-root t))
              (expect (call-interactively fn)
                      :to-throw
                      'error
                      (list (concat "Projectile cannot find a project definition in "
                                    default-directory))))))))))

(defun projectile-test-tmp-file-path ()
  "Return a filename suitable to save data to in the test temp directory."
  (concat projectile-test-path
          "/tmp/temporary-file-" (format "%d" (random))
          ".eld"))

(defun file-handler-for-tests (operation &rest args)
  "Handler for # files.
Just delegates OPERATION and ARGS for all operations except for`shell-command`'."
  (let ((inhibit-file-name-handlers
         (cons 'file-handler-for-tests
               (and (eq inhibit-file-name-operation operation)
                  inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (cond ((eq operation 'shell-command) (let ((default-directory ""))
                                           (shell-command "echo magic" t)))
          (t (apply operation args)))))

(add-to-list 'file-name-handler-alist (cons "^#" 'file-handler-for-tests))

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
    (expect (caar projectile-project-types) :to-equal 'bar)))

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
       :to-throw))))

(describe "projectile-project-type"
  (it "detects the type of Projectile's project"
    (expect (projectile-project-type) :to-equal 'emacs-eldev))
  (it "caches the project type"
    (expect (gethash (projectile-project-root) projectile-project-type-cache) :to-equal 'emacs-eldev)))

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
    (let ((files '("/path/to/project/TAGS"
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
            (lambda (project dir vcs)
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
              (save-excursion (insert "\n-exclude\n+include\n#may-be-a-comment\nno-prefix\n left-wspace\nright-wspace\t\n"))))
    (expect (projectile-parse-dirconfig-file) :to-equal '(("include/")
                                                          ("exclude"
							   "#may-be-a-comment"
							   "no-prefix"
							   "left-wspace"
							   "right-wspace")
                                                          nil))
    ;; same test - but with comment lines enabled using prefix '#'
    (let ((projectile-dirconfig-comment-prefix ?#))
      (expect (projectile-parse-dirconfig-file) :to-equal '(("include/")
							    ("exclude"
							     "no-prefix"
							     "left-wspace"
							     "right-wspace")
							    nil)))
    ))

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
  (it "fails unless directory exists"
    (spy-on 'file-directory-p :and-call-fake
            (lambda (filename) (equal filename "/my/root/")))
    (expect (projectile-dir-files "asdf") :to-throw))
  (it "lists the files in directory and sub-directories"
    (spy-on 'file-directory-p :and-call-fake
            (lambda (filename) (equal filename "/my/root/")))
    (spy-on 'projectile-patterns-to-ignore)
    (spy-on 'projectile-index-directory :and-call-fake (lambda (dir patterns progress-reporter)
                                                         (expect dir :to-equal "/my/root/")
                                                         '("/my/root/a/b/c" "/my/root/a/d/e")))
    (spy-on 'projectile-dir-files-alien :and-return-value '("a/b/c" "a/d/e"))
    (spy-on 'cd)
    (let ((projectile-indexing-method 'native))
      (expect (projectile-dir-files "/my/root/") :to-equal '("a/b/c" "a/d/e")))
    (let ((projectile-indexing-method 'hybrid))
      (expect (projectile-dir-files "/my/root/") :to-equal '("a/b/c" "a/d/e")))))

(describe "projectile-get-sub-projects-command"
  (it "gets sub projects command for git"
    (expect (string-prefix-p "git" (projectile-get-sub-projects-command 'git)) :to-be-truthy))
  (it "returns empty when vcs is not supported"
    (expect (string-empty-p (projectile-get-sub-projects-command 'none)) :to-be-truthy)))

(describe "projectile-files-via-ext-command"
          (it "returns nil when command is nil or empty or fails"
              (expect (projectile-files-via-ext-command "" "") :not :to-be-truthy)
              (expect (projectile-files-via-ext-command "" nil) :not :to-be-truthy)
              (expect (projectile-files-via-ext-command "" "echo Not a file name! > &2") :not :to-be-truthy)
              (expect (projectile-files-via-ext-command "" "echo filename") :to-equal '("filename")))

          (it "supports magic file handlers"
              (expect (projectile-files-via-ext-command "#magic#" "echo filename") :to-equal '("magic"))))

(describe "projectile-mode"
  (before-each
    (spy-on 'projectile--cleanup-known-projects)
    (spy-on 'projectile-discover-projects-in-search-path))
  (it "sets up hook functions"
    (projectile-mode 1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :to-be-truthy)
    (projectile-mode -1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :not :to-be-truthy))
  (it "respects projectile-auto-discover setting"
    (unwind-protect
        (progn
          (let ((projectile-auto-discover nil))
            (projectile-mode 1)
            (expect 'projectile-discover-projects-in-search-path :not :to-have-been-called))
          (let ((projectile-auto-discover t))
            (projectile-mode 1)
            (expect 'projectile-discover-projects-in-search-path :to-have-been-called)))
      (projectile-mode -1))))

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
        (mapc (lambda (d) (ignore-errors (delete-directory d))) directories)
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
            (projectile-project-root-functions '(projectile-root-bottom-up
                                                 projectile-root-top-down
                                                 projectile-root-top-down-recurring)))
        (projectile-test-should-root-in "projectA" "projectA/requirements/a/b/c/d/e/f/g")
        (projectile-test-should-root-in "projectA" "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA" "projectA/src/html")

        (setq projectile-project-root-functions '(projectile-root-top-down
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
            (projectile-project-root-functions '(projectile-root-top-down-recurring
                                                 projectile-root-bottom-up
                                                 projectile-root-top-down)))
        (projectile-test-should-root-in "projectA/src" "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA/src" "projectA/src/html")
        (projectile-test-should-root-in "projectA/" "projectA/build/framework/lib"))

      (let ((projectile-project-root-files-bottom-up '("somefile" "override"))
            (projectile-project-root-files '("otherfile" "anotherfile"))
            (projectile-project-root-files-top-down-recurring '("someotherfile" "yetanotherfile"))
            (projectile-project-root-functions '(projectile-root-bottom-up
                                                 projectile-root-top-down
                                                 projectile-root-top-down-recurring)))
        (projectile-test-should-root-in default-directory "projectA/src/framework/lib")
        (projectile-test-should-root-in default-directory "projectA/src/html"))

      (let ((projectile-project-root-files-bottom-up '("somecoolfile"))
            (projectile-project-root-files nil)
            (projectile-project-root-files-top-down-recurring '(".svn"))
            (projectile-project-root-functions '(projectile-root-bottom-up
                                                 projectile-root-top-down
                                                 projectile-root-top-down-recurring)))
        (projectile-test-should-root-in "projectA/src/" "projectA/src/")
        (projectile-test-should-root-in "projectA/src/" "projectA/src/html")))))

  (it "caches permanent failure to find a project root"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/src/")
      (let* ((projectile-project-root-functions '())
             (dir "projectA/src")
             (cache-key (format "projectilerootless-%s" dir))
             (projectile-project-root-cache (make-hash-table :test 'equal)))
        (expect (gethash cache-key projectile-project-root-cache) :to-be nil)
        (expect (projectile-project-root dir) :to-be nil)
        ;; now that this has run once, the cache should be populated with 'none
        (expect (gethash cache-key projectile-project-root-cache) :to-be 'none)
        ;; but projectile-project-root should still return nil
        (expect (projectile-project-root dir) :to-be nil)))))

  (it "does not cache transitory failure to find a project root"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/src/")
      ;; hackish, but override file-remote-p for a moment, which is called in
      ;; projectile-project-root with 1 argument to test if the file is remote,
      ;; and 3 arguments to see if the file is connected.  We want to return t
      ;; when checking if remote, and nil when checking if connected.
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (&rest args) (eql 1 (length args)))))
        (let* ((projectile-project-root-functions '())
               (dir "projectA/src")
               (cache-key (format "projectilerootless-%s" dir))
               (projectile-project-root-cache (make-hash-table :test 'equal)))
          (expect (gethash cache-key projectile-project-root-cache) :to-be nil)
          (expect (projectile-project-root dir) :to-be nil)
          ;; since the failure was transitory, there should be nothing cached
          (expect (gethash cache-key projectile-project-root-cache) :to-be nil)
          ;; and projectile-project-root should still return nil
          (expect (projectile-project-root dir) :to-be nil)))))))

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

        (spy-on 'projectile-acquire-root :and-call-fake (lambda () (file-truename default-directory)))
        (spy-on 'projectile-project-vcs :and-return-value 'none)
        ;; After listing all the files, the cache should have been updated.
        (projectile-current-project-files)
        ;; find returns the leading ./ therefore the somewhat odd notation here
        (dolist (f '("file1.el" "file2.el"))
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
  (describe "multi-root grep"
    (after-each
      (cl-flet ((grep-buffer-p (b) (string-prefix-p "*grep" (buffer-name b))))
        (let ((grep-buffers (cl-remove-if-not #'grep-buffer-p (buffer-list))))
          (dolist (grep-buffer grep-buffers)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer grep-buffer))))))
    (it "grep multi-root projects"
      (projectile-test-with-sandbox
        (projectile-test-with-files
            ("project/bar/"
             "project/baz/")
          (cd "project")
          (with-temp-file ".projectile" (insert (concat "+/baz\n"
                                                        "+/bar\n")))
          (with-temp-file "foo.txt" (insert "hi"))
          (with-temp-file "bar/bar.txt" (insert "hi"))
          (with-temp-file "baz/baz.txt" (insert "hi"))
          (with-current-buffer (find-file-noselect ".projectile" t)
            (let ((grep-find-template "<X>")
                  grep-find-ignored-directories grep-find-ignored-files
                  projectile-globally-ignored-files
                  projectile-globally-ignored-file-suffixes
                  projectile-globally-ignored-directories)
              (projectile-grep "hi")))))))

  (describe "rgrep"
    (before-each
      (spy-on 'compilation-start))
    (it "excludes global ignores"
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/"
         "project/.projectile")
        (cd "project")
        (with-current-buffer (find-file-noselect ".projectile" t)
          (let ((grep-find-template "<X>")
                (grep-find-ignored-directories '("IG_DIR"))
                (grep-find-ignored-files '("IG_FILE"))
                (projectile-globally-ignored-files '("GLOB_IG_FILE"))
                (projectile-globally-ignored-file-suffixes '("IG_SUF"))
                (projectile-globally-ignored-directories '("GLOB_IG_DIR")))
            (projectile-grep "hi")))
        (expect 'compilation-start :to-have-been-called-with
                (concat "-type d \\( -path \\*/IG_DIR \\) -prune -o "
                        "\\! -type d \\( -name IG_FILE -o -name \\*IG_SUF \\) -prune -o "
                        "\\( -path ./GLOB_IG_DIR -o -path ./GLOB_IG_FILE \\) -prune -o ")
                'grep-mode))))
    (it "excludes project ignores"
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/bar/"
         "project/baz/")
        (cd "project")
        (with-temp-file ".projectile" (insert (concat "-/*.txt\n"
                                                      "-/bar/*.txt\n"
                                                      "-/baz\n"
                                                      "-*.txt\n"
                                                      "-*.text\n"
                                                      "!/abc.txt\n"
                                                      "!/bar/abc.txt\n"
                                                      "!def.txt\n")))
        (with-temp-file "foo.txt")
        (with-temp-file "abc.txt")
        (with-temp-file "bar/foo.txt")
        (with-temp-file "bar/abc.txt")
        (with-current-buffer (find-file-noselect ".projectile" t)
          (let ((grep-find-template "<X>")
                grep-find-ignored-directories grep-find-ignored-files
                projectile-globally-ignored-files
                projectile-globally-ignored-file-suffixes
                projectile-globally-ignored-directories)
            (projectile-grep "hi")))
        (expect 'compilation-start :to-have-been-called-with
                (concat "\\( -path ./baz -o -path ./foo.txt -o -path ./bar/foo.txt \\) -prune -o "
                        "\\( "
                        "\\( -path \\*.txt -o -path \\*.text \\) "
                        "-a \\! \\( -path ./abc.txt -o -path ./bar/abc.txt -o -path \\*/def.txt \\) "
                        "\\) -prune -o ")
                'grep-mode)))))
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

(describe "projectile-switch-project-by-name"
  (it "calls the switch project action with project-to-switch's dir-locals loaded"
    (defvar switch-project-foo)
    (let ((foo 'bar)
          (switch-project-foo)
          (safe-local-variable-values '((foo . baz)))
          (projectile-switch-project-action (lambda () (setq switch-project-foo foo))))
      (projectile-test-with-sandbox
        (projectile-test-with-files
            ("project/"
             "project/.dir-locals.el"
             "project/.projectile")
          (append-to-file
           "((nil . ((foo . baz))))" nil "project/.dir-locals.el")
          (projectile-add-known-project (file-name-as-directory (expand-file-name "project")))
          (projectile-switch-project-by-name (file-name-as-directory (expand-file-name "project")))

          (expect switch-project-foo :to-be 'baz)))))

  (it "runs hooks from the project root directory"
      (defvar hook-dir)
      (let ((projectile-switch-project-action
             (lambda () (switch-to-buffer (find-file-noselect "file" t))))
            (hook (lambda () (setq hook-dir default-directory))))
        (add-hook 'projectile-after-switch-project-hook hook)
        (projectile-test-with-sandbox
         (projectile-test-with-files
          ("project/"
           "project/file")
          (let ((project-dir (file-name-as-directory (expand-file-name "project"))))
            (projectile-add-known-project project-dir)
            (projectile-switch-project-by-name project-dir)
            (remove-hook 'projectile-after-switch-project-hook hook)

            (expect hook-dir :to-equal project-dir))))))

  (it "ensures the buffer is switched immediately"
      (let ((projectile-switch-project-action
             (lambda () (switch-to-buffer (find-file-noselect "file" t)))))
        (projectile-test-with-sandbox
         (projectile-test-with-files
          ("project/"
           "project/file")
          (projectile-add-known-project (file-name-as-directory (expand-file-name "project")))
          (projectile-switch-project-by-name (file-name-as-directory (expand-file-name "project")))

          (expect (current-buffer) :to-be (get-file-buffer "project/file")))))))

(describe "projectile-ignored-buffer-p"
  (it "checks if buffer should be ignored"
    (let ((projectile-globally-ignored-buffers '("*nrepl messages*" "*something*")))
      (expect (projectile-ignored-buffer-p (get-buffer-create "*nrepl messages*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "*something*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "test")) :not :to-be-truthy))))

(describe "projectile-get-other-files"
  (it "returns files with same names but different extensions"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/test1.c"
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
           "include2/test2.js")
          ()
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
                                             ("js" . ("js")))))
          (expect (projectile-get-other-files "src/test1.c") :to-equal '("include1/test1.h" "include2/test1.h"))
          (expect (projectile-get-other-files "src/test1.cpp") :to-equal '("include1/test1.h" "include2/test1.h" "include1/test1.hpp"))
          (expect (projectile-get-other-files "test2.c") :to-equal '("include1/test2.h" "include2/test2.h"))
          (expect (projectile-get-other-files "test2.cpp") :to-equal '("include1/test2.h" "include2/test2.h" "include2/test2.hpp"))
          (expect (projectile-get-other-files "test1.h") :to-equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp"))
          (expect (projectile-get-other-files "test2.h") :to-equal '("src/test2.c" "src/test2.cpp" "include2/test2.hpp"))
          (expect (projectile-get-other-files "include1/test1.h" t) :to-equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp"))
          (expect (projectile-get-other-files "Makefile.lock") :to-equal '("src/Makefile"))
          (expect (projectile-get-other-files "include2/some_module/same_name.h") :to-equal '("src/some_module/same_name.c" "src/same_name.c"))
          ;; nested extensions
          (expect (projectile-get-other-files "src/test1.service.js") :to-equal '("include1/test1.service.spec.js" "include2/test1.service.spec.js"))
          ;; fallback to outer extensions if no rule for nested extension defined
          (expect (projectile-get-other-files "src/test2.service.spec.js") :to-equal '("include1/test2.js" "include2/test2.js"))
          (expect (projectile-get-other-files "src/test+copying.m") :to-equal '("include1/test+copying.h"))))))

  (it "returns files based on the paths returned by :related-files-fn option"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/test1.cpp"
           "src/test1.def"
           "src/test2.def"
           "src/test2.cpp"
           "src/test2.h"
           "src/test3.cpp"
           "src/test3.h")
          (:related-files-fn (lambda (file)
                           (cond ((equal file "src/test1.def") '(:other "src/test1.cpp"))
                                 ((equal file "src/test2.def") '(:other ("src/test2.cpp" "src/test2.h" "src/test4.h")))
                                 ((equal file "src/test3.cpp") '(:other nil)))))
        (expect (projectile-get-other-files "src/test1.def") :to-equal '("src/test1.cpp"))
        (expect (projectile-get-other-files "src/test2.def") :to-equal '("src/test2.cpp" "src/test2.h"))
        ;; Make sure extension based mechanism is still working
        (expect (projectile-get-other-files "src/test2.cpp") :to-equal '("src/test2.h"))
        ;; Make sure that related-files-fn option has priority over existing mechanism
        (expect (projectile-get-other-files "src/test3.cpp") :to-equal nil))))

  (it "returns files based on the predicate returned by :related-files-fn option"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/test1.cpp"
           "src/test1.def"
           "src/test2.def"
           "src/test2.cpp"
           "src/test2.h"
           "src/test3.cpp"
           "src/test3.h")
          (:related-files-fn
           (lambda (file)
             (cond ((equal file "src/test1.def")
                    (list :other (lambda (other-file)
                                   (equal other-file "src/test1.cpp"))))
                   ((equal file "src/test2.def")
                    (list :other (lambda (other-file)
                                   (or (equal other-file "src/test2.cpp")
                                       (equal other-file "src/test2.h")))))
                   ((equal file "src/test3.cpp")
                    (list :other (lambda (other-file) nil))))))

        (expect (projectile-get-other-files "src/test1.def") :to-equal '("src/test1.cpp"))
        (expect (projectile-get-other-files "src/test2.def") :to-equal '("src/test2.cpp" "src/test2.h"))
        ;; Make sure extension based mechanism is still working
        (expect (projectile-get-other-files "src/test2.cpp") :to-equal '("src/test2.h"))
        ;; Make sure that related-files-fn option has priority over existing mechanism
        (expect (projectile-get-other-files "src/test3.cpp") :to-equal nil)))))

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
        (expect (projectile-detect-project-type) :to-equal 'rails-rspec)))))
  (it "detects project-type for elisp eldev projects"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/Eldev"
       "project/project.el")
      (let ((projectile-indexing-method 'native))
        (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
        (expect (projectile-detect-project-type) :to-equal 'emacs-eldev))))))

(describe "projectile-dirname-matching-count"
  (it "counts matching dirnames ascending file paths"
    (expect (projectile-dirname-matching-count "src/food/sea.c" "src/food/cat.c") :to-equal 2)
    (expect (projectile-dirname-matching-count "src/weed/sea.c" "src/food/sea.c") :to-equal 0)
    (expect (projectile-dirname-matching-count "test/demo-test.el" "demo.el") :to-equal 0)))

(describe "projectile--find-matching-test"
  (it "finds matching test or file"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("app/models/weed/sea.rb"
           "app/models/food/sea.rb"
           "spec/models/weed/sea_spec.rb"
           "spec/models/food/sea_spec.rb")
          (:test-suffix "_spec")
        (expect (projectile--find-matching-test "app/models/food/sea.rb") :to-equal '("spec/models/food/sea_spec.rb"))
        (expect (projectile--find-matching-file "spec/models/food/sea_spec.rb") :to-equal '("app/models/food/sea.rb")))))

  (it "finds matching test or file with dirs"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("source/foo/foo.service.js"
           "source/bar/bar.service.js"
           "spec/foo/foo.service.spec.js"
           "spec/bar/bar.service.spec.js")
          (:test-suffix ".spec" :test-dir "spec/" :src-dir "source/")
        (expect (projectile--find-matching-test
                 "project/source/foo/foo.service.js")
                :to-equal '("spec/foo/foo.service.spec.js"))
        (expect (projectile--find-matching-file
                 "project/spec/bar/bar.service.spec.js")
                :to-equal '("source/bar/bar.service.js")))))

  (it "finds matching test with dirs and inexistent test file"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("project/src/main/scala/bar/package.scala"
           "project/src/main/scala/foo/package.scala"
           "project/src/test/scala/foo/packageSpec.scala")
          (:test-suffix "Spec" :test-dir "test" :src-dir "main")
        (expect (projectile--find-matching-test
                 "project/src/main/scala/bar/package.scala")
                :to-equal '("src/test/scala/bar/packageSpec.scala")))))

  (it "finds matching test or file based on the paths returned by :related-files-fn option"
    (defun -my/related-files(file)
      (if (string-match (rx (group (or "src" "test")) (group "/" (1+ anything) ".cpp")) file)
          (if (equal (match-string 1 file ) "test")
              (list :impl (concat "src" (match-string 2 file)))
            (list :test (concat "test" (match-string 2 file))))))
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/Foo.cpp"
           "src/Bar.cpp"
           "src/Baz.py"
           "test/Bar.cpp"
           "test/Foo.cpp"
           "other/Test_Baz.py")
          (:related-files-fn #'-my/related-files :test-prefix "Test_")
        (expect (projectile-test-file-p "test/Foo.cpp") :to-equal t)
        (expect (projectile-test-file-p "src/Foo.cpp") :to-equal nil)
        (expect (projectile--find-matching-test "src/Foo.cpp") :to-equal '("test/Foo.cpp"))
        (expect (projectile--find-matching-test "src/Foo2.cpp") :to-equal nil)
        (expect (projectile--find-matching-file "test/Foo.cpp") :to-equal '("src/Foo.cpp"))
        (expect (projectile--find-matching-file "test/Foo2.cpp") :to-equal nil)
        ;; Make sure that existing mechanism(:test-prefix) still works
        (expect (projectile-test-file-p "other/Test_Baz.py") :to-equal t)
        (expect (projectile-test-file-p "other/Baz.py") :to-equal nil)
        (expect (projectile--find-matching-file "other/Test_Baz.py") :to-equal '("src/Baz.py"))
        (expect (projectile--find-matching-test "src/Baz.py") :to-equal '("other/Test_Baz.py")))))

  (it "finds matching test or file by the predicate returned by :related-files-fn option"
    (defun -my/related-files(file)
      (cond ((equal file "src/Foo.cpp")
             (list :test (lambda (other-file)
                      (equal other-file "test/Foo.cpp"))))
            ((equal file "test/Foo.cpp")
             (list :impl (lambda (other-file)
                      (equal other-file "src/Foo.cpp"))))))
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/Foo.cpp"
           "src/Bar.cpp"
           "test/Bar.cpp"
           "test/Foo.cpp")
          (:related-files-fn #'-my/related-files)
        (expect (projectile-test-file-p "test/Foo.cpp") :to-equal t)
        (expect (projectile-test-file-p "src/Foo.cpp") :to-equal nil)
        (expect (projectile--find-matching-test "src/Foo.cpp") :to-equal '("test/Foo.cpp"))
        (expect (projectile--find-matching-test "src/Foo.cpp") :to-equal '("test/Foo.cpp"))
        (expect (projectile--find-matching-file "test/Foo.cpp") :to-equal '("src/Foo.cpp")))))

  (it "defers to test-dir property when it's set to a function"
    (projectile-test-with-sandbox
     (projectile-test-with-files-using-custom-project
          ("src/foo/Foo.cpp"
           "src/bar/Foo.cpp"
           "test/foo/FooTest.cpp")
          (:test-dir
           (lambda (file-path)
             (projectile-complementary-dir file-path "src" "test"))
           :test-suffix "Test")
          (expect (projectile--find-matching-test
                   (projectile-expand-root "src/bar/Foo.cpp"))
                  :to-equal
                  (list "test/bar/FooTest.cpp")))))

  (it "defers to src-dir property when it's set to a function"
    (projectile-test-with-sandbox
     (projectile-test-with-files-using-custom-project
          ("src/foo/Foo.cpp"
           "src/bar/Foo.cpp"
           "test/foo/FooTest.cpp")
          (:src-dir
           (lambda (file-path)
             (projectile-complementary-dir file-path "test" "src"))
           :test-suffix "Test")
          (expect (projectile--find-matching-file
                   (projectile-expand-root "test/foo/FooTest.cpp"))
                  :to-equal
                  (list "src/foo/Foo.cpp")))))

  (it "defers to a fallback using \"src\" and \"test\""
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("project.clj"
           "src/example/core.clj"
           "test/example/core2_test.clj")
          (:test-suffix "_test")
        (expect (projectile--find-matching-test
                 (projectile-expand-root "src/example/core.clj"))
                :to-equal
                (list "test/example/core_test.clj"))
        (expect (projectile--find-matching-file
                 (projectile-expand-root "test/example/core2_test.clj"))
                :to-equal
                (list "src/example/core2.clj"))))))

(describe "projectile--related-files"
  (it "returns related files for the given file"
    (defun -my/related-files(file)
      (cond ((equal file "src/Foo.c")
             (list :test "src/TestFoo.c" :doc "doc/Foo.txt"))
            ((equal file "src/TestFoo.c")
             (list :impl (lambda (other-file)
                           (equal other-file "src/Foo.c"))))))
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/Foo.c"
           "src/TestFoo.c"
           "doc/Foo.txt")
          (:related-files-fn #'-my/related-files)
        (expect (projectile--related-files-kinds "src/Foo.c") :to-equal '(:test :doc))
        (expect (projectile--related-files-kinds "src/TestFoo.c") :to-equal '(:impl))
        (expect (projectile--related-files "src/TestFoo.c" :impl) :to-equal '("src/Foo.c"))
        (expect (projectile--related-files "src/Foo.c" :doc) :to-equal '("doc/Foo.txt"))
        ;; Support abspath
        (expect (projectile--related-files-kinds (concat (projectile-project-root) "src/Foo.c")) :to-equal '(:test :doc))
        (expect (projectile--related-files (concat (projectile-project-root) "src/Foo.c") :doc) :to-equal '("doc/Foo.txt"))))))

(describe "projectile--merge-related-files-fns"
  (it "returns a new function which returns the merged plist from each fn"
    (defun -first-fn(file)
      (list :foo "file1"))
    (defun -second-fn(file)
      (list :foo (list "file2" "file3")))
    (defun -third-fn(file)
      (list :bar "file4"))
    (let ((fn (projectile--merge-related-files-fns '(-first-fn -second-fn))))
      (expect (funcall fn "something") :to-equal '(:foo ("file1" "file2" "file3"))))
    (let ((fn (projectile--merge-related-files-fns '(-first-fn -third-fn))))
      (expect (funcall fn "something") :to-equal '(:foo ("file1") :bar ("file4"))))))

(describe "projectile-related-files-fn-groups"
  (it "generate related files fn which relates members of each group as a specified kind"
    (let ((fn (projectile-related-files-fn-groups :foo '(("a.cpp" "req/a.txt" "doc/a.uml")
                                                         ("b.cpp" "req/b.txt")))))
      (expect (funcall fn "a.cpp") :to-equal '(:foo ("req/a.txt" "doc/a.uml")))
      (expect (funcall fn "req/a.txt") :to-equal '(:foo ("a.cpp" "doc/a.uml")))
      (expect (funcall fn "b.cpp") :to-equal '(:foo ("req/b.txt")))
      (expect (funcall fn "c.cpp") :to-equal nil))))

(describe "projectile-related-files-fn-extensions"
  (it "generate related files fn which relates files with the given extnsions"
    (let* ((fn (projectile-related-files-fn-extensions :foo '("cpp" "h" "hpp")))
           (plist (funcall fn "a.cpp"))
           (predicate (plist-get plist :foo)))
      (expect plist :to-contain :foo)
      (expect (funcall predicate "a.h") :to-equal t)
      (expect (funcall predicate "a.hpp") :to-equal t)
      (expect (funcall predicate "b.cpp") :to-equal nil)
      (expect (funcall predicate "a.cpp") :to-equal nil))))

(describe "projectile-related-files-fn-tests-with-prefix"
  (it "generate related files fn which relates tests and impl based on extension and prefix"
    (let ((fn (projectile-related-files-fn-test-with-prefix "py" "test_")))
      (let* ((plist (funcall fn "foo/a.py"))
            (predicate (plist-get plist :test)))
        (expect plist :to-contain :test)
        (expect (funcall predicate "bar/test_a.py") :to-equal t)
        (expect (funcall predicate "bar/test_a.cpp") :to-equal nil))
      (let* ((plist (funcall fn "foo/test_a.py"))
             (predicate (plist-get plist :impl)))
        (expect plist :to-contain :impl)
        (expect (funcall predicate "bar/a.py") :to-equal t)
        (expect (funcall predicate "bar/a.cpp") :to-equal nil)
        (expect (funcall predicate "bar/test_a.cpp") :to-equal nil)))))

(describe "projectile-related-files-fn-tests-with-suffix"
  (it "generate related files fn which relates tests and impl based on extension and suffix"
    (let ((fn (projectile-related-files-fn-test-with-suffix "py" "-test")))
      (let* ((plist (funcall fn "foo/a.py"))
            (predicate (plist-get plist :test)))
        (expect plist :to-contain :test)
        (expect (funcall predicate "bar/a-test.py") :to-equal t)
        (expect (funcall predicate "bar/a-test.cpp") :to-equal nil))
      (let* ((plist (funcall fn "foo/a-test.py"))
             (predicate (plist-get plist :impl)))
        (expect plist :to-contain :impl)
        (expect (funcall predicate "bar/a.py") :to-equal t)
        (expect (funcall predicate "bar/a.cpp") :to-equal nil)
        (expect (funcall predicate "bar/a-test.cpp") :to-equal nil)))))

(describe "projectile--related-files-plist-by-kind"
  (defun -sample-predicate (other-file)
    (equal other-file "src/foo.c"))
  (defun -sample-predicate2 (other-file)
    (equal other-file "src/bar.c"))
  (describe "when :related-files-fn returns paths"
    (it "returns a plist containing :paths only with the existing files on file system without duplication"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")
            (:related-files-fn (lambda (_)
                                 (list :foo '("src/foo.c" "src/bar.c" "src/foo.c"))))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:paths ("src/foo.c")))))))
  (describe "when :related-files-fn returns one predicate"
    (it "returns a plist containing :predicate with the same predicate"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")  ; Contents does not matter
            (:related-files-fn (lambda (_)
                                 (list :foo '-sample-predicate)))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:predicate -sample-predicate))))))
  (describe "when :related-files-fn returns multiple predicates"
    (it "returns a plist containing :predicate with a merging predicate"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")  ; Contents does not matter
            (:related-files-fn (lambda (_)
                                 (list :foo (list '-sample-predicate '-sample-predicate2))))
          (let* ((plist (projectile--related-files-plist-by-kind "something" :foo))
                 (predicate (plist-get plist :predicate)))
            (expect plist :to-contain :predicate)
            (expect (funcall predicate "src/foo.c") :to-equal t)
            (expect (funcall predicate "src/bar.c") :to-equal t))))))
  (describe "when :related-files-fn returns both paths and predicates"
    (it "returns a plist containing both :paths and :predicates"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")
            (:related-files-fn (lambda (_)
                                 (list :foo '("src/foo.c" -sample-predicate))))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:paths ("src/foo.c") :predicate -sample-predicate))))))
  (describe "when :related-files-fn is a list of functions"
    (it "returns a plist containing the merged results"
      (defun -sample-fn(file)
        (list :foo "src/foo.c"))
      (defun -sample-fn2(file)
        (list :foo '-sample-predicate))
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c"
             "src/bar.c")
            (:related-files-fn (list '-sample-fn '-sample-fn2))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:paths ("src/foo.c") :predicate -sample-predicate)))))))

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

(describe "projectile-get-all-sub-projects-files"
  (it "returns relative paths to submodule files"
    (spy-on 'projectile-get-all-sub-projects :and-return-value '("/a/b/x/"))
    (spy-on 'projectile-files-via-ext-command :and-return-value '("1.txt" "2.txt"))
    (expect (projectile-get-sub-projects-files "/a/b" 'git) :to-equal
      (list "x/1.txt" "x/2.txt"))))

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

(describe "projectile-reset-known-projects"
  (it "resets known projects"
    (spy-on 'projectile-clear-known-projects)
    (spy-on 'projectile-discover-projects-in-search-path)
    (projectile-reset-known-projects)
    (expect 'projectile-clear-known-projects :to-have-been-called)
    (expect 'projectile-discover-projects-in-search-path :to-have-been-called)))

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

(describe "projectile--directory-p"
  (it "tests which directory exists"
    (expect (projectile--directory-p nil) :to-be nil)
    (expect (projectile--directory-p "asdf") :to-be nil)
    (expect (projectile--directory-p user-emacs-directory) :to-be-truthy)))

(describe "projectile-find-file-in-directory"
  (it "fails when called in a non-existing directory"
    (expect (projectile-find-file-in-directory "asdf") :to-throw)))

(describe "projectile-dir-files-native"
  (it "calculates ignored files and directories only once during recursion"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/"
       "projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/.git/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/src/framework.conf"
       "projectA/src/html/index.html"
       "projectA/.projectile")

      ;; verify that indexing only invokes these funcs once during recursion
      (spy-on 'projectile-ignored-files :and-call-through)
      (spy-on 'projectile-ignored-directories :and-call-through)
      (spy-on 'projectile-globally-ignored-directory-names :and-call-through)

      (projectile-dir-files-native "projectA/")
      (expect 'projectile-ignored-files :to-have-been-called-times 1)
      (expect 'projectile-globally-ignored-directory-names :to-have-been-called-times 1)
      (expect 'projectile-ignored-directories :to-have-been-called-times 1))))
  (it "ignores globally ignored directories when using native indexing"
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/"
         "project/.ignoreme/"
         "project/.ignoreme/should_ignore"
         "project/src/"
         "project/src/.ignoreme/"
         "project/src/.ignoreme/should_ignore"
         "project/config.conf")

        (setq projectile-globally-ignored-directories '(".ignoreme"))
        (expect (projectile-dir-files-native "project") :to-equal '("config.conf"))))))

(describe "projectile-process-current-project-buffers-current"
  (it "expects projectile-process-current-project-buffers and
projectile-process-current-project-buffers-current to have similar behaviour"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/"
       "projectA/.projectile"
       "projectA/bufferA"
       "projectA/fileA"
       "projectA/dirA/"
       "projectA/dirA/fileC")
      (let ((list-a '())
            (list-b '()))
        (projectile-process-current-project-buffers (lambda (b) (push b list-a)))
        (projectile-process-current-project-buffers-current (lambda () (push (current-buffer) list-b)))
        (expect list-a :to-equal list-b))))))

(describe "projectile-project-buffers"
          (it "return project buffers"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("project1/"
                 "project1/.projectile"
                 "project1/foo")
                (cd "project1")
                (with-current-buffer (find-file-noselect "foo" t))
                (expect (length (projectile-project-buffers)) :to-equal 1)))))

(describe "projectile--impl-name-for-test-name"
  :var ((mock-projectile-project-types
         '((foo test-suffix "Test")
           (bar test-prefix "Test"))))
  (it "removes suffix from test file"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda () 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-name-for-test-name "FooTest.cpp")
              :to-equal
              "Foo.cpp")))
  (it "removes prefix from test file"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda () 'bar))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-name-for-test-name "TestFoo.cpp")
              :to-equal
              "Foo.cpp"))))

(describe "projectile-find-implementation-or-test"
  (it "error when test file does not exist and projectile-create-missing-test-files is nil"
    (cl-letf (((symbol-function 'projectile-test-file-p) #'ignore)
              ((symbol-function 'file-exists-p) #'ignore)
              ((symbol-function 'projectile-expand-root) #'identity)
              ((symbol-function 'projectile-find-matching-test) (lambda (file) "dir/foo"))
              (projectile-create-missing-test-files nil)
              (projectile-project-type 'foo))
      (expect (projectile-find-implementation-or-test "foo") :to-throw))))

(describe "projectile--impl-file-from-src-dir-fn"
  :var ((mock-projectile-project-types
         '((foo src-dir (lambda (impl-file) "/outer/foo/test/dir"))
           (bar src-dir "not a function"))))
  (it "returns result of projectile--complementary-file when src-dir property is a function"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              ((symbol-function 'projectile-project-root) (lambda () "foo"))
              ((symbol-function 'file-relative-name) (lambda (f rel) f))
              ((symbol-function 'file-exists-p) (lambda (file) t))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "foo") :to-equal "/outer/foo/test/dir")))
  (it "returns file relative to project root"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              ((symbol-function 'projectile-project-root) (lambda () "/outer/foo"))
              ((symbol-function 'file-exists-p) (lambda (file) t))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "/outer/foo/bar")
              :to-equal
              "test/dir")))
  (it "returns nil when src-dir property is a not function"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda () 'bar))
              ((symbol-function 'projectile-project-root) (lambda () "foo"))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "bar") :to-equal nil)))
  (it "returns nil when src-dir function result is not an existing file"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              ((symbol-function 'projectile-project-root) (lambda () "/outer/foo"))
              ((symbol-function 'file-exists-p) #'ignore)
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "bar") :to-equal nil))))

(describe "projectile--test-file-from-test-dir-fn"
  :var ((mock-projectile-project-types
         '((foo test-dir (lambda (impl-file) "/outer/foo/test/dir"))
           (bar test-dir "not a function"))))
  (it "returns result of projectile--complementary-file when test-dir property is a function"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              ((symbol-function 'projectile-project-root) (lambda () "foo"))
              ((symbol-function 'file-relative-name) (lambda (f rel) f))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-file-from-test-dir-fn "foo") :to-equal "/outer/foo/test/dir")))
  (it "returns file relative to project root"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda () 'foo))
              ((symbol-function 'projectile-project-root) (lambda () "/outer/foo"))
              ((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-file-from-test-dir-fn "/outer/foo/bar")
              :to-equal
              "test/dir")))
  (it "returns nil when test-dir property is a not function"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda () 'bar))
              (projectile-project-types mock-projectile-project-types)
              ((symbol-function 'projectile-project-root) (lambda () "foo")))
      (expect (projectile--test-file-from-test-dir-fn "bar") :to-equal nil))))

(describe "projectile--complementary-file"
  (it "dir-fn and filename-fn applied correctly"
    (cl-letf (((symbol-function 'file-exists-p) (lambda (file) t))
              ((symbol-function 'dir-fn) (lambda (dir) "foo/test/dir"))
              ((symbol-function 'filename-fn) (lambda (filename) "Foo.test")))
      (expect (projectile--complementary-file
               "foo/src/dir/Foo.impl"
               #'dir-fn
               #'filename-fn)
              :to-equal "foo/test/dir/Foo.test"))))

(describe "projectile--impl-to-test-dir"
  :var ((mock-projectile-project-types
         '((foo test-dir "test" src-dir "src")
           (bar test-dir identity src-dir "src"))))
  (it "replaces occurrences of src-dir with test-dir"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda () "foo"))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-to-test-dir "/foo/src/Foo") :to-equal "/foo/test/")))
  (it "nil returned when test-dir property is not a string"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda () "bar"))
              ((symbol-function 'projectile-project-type) (lambda () 'bar))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-to-test-dir "/bar/src/bar") :to-be nil)))
  (it "error when src-dir not a substring of impl file"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda () "foo"))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-to-test-dir "/bar/other/bar") :to-throw))))

(describe "projectile--test-to-impl-dir"
  :var ((mock-projectile-project-types
         '((foo test-dir "test" src-dir "src")
           (bar test-dir "test" src-dir identity))))
  (it "replaces occurrences of test-dir with src-dir"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda () "foo"))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-to-impl-dir "/foo/test/Foo") :to-equal "/foo/src/")))
  (it "nil returned when src-dir property is not a string"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda () "bar"))
              ((symbol-function 'projectile-project-type) (lambda () 'bar))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-to-impl-dir "/bar/test/bar") :to-be nil)))
  (it "error when test-dir not a substring of test file"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda () "foo"))
              ((symbol-function 'projectile-project-type) (lambda () 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-to-impl-dir "/bar/other/bar") :to-throw))))

(describe "projectile-run-shell-command-in-root"
  (describe "when called directly in elisp"
    (before-each (spy-on 'shell-command))
    (describe "when called with all three parameters"
      (it "expects to call shell-command with the same parameters"
        (projectile-run-shell-command-in-root "cmd" "output-buffer" "error-buffer")
        (expect 'shell-command :to-have-been-called-with "cmd" "output-buffer" "error-buffer")))
    (describe "when called with only one optional parameter"
      (it "expects to call shell-command with the same parameters"
        (projectile-run-shell-command-in-root "cmd" "output-buffer")
        (expect 'shell-command :to-have-been-called-with "cmd" "output-buffer" nil)))
    (describe "when called with no optional parameters"
      (it "expects to call shell-command with the same parameters"
        (projectile-run-shell-command-in-root "cmd")
        (expect 'shell-command :to-have-been-called-with "cmd" nil nil))))
  (describe "when called interactively"
    (before-each (spy-on 'shell-command))
    (it "expects to be interactive"
      (expect (interactive-form 'projectile-run-shell-command-in-root) :not :to-be nil))
    (it "expects to call shell-command with the given command"
      (funcall-interactively 'projectile-run-shell-command-in-root "cmd")
      (expect 'shell-command :to-have-been-called-with "cmd" nil nil))))

(describe "projectile-run-async-shell-command-in-root"
  (describe "when called directly in elisp"
    (before-each (spy-on 'async-shell-command))
    (describe "when called with all three parameters"
      (it "expects to call async-shell-command with the same parameters"
        (projectile-run-async-shell-command-in-root "cmd" "output-buffer" "error-buffer")
        (expect 'async-shell-command :to-have-been-called-with "cmd" "output-buffer" "error-buffer")))
    (describe "when called with only one optional parameter"
      (it "expects to call async-shell-command with the same parameters"
        (projectile-run-async-shell-command-in-root "cmd" "output-buffer")
        (expect 'async-shell-command :to-have-been-called-with "cmd" "output-buffer" nil)))
    (describe "when called with no optional parameters"
      (it "expects to call async-shell-command with the same parameters"
        (projectile-run-async-shell-command-in-root "cmd")
        (expect 'async-shell-command :to-have-been-called-with "cmd" nil nil))))
  (describe "when called interactively"
    (before-each (spy-on 'async-shell-command))
    (it "expects to be interactive"
      (expect (interactive-form 'projectile-run-async-shell-command-in-root) :not :to-be nil))
    (it "expects to call async-shell-command with the given command"
      (funcall-interactively 'projectile-run-async-shell-command-in-root "cmd")
      (expect 'async-shell-command :to-have-been-called-with "cmd" nil nil))))

(describe "projectile--run-project-cmd"
  (it "command history is not duplicated"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-maybe-read-command :and-call-fake
            (lambda (arg default-cmd prompt) default-cmd))
    ;; Stops projectile--run-project-cmd from creating a new directory for
    ;; the compilation dir
    (spy-on 'file-directory-p :and-return-value t)
    (let ((command-map (make-hash-table :test 'equal))
          ;; history is based on the project root, so we set it to a random
          ;; path to ensure there are no existing commands in history
          (projectile-project-root "/a/random/path"))
      (projectile--run-project-cmd "foo" command-map)
      (projectile--run-project-cmd "foo" command-map)
      (projectile--run-project-cmd "foo" command-map)
      (projectile--run-project-cmd "bar" command-map)
      (expect 'projectile-run-compilation :to-have-been-called-times 4)
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root))
              :to-equal '("bar" "foo")))))

;; A bunch of tests that make sure Projectile commands handle
;; gracefully the case of being run outside of a project.
(assert-friendly-error-when-no-project projectile-project-info)
(assert-friendly-error-when-no-project projectile-display-buffer)
(assert-friendly-error-when-no-project projectile-find-implementation-or-test-other-frame)
(assert-friendly-error-when-no-project projectile-find-implementation-or-test-other-window)
(assert-friendly-error-when-no-project projectile-find-other-file)
(assert-friendly-error-when-no-project projectile-find-other-file-other-frame)
(assert-friendly-error-when-no-project projectile-find-other-file-other-window)
(assert-friendly-error-when-no-project projectile-find-test-file)
(assert-friendly-error-when-no-project projectile-grep)
(assert-friendly-error-when-no-project projectile-ibuffer)
(assert-friendly-error-when-no-project projectile-project-buffers-other-buffer)
(assert-friendly-error-when-no-project projectile-project-info)
(assert-friendly-error-when-no-project projectile-regenerate-tags)
(assert-friendly-error-when-no-project projectile-remove-current-project-from-known-projects)
(assert-friendly-error-when-no-project projectile-switch-to-buffer)
(assert-friendly-error-when-no-project projectile-switch-to-buffer-other-frame)
(assert-friendly-error-when-no-project projectile-switch-to-buffer-other-window)
