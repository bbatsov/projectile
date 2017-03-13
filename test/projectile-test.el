(require 'projectile)
(require 'dash)
(require 'ert)
(require 'noflet)

(defmacro projectile-test-with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  (declare (indent 0) (debug (&rest form)))
  `(let ((sandbox
          (--if-let (bound-and-true-p projectile-test-path)
              (file-name-as-directory (expand-file-name "sandbox" it))
            (expand-file-name
             (convert-standard-filename "test/sandbox/")
             (file-name-directory (locate-library "projectile.el" t))))))
     (when (file-directory-p sandbox)
       (delete-directory sandbox t))
     (make-directory sandbox t)
     (let ((default-directory sandbox))
       ,@body)))

(defmacro projectile-test-with-files (files &rest body)
  (declare (indent 1) (debug (sexp &rest form)))
  `(progn ,@(mapcar (lambda (file)
                      (if (string-suffix-p "/" file)
                          `(make-directory ,file t)
                        `(with-temp-file ,file)))
                    files)
          ,@body))

(defun projectile-test-should-root-in (root directory)
  (let ((projectile-project-root-cache (make-hash-table :test 'equal)))
    (should (equal (file-truename (file-name-as-directory root))
                   (let ((default-directory
                           (expand-file-name
                            (file-name-as-directory directory))))
                     (file-truename (projectile-project-root)))))))

(ert-deftest projectile-test-project-get-name ()
  (noflet ((projectile-project-name () "project"))
    (should (equal (projectile-project-name) "project"))))

(ert-deftest projectile-test-prepend-project-name ()
  (noflet ((projectile-project-name () "project"))
    (should (equal (projectile-prepend-project-name "Test") "[project] Test"))))

(ert-deftest projectile-test-expand-root ()
  (noflet ((projectile-project-root () "/path/to/project"))
    (should (equal (projectile-expand-root "foo") "/path/to/project/foo"))
    (should (equal (projectile-expand-root "foo/bar") "/path/to/project/foo/bar"))
    (should (equal (projectile-expand-root "./foo/bar") "/path/to/project/foo/bar"))))

(ert-deftest projectile-test-ignored-directory-p ()
  (noflet ((projectile-ignored-directories () '("/path/to/project/tmp")))
    (should (projectile-ignored-directory-p "/path/to/project/tmp"))
    (should-not (projectile-ignored-directory-p "/path/to/project/log"))))

(ert-deftest projectile-test-ignored-file-p ()
  (noflet ((projectile-ignored-files () '("/path/to/project/TAGS")))
    (should (projectile-ignored-file-p "/path/to/project/TAGS"))
    (should-not (projectile-ignored-file-p "/path/to/project/foo.el"))))

(ert-deftest projectile-test-ignored-files ()
  (noflet ((projectile-project-root () "/path/to/project")
           (projectile-project-name () "project")
           (projectile-project-ignored-files () '("foo.js" "bar.rb")))
    (let ((expected '("/path/to/project/TAGS"
                      "/path/to/project/foo.js"
                      "/path/to/project/bar.rb"))
          (projectile-ignored-files '("TAGS")))
      (should (equal (projectile-ignored-files) expected)))))

(ert-deftest projectile-test-ignored-directories ()
  (noflet ((projectile-project-ignored-directories () '("tmp" "log"))
           (projectile-project-root () "/path/to/project"))
    (let ((expected '("/path/to/project/compiled/"
                      "/path/to/project/tmp/"
                      "/path/to/project/log/"))
          (projectile-globally-ignored-directories '("compiled")))
      (should (equal (projectile-ignored-directories) expected)))))

(ert-deftest projectile-test-project-ignored-files ()
  (let ((files '("/path/to/project/foo.el" "/path/to/project/foo.elc")))
    (noflet ((projectile-project-ignored () files))
      (noflet ((file-directory-p (filename) nil))
        (should (equal (projectile-project-ignored-files) files)))
      (noflet ((file-directory-p (filename) t))
        (should-not (projectile-project-ignored-files))))))

(ert-deftest projectile-test-project-ignored-directories ()
  (let ((directories '("/path/to/project/tmp" "/path/to/project/log")))
    (noflet ((projectile-project-ignored () directories))
      (noflet ((file-directory-p (filename) t))
        (should (equal (projectile-project-ignored-directories) directories)))
      (noflet ((file-directory-p (filename) nil))
        (should-not (projectile-project-ignored-directories))))))

(ert-deftest projectile-test-project-ignored ()
  (noflet ((projectile-project-root () "/path/to/project")
           (projectile-project-name () "project"))
    (let* ((file-names '("log" "tmp" "compiled"))
           (files (mapcar 'projectile-expand-root file-names)))
      (noflet ((projectile-paths-to-ignore () (list "log" "tmp" "compiled"))
               (file-expand-wildcards (pattern ignored)
                                      (cond
                                       ((string-equal pattern "log")
                                        "/path/to/project/log")
                                       ((string-equal pattern "tmp")
                                        "/path/to/project/tmp")
                                       ((string-equal pattern "compiled")
                                        "/path/to/project/compiled"))))
        (should (equal (projectile-project-ignored) files))))))

(ert-deftest projectile-remove-ignored-suffixes ()
  (noflet ((projectile-project-root () "/path/to/project")
           (projectile-project-name () "project")
           (projectile-ignored-files-rel () ())
           (projectile-ignored-directories-rel () ()))
          (let* ((file-names '("foo.c" "foo.o" "foo.so" "foo.o.gz"))
                 (files (mapcar 'projectile-expand-root file-names)))
            (let ((projectile-globally-ignored-file-suffixes '(".o" ".so")))
              (should (equal (projectile-remove-ignored files)
                             (mapcar 'projectile-expand-root
                                     '("foo.c" "foo.o.gz"))))))))

(ert-deftest projectile-add-unignored-files ()
  (noflet ((projectile-get-repo-ignored-files () '("unignored-file"
                                                   "path/unignored-file2")))
    (let ((projectile-globally-unignored-files '("unignored-file")))
      (should (equal (projectile-add-unignored '("file"))
                     '("file" "unignored-file"))))
    ;; Files inside ignored paths need to be explicitely unignored
    (let ((projectile-globally-unignored-files '("unignored-file"
                                               "path/unignored-file2")))
      (should (equal (projectile-add-unignored '("file"))
                     '("file" "unignored-file" "path/unignored-file2"))))))

(ert-deftest projectile-add-unignored-files-no-vcs ()
  (noflet ((projectile-project-vcs () 'none))
    ;; on an unsupported VCS we simply return the list of globally unignored files
    (let ((projectile-globally-unignored-files '("unignored-file")))
      (should (equal (projectile-add-unignored '("file")) '("file"))))))

(ert-deftest projectile-add-unignored-directories ()
  (noflet ((projectile-project-vcs () 'git)
           (projectile-get-repo-ignored-files () '("path/unignored-file")))
    (let ((projectile-globally-unignored-directories '("path")))
      (should (equal (projectile-add-unignored '("file"))
                     '("file" "path/unignored-file")))
      ;; Ignored files inside unignored paths need to be explicitely
      ;; unignored
      (let ((projectile-globally-ignored-files '("unignored-file")))
        (should (equal (projectile-add-unignored '("file"))
                       '("file")))
        (let ((projectile-globally-unignored-files '("path/unignored-file")))
          (should (equal (projectile-add-unignored '("file"))
                       '("file" "path/unignored-file"))))))))

(ert-deftest projectile-test-parse-dirconfig-file ()
  (noflet ((file-exists-p (filename) t)
           (file-truename (filename) filename)
           (insert-file-contents
            (filename)
            (save-excursion
              (insert
               "\n-exclude\n+include\nno-prefix\n left-wspace\nright-wspace\t\n"))))
    (should (equal '(("include/")
                     ("exclude" "no-prefix" "left-wspace" "right-wspace")
                     nil)
                   (projectile-parse-dirconfig-file)))))

(ert-deftest projectile-test-get-project-directories ()
  (noflet ((projectile-project-root () "/my/root/")
           (projectile-parse-dirconfig-file () '(nil)))
    (should (equal '("/my/root/") (projectile-get-project-directories)))
    (noflet ((projectile-parse-dirconfig-file () '(("foo" "bar/baz"))))
      (should (equal '("/my/root/foo" "/my/root/bar/baz")
                     (projectile-get-project-directories))))))

(ert-deftest projectile-test-file-truename ()
  (should (equal nil (projectile-file-truename nil)))
  (should (equal (file-truename "test") (projectile-file-truename "test"))))

(ert-deftest projectile-test-dir-files ()
  (noflet ((projectile-project-root () "/my/root/")
           (projectile-patterns-to-ignore () nil)
           (projectile-index-directory (dir patterns progress-reporter)
                                       (should (equal dir "a/"))
                                       '("/my/root/a/b/c" "/my/root/a/d/e"))
           (projectile-get-repo-files () '("/my/root/a/b/c" "/my/root/a/d/e"))
           (cd (directory) "/my/root/a/" nil))
    (let ((projectile-indexing-method 'native))
      (should (equal '("a/b/c" "a/d/e") (projectile-dir-files "a/"))))
    (let ((projectile-indexing-method 'alien))
      (should (equal '("a/b/c" "a/d/e") (projectile-dir-files "a/"))))))

(ert-deftest projectile-test-setup-hook-functions-projectile-mode ()
  (projectile-mode 1)
  (should (memq 'projectile-find-file-hook-function find-file-hook))
  (projectile-mode -1)
  (should (not (memq 'projectile-find-file-hook-function find-file-hook))))

(ert-deftest projectile-test-relevant-known-projects ()
  (let ((projectile-known-projects '("/path/to/project1" "/path/to/project2")))
    (noflet ((projectile-project-root () "/path/to/project1"))
      (should (equal (projectile-relevant-known-projects) '("/path/to/project2"))))))

(ert-deftest projectile-test-projects-cleaned ()
  (let* ((projectile-known-projects-file (projectile-test-tmp-file-path))
         (directories (cl-loop repeat 3 collect (make-temp-file "projectile-cleanup" t)))
         (projectile-known-projects directories))
    (unwind-protect
        (progn
          (projectile-cleanup-known-projects)
          (should (equal projectile-known-projects directories))
          (delete-directory (car directories))
          (projectile-cleanup-known-projects)
          (should (equal projectile-known-projects (cdr directories))))
      (--each directories (ignore-errors (delete-directory it)))
      (delete-file projectile-known-projects-file nil))))

(ert-deftest projectile-test-project-root-is-absolute ()
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
            (should (file-name-absolute-p (projectile-project-root)))))
      (ignore-errors (delete-directory root-directory t)))))

(ert-deftest projectile-test-tags-exclude-items ()
  (noflet ((projectile-ignored-directories-rel () (list ".git/" ".hg/")))
    (should (equal (projectile-tags-exclude-patterns)
                   "--exclude=\".git\" --exclude=\".hg\""))))

(ert-deftest projectile-test-maybe-invalidate ()
  (noflet ((projectile-invalidate-cache (arg) t))
    (should-not (projectile-maybe-invalidate-cache nil))
    (should (projectile-maybe-invalidate-cache t))
    (noflet ((file-newer-than-file-p (a b) t))
      (should (projectile-maybe-invalidate-cache nil)))))

(ert-deftest projectile-test-root-top-down ()
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
      (should (equal (expand-file-name "projectA/src/")
                     (projectile-root-top-down "projectA/src/framework/lib"
                                               '("framework.conf" ".git"))))
      (should (equal (expand-file-name "projectA/src/")
                     (projectile-root-top-down "projectA/src/framework/lib"
                                               '(".git" "framework.conf"))))
      (should (equal (expand-file-name "projectA/src/html/")
                     (projectile-root-top-down "projectA/src/html/"
                                               '(".svn")))))))

(ert-deftest projectile-test-root-top-down-recurring ()
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
      (should (equal (expand-file-name "projectA/")
                     (projectile-root-top-down-recurring
                      "projectA/src/html/"
                      '("something" ".svn" ".git"))))
      (should (equal (expand-file-name "projectA/")
                     (projectile-root-top-down-recurring
                      "projectA/src/html/"
                      '(".git"))))
      (should-not (projectile-root-top-down-recurring
                   "projectA/src/html/"
                   '("elusivefile"))))))

(ert-deftest projectile-test-root-bottom-up ()
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
      (should (equal (expand-file-name "projectA/")
                     (projectile-root-bottom-up "projectA/src/framework/lib"
                                                '(".git" ".svn"))))
      (should (equal (expand-file-name "projectA/")
                     (projectile-root-bottom-up "projectA/src/html"
                                                '(".git" ".svn"))))
      (should (equal (expand-file-name "projectA/src/html/")
                     (projectile-root-bottom-up "projectA/src/html"
                                                '(".svn" ".git"))))
      (should (equal (expand-file-name "projectA/")
                     (projectile-root-bottom-up "projectA/src/html"
                                                '(".projectile" "index.html")))))))

(ert-deftest projectile-test-project-root ()
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
        (projectile-test-should-root-in "projectA/src/" "projectA/src/html")))))

(ert-deftest projectile-test-file-exists-cache-disabled ()
  (projectile-test-with-sandbox
    (projectile-test-with-files
        ("project/dirA/dirB/"
         "project/fileA")
      (let ((projectile-file-exists-local-cache-expire nil)
            (projectile-file-exists-remote-cache-expire nil))
        (should (projectile-file-exists-p "project/fileA"))
        (should (projectile-file-exists-p "project/dirA/dirB"))
        (should-not (projectile-file-exists-p "project/dirA/fileB"))
        (with-temp-file "project/dirA/fileB")
        (should(projectile-file-exists-p "project/dirA/fileB"))
        (should-not (projectile-file-exists-p "project/nofile"))
        (delete-file "project/fileA")
        (should-not (projectile-file-exists-p "project/fileA"))))))

(ert-deftest projectile-test-file-exists-cache ()
  (projectile-test-with-sandbox
    (projectile-test-with-files
        ("dirA/dirB/"
         "fileA")
      (let* ((initial-time (current-time))
             (projectile-file-exists-local-cache-expire 100)
             (projectile-file-exists-remote-cache-expire nil))

        (noflet ((run-with-timer (&rest args) 'nooptimer))
          (noflet ((current-time () initial-time))
            (should (projectile-file-exists-p "fileA"))
            (should (projectile-file-exists-p "dirA/dirB"))
            (should-not (projectile-file-exists-p "dirA/fileB"))
            (with-temp-file "dirA/fileB")
            (should-not (projectile-file-exists-p "dirA/fileB"))
            (delete-file "fileA")
            (should (projectile-file-exists-p "fileA"))
            (should (equal  projectile-file-exists-cache-timer 'nooptimer))
            (projectile-file-exists-cache-cleanup)
            (should (equal  projectile-file-exists-cache-timer 'nooptimer)))

          (noflet ((current-time () (time-add initial-time (seconds-to-time 50))))
            (projectile-file-exists-cache-cleanup)
            (should (projectile-file-exists-p "fileA"))
            (should-not (projectile-file-exists-p "dirA/fileB"))
            (should-not (projectile-file-exists-p "fileC"))
            (with-temp-file "fileC")
            (should (projectile-file-exists-p "fileA"))
            (projectile-file-exists-cache-cleanup)
            (should (equal projectile-file-exists-cache-timer 'nooptimer)))

          (noflet ((current-time () (time-add initial-time (seconds-to-time 120))))
            (projectile-file-exists-cache-cleanup)
            (should (projectile-file-exists-p "dirA/fileB"))
            (should-not (projectile-file-exists-p "fileA"))
            (should-not (projectile-file-exists-p "fileC"))
            (should (equal  projectile-file-exists-cache-timer 'nooptimer))
            (projectile-file-exists-cache-cleanup)
            (should (equal projectile-file-exists-cache-timer 'nooptimer)))

          (noflet ((current-time () (time-add initial-time (seconds-to-time 220))))
            (projectile-file-exists-cache-cleanup)
            (should (projectile-file-exists-p "fileC"))
            (should (equal  projectile-file-exists-cache-timer 'nooptimer))
            (projectile-file-exists-cache-cleanup)
            (should (equal projectile-file-exists-cache-timer 'nooptimer)))

          (noflet ((current-time () (time-add initial-time (seconds-to-time 1000))))
            (should (equal  projectile-file-exists-cache-timer 'nooptimer))
            (projectile-file-exists-cache-cleanup)
            (should-not projectile-file-exists-cache-timer)))))))

(ert-deftest projectile-test-cache-current-file ()
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
            (projectile-enable-caching t))
        (puthash (projectile-project-root)
                 '("file1.el")
                 projectile-projects-cache)
        (noflet ((projectile-project-root () (file-truename default-directory))
                 (projectile-project-vcs () 'none))
          (with-current-buffer (find-file-noselect  "file2.el" t)
            (projectile-cache-current-file)
            (dolist (f '("file1.el" "file2.el"))
              (should (member f (gethash (projectile-project-root)
                                         projectile-projects-cache)))))
          (with-current-buffer (find-file-noselect "file3.el" t)
            (projectile-cache-current-file)
            (dolist (f '("file1.el" "file2.el" "file3.el"))
              (should (member f (gethash (projectile-project-root)
                                         projectile-projects-cache)))))
          (with-current-buffer (find-file-noselect "file4.el" t)
            (projectile-cache-current-file)
            (dolist (f '("file1.el" "file2.el" "file3.el" "file4.el"))
              (should (member f (gethash (projectile-project-root)
                                         projectile-projects-cache))))))))))

(ert-deftest projectile-test-old-project-root-gone ()
  "Ensure that we don't cache a project root if the path has changed."
  (projectile-test-with-sandbox
    (projectile-test-with-files
        ("project/"
         "project/.projectile")
      (cd "project")
      (let* ((projectile-project-root-cache (make-hash-table :test #'equal))
             (correct-project-root (projectile-project-root)))
        ;; If this project has been moved, then we will have stale
        ;; paths in the cache.
        (puthash
         (format "projectile-root-bottom-up-%s" correct-project-root)
         "/this/path/does/not/exist"
         projectile-project-root-cache)
        (should (string= (projectile-project-root) correct-project-root))))))

(ert-deftest projectile-test-git-grep-prefix ()
  (require 'vc-git)
  (projectile-test-with-sandbox
    (projectile-test-with-files
	("project/c/src/"
	 "project/c/include/"
	 "project/go/src/package1/"
	 "project/.projectile")
      (cd "project")
      (with-temp-file "go/src/package1/x.go" (insert "foo(bar)"))
      (with-temp-file "c/include/x.h" (insert "typedef struct bar_t" ))
      (with-temp-file "c/src/x.c" (insert "struct bar_t *x"))
      (dolist (test '(("go/src/package1/x.go" "foo" "*.go")
		      ("c/src/x.c" "bar_t" "*.[ch]")
		      ("c/include/x.h" "bar_t" "*.[ch]")))
	(let ((projectile-use-git-grep t)
	      (current-prefix-arg '-)
	      (sym (cadr test)))
	  (noflet ((projectile-project-vcs () 'git)
               (read-string (prompt initial-input history default-value &rest args)
                            (if (should (equal sym default-value)) default-value))
		   (vc-git-grep (regexp files dir)
				(progn (should (equal sym regexp))
				       (should (equal (car (last test)) files))
				       (should (equal (projectile-project-root) dir)))))
            (with-current-buffer (find-file-noselect (car test) t)
	      (save-excursion
		(re-search-forward sym)
		(projectile-grep nil ?-)))))))))

;;;;;;;;; fresh tests

(ert-deftest projectile-clear-known-projects ()
  (let ((projectile-known-projects '("one" "two" "three")))
    (projectile-clear-known-projects)
    (should (null projectile-known-projects))))

(ert-deftest projectile-switch-project-no-projects ()
  (let ((projectile-known-projects nil))
    (should-error (projectile-switch-project))))

(ert-deftest projectile-ignored-buffer-p-by-name ()
  (let ((projectile-globally-ignored-buffers '("*nrepl messages*" "*something*")))
    (should (projectile-ignored-buffer-p (get-buffer-create "*nrepl messages*")))
    (should (projectile-ignored-buffer-p (get-buffer-create "*something*")))
    (should-not (projectile-ignored-buffer-p (get-buffer-create "test")))))

(ert-deftest projectile-test-get-other-files ()
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
                                       ("js" . ("js"))
                                       ))
        (source-tree '("src/test1.c"
                       "src/test2.c"
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

    (should (equal '("include1/test1.h" "include2/test1.h")
                   (projectile-get-other-files "src/test1.c" source-tree)))
    (should (equal '("include1/test1.h" "include2/test1.h" "include1/test1.hpp")
                   (projectile-get-other-files "src/test1.cpp" source-tree)))
    (should (equal '("include1/test2.h" "include2/test2.h")
                   (projectile-get-other-files "test2.c" source-tree)))
    (should (equal '("include1/test2.h" "include2/test2.h" "include2/test2.hpp")
                   (projectile-get-other-files "test2.cpp" source-tree)))
    (should (equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp")
                   (projectile-get-other-files "test1.h" source-tree)))
    (should (equal '("src/test2.c" "src/test2.cpp" "include2/test2.hpp")
                   (projectile-get-other-files "test2.h" source-tree)))
    (should (equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp")
                   (projectile-get-other-files "include1/test1.h" source-tree t)))
    (should (equal '("src/Makefile")
                   (projectile-get-other-files "Makefile.lock" source-tree)))
    (should (equal '("src/some_module/same_name.c" "src/same_name.c")
                   (projectile-get-other-files "include2/some_module/same_name.h" source-tree)))
    ;; nested extensions
    (should (equal '("include1/test1.service.spec.js" "include2/test1.service.spec.js")
                   (projectile-get-other-files "src/test1.service.js" source-tree)))
    ;; fallback to outer extensions if no rule for nested extension defined
    (should (equal '("include1/test2.js" "include2/test2.js")
                   (projectile-get-other-files "src/test2.service.spec.js" source-tree)))
    ))

(ert-deftest projectile-test-compilation-directory ()
  (defun helper (project-root rel-dir)
    (noflet ((projectile-project-root () project-root))
            (let ((projectile-project-compilation-dir rel-dir))
              (projectile-compilation-dir))))

  (should (equal "/root/build/" (helper "/root/" "build")))
  (should (equal "/root/build/" (helper "/root/" "build/")))
  (should (equal "/root/build/" (helper "/root/" "./build")))
  (should (equal "/root/local/build/" (helper "/root/" "local/build"))))

(ert-deftest projectile-test-dirname-matching-count ()
  (should (equal 2
                 (projectile-dirname-matching-count "src/food/sea.c"
                                                    "src/food/cat.c")))
  (should (equal 0
                 (projectile-dirname-matching-count "src/weed/sea.c"
                                                    "src/food/sea.c")))
  (should (equal 0
                 (projectile-dirname-matching-count "test/demo-test.el"
                                                    "demo.el"))))

(ert-deftest projectile-test-find-matching-test ()
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
        (noflet ((projectile-project-type () 'rails-rspec)
                 (projectile-project-root
                  () (file-truename (expand-file-name "project/"))))
          (should (equal "spec/models/food/sea_spec.rb"
                         (projectile-find-matching-test
                          "app/models/food/sea.rb"))))))))

(ert-deftest projectile-test-find-matching-file ()
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
        (noflet ((projectile-project-type () 'rails-rspec)
                 (projectile-project-root () (file-truename (expand-file-name "project/"))))
          (should (equal "app/models/food/sea.rb"
                         (projectile-find-matching-file
                          "spec/models/food/sea_spec.rb"))))))))

(ert-deftest projectile-test-find-matching-test/file-custom-project ()
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
     (let* ((projectile-indexing-method 'native)
            (reg (projectile-register-project-type 'npm-project '("somefile") :test-suffix ".spec")))
        (noflet ((projectile-project-type () 'npm-project)
                 (projectile-project-root () (file-truename (expand-file-name "project/"))))
          (let ((test-file (projectile-find-matching-test "src/foo/foo.service.js"))
                (impl-file (projectile-find-matching-file "test/bar/bar.service.spec.js")))
            (should (equal "test/foo/foo.service.spec.js" test-file))
            (should (equal "src/bar/bar.service.js" impl-file))))))))

(ert-deftest projectile-test-exclude-out-of-project-submodules ()
  (projectile-test-with-files
   (;; VSC root is here
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
     (noflet ((projectile-files-via-ext-command
               (arg) (when (string= default-directory project)
                       '("vendor/client-submodule"
                         "../server/vendor/server-submodule")))
              (projectile-project-root
               () project))

       ;; assert that it only returns the submodule 'project/web-ui/vendor/client-submodule/'
       (should (equal (list (expand-file-name "vendor/client-submodule/" project))
                      (projectile-get-all-sub-projects project)))))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; projectile-test.el ends here
