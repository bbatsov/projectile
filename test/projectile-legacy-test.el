;;;;;;; Warning
;;
;; This file contains the project's legacy tests using the ERT framework.
;; We're in the process of moving those tests to Buttercup.
;; Don't add anything new here!

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

(ert-deftest projectile-test-ignored-directory-p ()
  (noflet ((projectile-ignored-directories () '("/path/to/project/tmp" "/path/to/project/t\\.*")))
    (should (projectile-ignored-directory-p "/path/to/project/tmp"))
    (should (projectile-ignored-directory-p "/path/to/project/t.ignore"))
    (should-not (projectile-ignored-directory-p "/path/to/project/log"))))

(ert-deftest projectile-test-ignored-file-p ()
  (noflet ((projectile-ignored-files () '("/path/to/project/TAGS" "/path/to/project/T.*")))
    (should (projectile-ignored-file-p "/path/to/project/TAGS"))
    (should-not (projectile-ignored-file-p "/path/to/project/foo.el"))))

(ert-deftest projectile-test-ignored-files ()
  (noflet ((projectile-project-root () "/path/to/project")
           (projectile-project-name () "project")
           (projectile-project-ignored-files () '("foo.js" "bar.rb")))
    (let ((expected '("/path/to/project/TAGS"
                      "/path/to/project/foo.js"
                      "/path/to/project/bar.rb"
                      "/path/to/project/file1.log"
                      "/path/to/project/file2.log"))
          (projectile-ignored-files '("TAGS" "file\d+\\.log")))
            (should-not (equal (projectile-ignored-files) expected)))))

(ert-deftest projectile-test-ignored-directories ()
  (noflet ((projectile-project-ignored-directories () '("tmp" "log"))
           (projectile-project-root () "/path/to/project"))
          (let ((expected '("/path/to/project/compiled/"
                            "/path/to/project/ignoreme"
                            "/path/to/project/ignoremetoo"
                            "/path/to/project/tmp"
                            "/path/to/project/log"))
                (projectile-globally-ignored-directories '("compiled" "ignoreme")))
            (should-not (equal (projectile-ignored-directories) expected)))))

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
          (let* ((file-names '("foo.c" "foo.o" "foo.so" "foo.o.gz" "foo.tar.gz" "foo.tar.GZ"))
                 (files (mapcar 'projectile-expand-root file-names)))
            (let ((projectile-globally-ignored-file-suffixes '(".o" ".so" ".tar.gz")))
              (should (equal (projectile-remove-ignored files)
                             (mapcar 'projectile-expand-root
                                     '("foo.c" "foo.o.gz"))))))))

(ert-deftest projectile-add-unignored-files ()
  (noflet ((projectile-get-repo-ignored-files (project vcs) '("unignored-file"
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
  (noflet ((projectile-project-vcs (project) 'none))
    ;; on an unsupported VCS we simply return the list of globally unignored files
    (let ((projectile-globally-unignored-files '("unignored-file")))
      (should (equal (projectile-add-unignored '("file")) '("file"))))))

(ert-deftest projectile-add-unignored-directories ()
  (noflet ((projectile-project-vcs (project) 'git)
           (projectile-get-repo-ignored-files (project vcs) '("path/unignored-file"))
           (projectile-get-repo-ignored-directory (project dir vcs) (list (concat dir "unignored-file"))))
    (let ((projectile-globally-unignored-directories '("path")))
      (should (equal (projectile-add-unignored '("file"))
                     '("file" "path/unignored-file")))
      ;; Ignored files inside unignored paths need to be explicitly
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
    (should (equal '("/my/root/") (projectile-get-project-directories "/my/root/")))
    (noflet ((projectile-parse-dirconfig-file () '(("foo" "bar/baz"))))
      (should (equal '("/my/root/foo" "/my/root/bar/baz")
                     (projectile-get-project-directories "/my/root/"))))))

(ert-deftest projectile-test-dir-files ()
  (noflet ((projectile-patterns-to-ignore () nil)
           (projectile-index-directory (dir patterns progress-reporter)
                                       (should (equal dir "a/"))
                                       '("/my/root/a/b/c" "/my/root/a/d/e"))
           (projectile-dir-files-alien (dir) '("a/b/c" "a/d/e"))
           (cd (directory) "/my/root/a/" nil))
    (let ((projectile-indexing-method 'native))
      (should (equal '("a/b/c" "a/d/e") (projectile-dir-files "/my/root/"))))
    (let ((projectile-indexing-method 'alien))
      (should (equal '("a/b/c" "a/d/e") (projectile-dir-files "/my/root/"))))))

(ert-deftest projectile-test-git-submodule-command ()
  (should (string-prefix-p "git" (projectile-get-sub-projects-command 'git)))
  (should (string-empty-p (projectile-get-sub-projects-command 'none))))

(ert-deftest projectile-test-files-via-ext-command ()
  (should (not (projectile-files-via-ext-command "" "")))
  (should (not (projectile-files-via-ext-command "" nil))))

(ert-deftest projectile-test-setup-hook-functions-projectile-mode ()
  (noflet ((projectile--cleanup-known-projects () nil)
           (projectile-discover-projects-in-search-path () nil))
    (projectile-mode 1)
    (should (memq 'projectile-find-file-hook-function find-file-hook))
    (projectile-mode -1)
    (should (not (memq 'projectile-find-file-hook-function find-file-hook)))))

(ert-deftest projectile-test-relevant-known-projects ()
  (let ((projectile-known-projects '("/path/to/project1" "/path/to/project2")))
    (noflet ((projectile-project-root (&optional dir) "/path/to/project1"))
      (should (equal (projectile-relevant-known-projects) '("/path/to/project2"))))))

(ert-deftest projectile-test-projects-cleaned ()
  (let* ((projectile-known-projects-file (projectile-test-tmp-file-path))
         (directories (cl-loop repeat 3 collect (make-temp-file "projectile-cleanup" t)))
         (projectile-known-projects directories))
    (unwind-protect
        (progn
          (projectile--cleanup-known-projects)
          (should (equal projectile-known-projects directories))
          (delete-directory (car directories))
          (projectile--cleanup-known-projects)
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

(defun projectile-test-tmp-file-path ()
  "Return a filename suitable to save data to in the
test temp directory"
  (concat projectile-test-path
          "/tmp/temporary-file-" (format "%d" (random))
          ".eld"))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; projectile-test.el ends here
