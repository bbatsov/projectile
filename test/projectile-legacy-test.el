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
