(defconst testsuite-dir
  (if load-file-name
      (file-name-directory load-file-name)
    ;; Fall back to default directory (in case of M-x eval-buffer)
    default-directory)
  "Directory of the test suite.")

(message "Running tests on Emacs %s" emacs-version)

(require 'dash)
(require 's)

;; Load Projectile
(load (expand-file-name "../projectile" testsuite-dir) nil :no-message)

;; Load test helpers
(load (expand-file-name "test-helper.el" testsuite-dir) nil :no-message)

(ert-deftest projectile-test-uniquify-file ()
  (should (equal (projectile-uniquify-file "ala/bala/portokala")
                 "bala/portokala")))

(ert-deftest projectile-test-project-get-name ()
  (should (equal (projectile-project-name) "project")))

(ert-deftest projectile-test-prepend-project-name ()
  (should (equal (projectile-prepend-project-name "Test") "[project] Test")))

(ert-deftest projectile-test-expand-root ()
  (should (equal (projectile-expand-root "foo") "/path/to/project/foo"))
  (should (equal (projectile-expand-root "foo/bar") "/path/to/project/foo/bar"))
  (should (equal (projectile-expand-root "./foo/bar") "/path/to/project/foo/bar")))

(ert-deftest projectile-test-ignored-directory-p ()
  (flet ((projectile-ignored-directories () '("/path/to/project/tmp")))
    (should (projectile-ignored-directory-p "/path/to/project/tmp"))
    (should-not (projectile-ignored-directory-p "/path/to/project/log"))))

(ert-deftest projectile-test-ignored-file-p ()
  (flet ((projectile-ignored-files () '("/path/to/project/TAGS")))
    (should (projectile-ignored-file-p "/path/to/project/TAGS"))
    (should-not (projectile-ignored-file-p "/path/to/project/foo.el"))))

(ert-deftest projectile-test-ignored-files ()
  (flet ((projectile-project-ignored-files () '("foo.js" "bar.rb")))
    (let ((expected '("/path/to/project/TAGS"
                      "/path/to/project/foo.js"
                      "/path/to/project/bar.rb"))
          (projectile-ignored-files '("TAGS")))
      (should (equal (projectile-ignored-files) expected)))))

(ert-deftest projectile-test-ignored-directories ()
  (flet ((projectile-project-ignored-directories () '("tmp" "log"))
         (projectile-project-root () "/path/to/project"))
    (let ((expected '("/path/to/project/compiled/"
                      "/path/to/project/tmp/"
                      "/path/to/project/log/"))
          (projectile-globally-ignored-directories '("compiled")))
      (should (equal (projectile-ignored-directories) expected)))))

(ert-deftest projectile-test-project-ignored-files ()
  (let ((files '("/path/to/project/foo.el" "/path/to/project/foo.elc")))
    (flet ((projectile-project-ignored () files))
      (flet ((file-directory-p (filename) nil))
        (should (equal (projectile-project-ignored-files) files)))
      (flet ((file-directory-p (filename) t))
        (should-not (projectile-project-ignored-files))))))

(ert-deftest projectile-test-project-ignored-directories ()
  (let ((directories '("/path/to/project/tmp" "/path/to/project/log")))
    (flet ((projectile-project-ignored () directories))
      (flet ((file-directory-p (filename) t))
        (should (equal (projectile-project-ignored-directories) directories)))
      (flet ((file-directory-p (filename) nil))
        (should-not (projectile-project-ignored-directories))))))

(ert-deftest projectile-test-project-ignored ()
  (let* ((file-names '("log" "tmp" "compiled"))
         (files (mapcar 'projectile-expand-root file-names)))
    (flet ((projectile-paths-to-ignore () (list "log" "tmp" "compiled"))
           (file-expand-wildcards (pattern ignored)
                                  (cond
                                   ((string-equal pattern "log")
                                    (list "/path/to/project/log"))
                                   ((string-equal pattern "tmp")
                                    (list "/path/to/project/tmp"))
                                   ((string-equal pattern "compiled")
                                    (list "/path/to/project/compiled")))))
      (should (equal (projectile-project-ignored) files)))))


(ert-deftest projectile-test-parse-dirconfig-file ()
  (flet ((buffer-string () " log\t\n-tmp \n-compiled\n+include\n")
         (file-exists-p (filename) t)
         (insert-file-contents-literally (filename) nil))
    (should (equal '(("include") . ("log" "tmp" "compiled"))
                   (projectile-parse-dirconfig-file)))))

(ert-deftest projectile-test-ack ()
  (flet ((projectile-ignored-directories () '("/path/to/project/tmp" "/path/to/project/log"))
         (call-interactively
          (function &optional record-flag keys)
          (should (equal ack-and-a-half-arguments '("--ignore-dir=tmp" "--ignore-dir=log")))))
    (projectile-ack)))

(ert-deftest projectile-test-hashify-relative ()
  (let* ((file-names '("/path/to/project/foo/bar"))
         (projectile-show-paths-function 'projectile-hashify-with-relative-paths)
         (files-table (projectile-hashify-files file-names))
         (files-table-keys))
    (maphash (lambda (key value) (setq files-table-keys (cons key files-table-keys))) files-table)
    (should (equal files-table-keys '("foo/bar")))))

(ert-deftest projectile-test-hashify-uniquify ()
  (let* ((file-names '("/path/to/project/foo/bar" "/path/to/project/foo2/bar" "/path/to/project/biz/baz"))
         (projectile-show-paths-function 'projectile-hashify-with-uniquify)
         (files-table (projectile-hashify-files file-names))
         (files-table-keys))
    (maphash (lambda (key value) (setq files-table-keys (cons key files-table-keys))) files-table)
    (should (equal (sort files-table-keys 'string-lessp) '("baz" "foo/bar" "foo2/bar")))))

(ert-deftest projectile-test-get-project-directories ()
  (flet ((projectile-project-root () "/my/root/")
         (projectile-parse-dirconfig-file () '(nil)))
    (should (equal '("/my/root/") (projectile-get-project-directories)))
    (flet ((projectile-parse-dirconfig-file () '(("foo" "bar/baz"))))
      (should (equal '("/my/root/foo" "/my/root/bar/baz")
                     (projectile-get-project-directories))))))
