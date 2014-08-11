(require 'projectile)
(require 'ert)
(require 'noflet)
(require 'test-helper)

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


(ert-deftest projectile-test-parse-dirconfig-file ()
  (noflet ((buffer-string () " log\t\n-tmp \n-compiled\n+include\n")
           (file-exists-p (filename) t)
           (file-truename (filename) filename)
           (insert-file-contents-literally (filename) nil))
    (should (equal '(("include/") . ("log" "tmp" "compiled"))
                   (projectile-parse-dirconfig-file)))))

(ert-deftest projectile-test-ack ()
  (let ((ack-and-a-half-arguments '()))
    (noflet ((projectile-ignored-directories () '("/path/to/project/tmp" "/path/to/project/log"))
             (ack-and-a-half (pattern regexp dir) '("result"))
             (call-interactively
              (function &optional record-flag keys)
              (should (equal ack-and-a-half-arguments '("--ignore-dir=tmp" "--ignore-dir=log")))))
      (projectile-ack "test"))))

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
           (projectile-index-directory (dir patterns) (should (equal dir "a/"))
                                       '("/my/root/a/b/c" "/my/root/a/d/e"))
           (projectile-get-repo-files () '("/my/root/a/b/c" "/my/root/a/d/e"))
           (cd (directory) "/my/root/a/" nil))
    (let ((projectile-use-native-indexing t))
      (should (equal '("a/b/c" "a/d/e") (projectile-dir-files "a/"))))
    (let ((projectile-use-native-indexing nil))
      (should (equal '("a/b/c" "a/d/e") (projectile-dir-files "a/"))))))

(ert-deftest projectile-test-setup-hook-functions-projectile-mode ()
  (projectile-mode 1)
  (should (and (memq 'projectile-cache-files-find-file-hook find-file-hook)
               (memq 'projectile-cache-projects-find-file-hook find-file-hook)))
  (projectile-mode -1)
  (should (and (not (memq 'projectile-cache-files-find-file-hook find-file-hook))
               (not (memq 'projectile-cache-projects-find-file-hook find-file-hook)))))

(ert-deftest projectile-test-setup-hook-functions-projectile-global-mode ()
  (projectile-global-mode 1)
  (should (and (memq 'projectile-cache-files-find-file-hook find-file-hook)
               (memq 'projectile-cache-projects-find-file-hook find-file-hook)))
  (projectile-global-mode -1)
  (should (and (not (memq 'projectile-cache-files-find-file-hook find-file-hook))
               (not (memq 'projectile-cache-projects-find-file-hook find-file-hook)))))

(ert-deftest projectile-test-relevant-known-projects ()
  (let ((projectile-known-projects '("/path/to/project1" "/path/to/project2")))
    (noflet ((projectile-project-root () "/path/to/project1"))
      (should (equal (projectile-relevant-known-projects) '("/path/to/project2"))))))

(ert-deftest projectile-test-projects-cleaned ()
  (let* ((directories (cl-loop repeat 3 collect (make-temp-file "projectile-cleanup" t)))
         (projectile-known-projects directories))
    (unwind-protect
        (progn
          (projectile-cleanup-known-projects)
          (should (equal projectile-known-projects directories))
          (delete-directory (car directories))
          (projectile-cleanup-known-projects)
          (should (equal projectile-known-projects (cdr directories))))
      (--each directories (ignore-errors (delete-directory it))))))

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
          (with-current-buffer (find-file-noselect project-file)
            (should (file-name-absolute-p (projectile-project-root)))))
      (ignore-errors (delete-directory root-directory t)))))

(ert-deftest projectile-test-tags-exclude-items ()
  (noflet ((projectile-ignored-directories-rel () (list ".git/" ".hg/")))
    (should (equal (projectile-tags-exclude-patterns)
                   "--exclude=.git --exclude=.hg"))))

(ert-deftest projectile-test-maybe-invalidate ()
  (noflet ((projectile-invalidate-cache (arg) t))
    (should-not (projectile-maybe-invalidate-cache nil))
    (should (projectile-maybe-invalidate-cache t))
    (noflet ((file-newer-than-file-p (a b) t))
      (should (projectile-maybe-invalidate-cache nil)))))

(ert-deftest projectile-test-root-top-down ()
  (with-sandbox
   (f-mkdir "projectA" ".svn")
   (f-mkdir "projectA" "src" ".svn")
   (f-mkdir "projectA" "src" "html" ".svn")
   (f-mkdir "projectA" ".git")
   (f-mkdir "projectA" "src" "html")
   (f-mkdir "projectA" "src" "framework" "lib")
   (f-touch "projectA/src/framework.conf")
   (f-touch "projectA/src/html/index.html")
   (should (equal "projectA/src/"
                  (projectile-root-top-down "projectA/src/framework/lib"
                                            '("framework.conf" ".git"))))
   (should (equal "projectA/src/"
                  (projectile-root-top-down "projectA/src/framework/lib"
                                            '(".git" "framework.conf"))))
   (should (equal "projectA/src/html/"
                  (projectile-root-top-down "projectA/src/html/"
                                            '(".svn"))))))

(ert-deftest projectile-test-root-top-down-recurring ()
  (with-sandbox
   (f-mkdir "projectA" ".svn")
   (f-mkdir "projectA" "src" ".svn")
   (f-mkdir "projectA" "src" "html" ".svn")
   (f-mkdir "projectA" ".git")
   (f-mkdir "projectA" "src" "html")
   (f-mkdir "projectA" "src" "framework" "lib")
   (f-touch "projectA/src/framework/framework.conf")
   (f-touch "projectA/src/html/index.html")
   (f-touch ".projectile")
   (should (equal "projectA/"
                  (projectile-root-top-down-recurring "projectA/src/html/"
                                                      '("something" ".svn" ".git"))))
   (should (equal "projectA/"
                  (projectile-root-top-down-recurring "projectA/src/html/"
                                                      '(".git"))))
   (should-not (projectile-root-top-down-recurring "projectA/src/html/"
                                                   '("elusivefile")))))

(ert-deftest projectile-test-root-bottom-up ()
  (with-sandbox
   (f-mkdir "projectA" ".svn")
   (f-mkdir "projectA" "src" ".svn")
   (f-mkdir "projectA" "src" "html" ".svn")
   (f-mkdir "projectA" ".git")
   (f-mkdir "projectA" "src" "html")
   (f-mkdir "projectA" "src" "framework" "lib")
   (f-touch "projectA/src/framework/framework.conf")
   (f-touch "projectA/src/html/index.html")
   (f-touch "projectA/.projectile")
   (should (equal "projectA/"
                  (projectile-root-bottom-up "projectA/src/framework/lib"
                                             '(".git" ".svn"))))
   (should (equal "projectA/"
                  (projectile-root-bottom-up "projectA/src/html"
                                             '(".git" ".svn"))))
   (should (equal "projectA/src/html/"
                  (projectile-root-bottom-up "projectA/src/html"
                                             '(".svn" ".git"))))
   (should (equal "projectA/"
                  (projectile-root-bottom-up "projectA/src/html"
                                             '(".projectile" "index.html"))))))

(ert-deftest projectile-test-project-root ()
  (with-sandbox
   (f-mkdir "projectA" "src" ".svn")
   (f-mkdir "projectA" "src" "html" ".svn")
   (f-mkdir "projectA" "src" "html")
   (f-mkdir "projectA" "src" "framework" "lib")
   (f-mkdir "projectA" "build" "framework" "lib")
   (f-mkdir "projectA" "requirements" "a" "b" "c" "d" "e" "f" "g")
   (f-touch "projectA/src/framework/framework.conf")
   (f-touch "projectA/requirements/a/b/c/requirements.txt")
   (f-touch "projectA/src/html/index.html")
   (f-touch "projectA/.projectile")
   (f-touch "override")
   (let ((projectile-project-root-files-bottom-up '("somefile" ".projectile"))
         (projectile-project-root-files '("otherfile" "framework.conf" "requirements.txt"))
         (projectile-project-root-files-top-down-recurring '(".svn" ".foo"))
         (projectile-project-root-files-functions '(projectile-root-bottom-up
                                                    projectile-root-top-down
                                                    projectile-root-top-down-recurring)))
     (should (f-same? "projectA"
                      (project-root-in "projectA/requirements/a/b/c/d/e/f/g")))
     (should (f-same? "projectA"
                      (project-root-in "projectA/src/framework/lib")))
     (should (f-same? "projectA"
                      (project-root-in "projectA/src/html")))

     (setq projectile-project-root-files-functions '(projectile-root-top-down
                                                     projectile-root-top-down-recurring
                                                     projectile-root-bottom-up))
     (should (f-same? "projectA/requirements/a/b/c"
                      (project-root-in "projectA/requirements/a/b/c/d/e/f/g")))
     (should (f-same? "projectA/src/framework"
                      (project-root-in "projectA/src/framework/lib")))
     (should (f-same? "projectA/src"
                      (project-root-in "projectA/src/html"))))

   (let ((projectile-project-root-files-bottom-up '("somefile" ".projectile"))
         (projectile-project-root-files '("otherfile" "noframework.conf"))
         (projectile-project-root-files-top-down-recurring '(".svn" ".foo"))
         (projectile-project-root-files-functions '(projectile-root-top-down-recurring
                                                    projectile-root-bottom-up
                                                    projectile-root-top-down)))
     (should (f-same? "projectA/src"
                      (project-root-in "projectA/src/framework/lib")))
     (should (f-same? "projectA/src"
                      (project-root-in "projectA/src/html")))
     (should (f-same? "projectA/"
                      (project-root-in "projectA/build/framework/lib"))))

   (let ((projectile-project-root-files-bottom-up '("somefile" "override"))
         (projectile-project-root-files '("otherfile" "anotherfile"))
         (projectile-project-root-files-top-down-recurring '("someotherfile" "yetanotherfile"))
         (projectile-project-root-files-functions '(projectile-root-bottom-up
                                                    projectile-root-top-down
                                                    projectile-root-top-down-recurring)))
     (should (f-same? default-directory
                      (project-root-in "projectA/src/framework/lib")))
     (should (f-same? default-directory
                      (project-root-in "projectA/src/html"))))
   (let ((projectile-project-root-files-bottom-up '("somecoolfile"))
         (projectile-project-root-files nil)
         (projectile-project-root-files-top-down-recurring '(".svn"))
         (projectile-project-root-files-functions '(projectile-root-bottom-up
                                                    projectile-root-top-down
                                                    projectile-root-top-down-recurring)))
     (should (f-same? "projectA/src/"
                      (project-root-in "projectA/src/")))
     (should (f-same? "projectA/src/"
                      (project-root-in "projectA/src/html"))))))

(ert-deftest projectile-test-file-exists-cache-disabled ()
  (with-sandbox
   (f-mkdir "project" "dirA" "dirB")
   (f-touch "project/fileA")
   (let ((projectile-file-exists-local-cache-expire nil)
         (projectile-file-exists-remote-cache-expire nil))
     (should (projectile-file-exists-p "project/fileA"))
     (should (projectile-file-exists-p "project/dirA/dirB"))
     (should-not (projectile-file-exists-p "project/dirA/fileB"))
     (f-touch "project/dirA/fileB")
     (should(projectile-file-exists-p "project/dirA/fileB"))
     (should-not (projectile-file-exists-p "project/nofile"))
     (f-delete "project/fileA")
     (should-not (projectile-file-exists-p "project/fileA")))))

(ert-deftest projectile-test-file-exists-cache ()
  (with-sandbox
   (f-mkdir "dirA" "dirB")
   (f-touch "fileA")
   (let* ((initial-time (current-time))
          (projectile-file-exists-local-cache-expire 100)
          (projectile-file-exists-remote-cache-expire nil))

     (noflet ((run-with-timer (&rest args) 'nooptimer))
       (noflet ((current-time () initial-time))
         (should (projectile-file-exists-p "fileA"))
         (should (projectile-file-exists-p "dirA/dirB"))
         (should-not (projectile-file-exists-p "dirA/fileB"))
         (f-touch "dirA/fileB")
         (should-not (projectile-file-exists-p "dirA/fileB"))
         (f-delete "fileA")
         (should (projectile-file-exists-p "fileA"))
         (should (equal  projectile-file-exists-cache-timer 'nooptimer))
         (projectile-file-exists-cache-cleanup)
         (should (equal  projectile-file-exists-cache-timer 'nooptimer)))

       (noflet ((current-time () (time-add initial-time (seconds-to-time 50))))
         (projectile-file-exists-cache-cleanup)
         (should (projectile-file-exists-p "fileA"))
         (should-not (projectile-file-exists-p "dirA/fileB"))
         (should-not (projectile-file-exists-p "fileC"))
         (f-touch "fileC")
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
         (should-not projectile-file-exists-cache-timer))))))

(ert-deftest projectile-test-git-grep-prefix ()
  (require 'vc-git)
  (with-sandbox
   (f-mkdir "project" "c" "src")
   (f-mkdir "project" "c" "include")
   (f-mkdir "project" "go" "src" "package1")
   (f-touch "project/.projectile")
   (cd "project")
   (f-write-bytes "foo(bar)" "go/src/package1/x.go")
   (f-write-bytes "typedef struct bar_t" "c/include/x.h")
   (f-write-bytes "struct bar_t *x" "c/src/x.c")
   (dolist (test '(("go/src/package1/x.go" "foo" "*.go")
                   ("c/src/x.c" "bar_t" "*.[ch]")
                   ("c/include/x.h" "bar_t" "*.[ch]")))
     (let ((projectile-use-git-grep t)
           (current-prefix-arg '-)
           (sym (cadr test)))
       (noflet ((projectile-project-vcs () 'git)
                (read-string (prompt initial &rest args)
                             (if (should (equal sym initial)) initial))
                (vc-git-grep (regexp files dir)
                             (progn (should (equal sym regexp))
                                    (should (equal (car (last test)) files))
                                    (should (equal (projectile-project-root) dir)))))
         (with-current-buffer (find-file-noselect (car test))
           (save-excursion
             (re-search-forward sym)
             (projectile-grep ?-))))))))

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
    (should (projectile-ignored-buffer-p (generate-new-buffer "*nrepl messages*")))
    (should (projectile-ignored-buffer-p (generate-new-buffer "*something*")))
    (should-not (projectile-ignored-buffer-p (generate-new-buffer "test")))))
