(require 'projectile)
(require 'ert)
(require 'noflet)

(ert-deftest projectile-test-add-known-project-adds-project-to-known-projects ()
  "An added project should be added to the list of known projects."
  (let (projectile-known-projects)
    (projectile-add-known-project "~/my/new/project")
    (should (string= (car projectile-known-projects)
                     "~/my/new/project"))))

(ert-deftest projectile-test-add-known-project-moves-projects-to-front-of-list ()
  "adding a project should move it to the front of the list of known projects, if it already
existed."
  (let ((projectile-known-projects (list "~/b" "~/a")))
    (projectile-add-known-project "~/a")
    (should (equal projectile-known-projects
                   (list "~/a" "~/b")))))

(defun projectile-mock-serialization-functions (&rest body)
  (let (projectile-serialization-calls)
    (noflet ((projectile-serialize (&rest args)
                                   (push (cons 'serialize args)
                                         projectile-serialization-calls)
                                   'projectile-serialize-return)
             (projectile-unserialize (&rest args)
                                     (push (cons 'unserialize args)
                                           projectile-serialization-calls)
                                     'projectile-unserialize-return))
            (eval (cons 'progn body)))))

(defun projectile-test-tmp-file-path ()
  "Return a filename suitable to save data to in the
test temp directory"
  (concat projectile-test-path
          "/tmp/temporary-file-" (format "%d" (random))
          ".eld"))

(ert-deftest projectile-test-loads-known-projects-through-serialization-functions ()
  (projectile-mock-serialization-functions
   '(let ((projectile-known-projects-file (projectile-test-tmp-file-path)))
      (projectile-load-known-projects)

      (should (equal projectile-known-projects
                     'projectile-unserialize-return))

      (should (equal (car projectile-serialization-calls)
                     `(unserialize ,projectile-known-projects-file))))))

(ert-deftest projectile-test-merge-known-projects ()
  (let ((projectile-known-projects nil)
        (projectile-known-projects-file (projectile-test-tmp-file-path)))
    ;; initalize saved known projects and load it from disk
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
    (should (equal projectile-known-projects '("a6" "a1" "a3" "b1" "b2")))))

(ert-deftest projectile-test-merge-known-projects-to-empty ()
  (let ((projectile-known-projects nil)
        (projectile-known-projects-file (projectile-test-tmp-file-path)))
    ;; initalize saved known projects and load it from disk
    (projectile-serialize '("a1" "a2" "a3" "a4" "a5")
                          projectile-known-projects-file)
    (projectile-load-known-projects)
    ;; empty the on disk known projects list
    (projectile-serialize '() projectile-known-projects-file)
    ;; merge
    (projectile-merge-known-projects)
    (delete-file projectile-known-projects-file nil)
    (should (equal projectile-known-projects '()))))

(ert-deftest projectile-test-merge-known-projects-from-empty ()
  (let ((projectile-known-projects nil)
        (projectile-known-projects-file (projectile-test-tmp-file-path)))
    ;; initalize saved known projects and load it from disk
    (projectile-serialize '() projectile-known-projects-file)
    (projectile-load-known-projects)
    ;; empty the on disk known projects list
    (projectile-serialize '("a" "b" "c" "d") projectile-known-projects-file)
    ;; merge
    (projectile-merge-known-projects)
    (delete-file projectile-known-projects-file nil)
    (should (equal projectile-known-projects '("a" "b" "c" "d")))))

(ert-deftest projectile-test-merge-known-projects-keep-order ()
  (let ((projectile-known-projects nil)
        (projectile-known-projects-file (projectile-test-tmp-file-path)))
    ;; initalize saved known projects and load it from disk
    (projectile-serialize '("a" "b" "c" "d") projectile-known-projects-file)
    (projectile-load-known-projects)
    ;; save the same list in different order
    (projectile-serialize '("d" "c" "b" "a") projectile-known-projects-file)
    ;; merge
    (projectile-merge-known-projects)
    (delete-file projectile-known-projects-file nil)
    (should (equal projectile-known-projects '("a" "b" "c" "d")))))

(ert-deftest
    projectile-test-saves-known-projects-through-serialization-functions ()
  (projectile-mock-serialization-functions
   '(let ((projectile-known-projects-file (projectile-test-tmp-file-path))
          (projectile-known-projects '(floop)))

      (projectile-save-known-projects)

      (should (equal (car projectile-serialization-calls)
                     `(serialize (floop) ,projectile-known-projects-file))))))

(ert-deftest projectile-test-serialization-functions ()
  "Test that serialization funtions can save/restore data to the filesystem."
  (let ((this-test-file (projectile-test-tmp-file-path)))
    (unwind-protect
        (progn
          (projectile-serialize '(some random data) this-test-file)
          (should (equal (projectile-unserialize this-test-file)
                         '(some random data))))
      (when (file-exists-p this-test-file)
        (delete-file this-test-file)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
