(defun projectile-switch-project-test-env (body)
  (let (dired-calls
        projectile-called-hooks-flag
        ido-response
        (projectile-switch-project-hook
         (lambda () (setq projectile-called-hooks-flag t))))
    (flet ((ido-completing-read (prompt options)
                                ido-response)
           (dired (&rest args)
                  (push args dired-calls)))
      (eval (cons 'progn body)))))

(ert-deftest projectile-test-switch-project-calls-dired ()
  "A good default action for switching a project is to
switch a dired buffer on the projects root directory."
  (projectile-switch-project-test-env
   '((let ((ido-response "~/my/project/"))
       (projectile-switch-project)
       (should (string= (car (car dired-calls))
                        ido-response))))))



(ert-deftest projectile-test-switch-project-calls-hooks ()
  "Hooks should be called so user run code
that is needed to support a working project."
  (projectile-switch-project-test-env
   '((projectile-switch-project)
     (should projectile-called-hooks-flag))))

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
    (flet ((projectile-serialize (&rest args)
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

(ert-deftest projectile-test-saves-known-projects-through-serialization-functions ()
  (projectile-mock-serialization-functions
   '(let ((projectile-known-projects-file (projectile-test-tmp-file-path))
          (projectile-known-projects '(floop)))

      (projectile-save-known-projects)

      (should (equal (car projectile-serialization-calls)
                     `(serialize ,projectile-known-projects-file (floop)))))))

(ert-deftest projectile-test-serialization-functions ()
  "Test that serialization funtions can save/restore data to the filesystem."
  (let ((this-test-file (projectile-test-tmp-file-path)))
    (unwind-protect
        (progn
          (projectile-serialize this-test-file '(some random data))
          (should (equal (projectile-unserialize this-test-file)
                         '(some random data))))
      (when (file-exists-p this-test-file)
        (delete-file this-test-file)))))



