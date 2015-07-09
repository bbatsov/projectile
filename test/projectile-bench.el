(require 'projectile)
(require 'ert)
(require 'noflet)
(require 'test-helper)
(require 'bench-helper)
(require 'benchmark)
(require 'dash)
(require 'tramp)

;; just something to get started

(ert-deftest 00-connect-tramp ()
  (with-benchmarks
   (let ((default-directory bench-projectile-basedir))
     (should (file-exists-p (bp))))))

(ert-deftest projectile-find-projetile-root ()
  (with-benchmarks
   50
   (let ((default-directory (bp 'projectile "test/tmp/")))
     (should (equal (projectile-project-root) (bp 'projectile))))))

(ert-deftest projectile-find-linux-kernel-root ()
  (with-benchmarks
   50
   (let ((default-directory (bp 'linux "tools/cgroup/")))
     (directory-files default-directory)
     (should (equal (projectile-project-root) (bp 'linux))))))

(ert-deftest projectile-find-linux-kernel-root2 ()
  (with-benchmarks
   50
   (let ((default-directory (bp 'linux "tools/cgroup/")))
     (should (equal (projectile-project-root) (bp 'linux))))))

(ert-deftest projectile-find-linux-kernel-root3 ()
  (with-benchmarks
   10
   (let ((default-directory (bp 'linux "tools/cgroup/")))
     (should (equal (projectile-project-root) (bp 'linux))))))

(ert-deftest projectile-nofiles1 ()
  (with-benchmarks
   50
   (let ((default-directory (bp 'nofiles-medium)))
     (should (not (projectile-project-p))))))




;;; Test each root finding function indivudually.
;; TODO Right now these are run with their default individual configurations,
;; some matches some does not. This is all well for some profiling comparisons.

(defcustom projectile-project-root-files-functions
  '(projectile-root-bottom-up
    projectile-root-top-down
    projectile-root-top-down-recurring)
  "A list of functions for finding project roots."
  :group 'projectile
  :type '(repeat function))


(ert-deftest projectile-find-root-fn-nil ()
  (with-benchmarks
   10
   (let ((projectile-project-root-files-functions '())
         (default-directory (bp 'linux "tools/cgroup/")))
     (projectile-project-p))))

(ert-deftest projectile-find-root-fn-bottom-up ()
  (with-benchmarks
   10
   (let ((projectile-project-root-files-functions '(projectile-root-bottom-up))
         (default-directory (bp 'linux "tools/cgroup/")))
     (projectile-project-p))))

(ert-deftest projectile-find-root-fn-top-down ()
  (with-benchmarks
   10
   (let ((projectile-project-root-files-functions '(projectile-root-top-down))
         (default-directory (bp 'linux "tools/cgroup/")))
          (projectile-project-p))))

(ert-deftest projectile-find-root-fn-top-down-recurring ()
  (with-benchmarks
   10
   (let ((projectile-project-root-files-functions
          '(projectile-root-top-down-recurring))
         (default-directory (bp 'linux "tools/cgroup/")))
     (projectile-project-p))))


;;; Check non projectile project using default settings
;; These tests will check a couple of paths under a directory tree which will
;; not be identified as a projectile project. This is the most expensive type
;; of checks since all root finding functions has to be consulted to do this.


(ert-deftest projectile-nofiles-shallow-to-deep ()
  (with-benchmarks
   10
   (let ((default-directory (bp 'nofiles-shallow))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-medium))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-deep))) (projectile-project-p))))

(ert-deftest projectile-nofiles-shallow-to-edge ()
  (with-benchmarks
   10
   (let ((default-directory (bp 'nofiles-shallow))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-medium))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-deep))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-edge))) (projectile-project-p))))

(ert-deftest projectile-nofiles-edge-to-shallow ()
  (with-benchmarks
   10
   (let ((default-directory (bp 'nofiles-edge))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-deep))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-medium))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-shallow))) (projectile-project-p))))

(ert-deftest projectile-nofiles-random-order ()
  (with-benchmarks
   10
   (let ((default-directory (bp 'nofiles-medium))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-edge))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-shallow))) (projectile-project-p))
   (let ((default-directory (bp 'nofiles-deep))) (projectile-project-p))))

