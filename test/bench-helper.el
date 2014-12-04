(require 'elp)
(require 'trace)
(require 'tramp)
(require 'ert)
(require 'benchmark)
(require 'cl-lib)
(require 'dash)


(defvar bench-projectile-basedir
  (if (getenv "BENCH_DATADIR")
      (getenv "BENCH_DATADIR")
    nil))

(defvar bench-profile
  (if (getenv "BENCH_PROFILE")
      (getenv "BENCH_PROFILE")
    nil))

(defvar bench-results-file
  (if (getenv "BENCH_RESULTS")
      (getenv "BENCH_RESULTS")
    nil))

(defvar bench-file
  (if (getenv "BENCH_FILE")
      (getenv "BENCH_FILE")
    nil))


(when (equal bench-profile "no-cache")
  (setq projectile-enable-caching nil
        projectile-file-exists-remote-cache-expire 0
        projectile-file-exists-local-cache-expire 0))

(when (equal bench-profile "full-cache")
  (setq projectile-enable-caching t))

(defun empty-caches ()
  "Reset projectile caches to a clean state."
  (setq projectile-project-root-cache (make-hash-table :test 'equal)
        projectile-file-exists-cache (make-hash-table :test 'equal)
        projectile-cache-file (make-temp-file "projectile-cachefile-")
        projectile-projects-cache (make-hash-table :test 'equal)
        projectile-file-exists-remote-cache-expire 1000000
        projectile-file-exists-local-cache-expire 60))


(defvar pref-results '() "Benchmark results.")

(defun perf-write-results ()
  "Write benchmark results to file."
  (when bench-results-file
    (let ((data '()))
      (when (file-exists-p bench-results-file)
        (setq data (with-temp-buffer
                     (insert-file-contents bench-results-file)
                     (read (buffer-string)))))
      (when (file-writable-p bench-results-file)
        (with-temp-file bench-results-file
          (insert (let (print-length) (prin1-to-string
                                       (-concat data pref-results)))))))))

(defmacro with-benchmarks (&rest body)
  "Evaluate BODY with benchmarksin an empty temporary directory."
  (let ((duration (make-symbol "duration")))
    `(progn
       (sit-for 0.1)
       (when (equal bench-profile "cache-per-test")
         (empty-caches))
       (let ((bench-result (benchmark-run ,@body))
             (test-name  (ert-test-name (ert-running-test))))
         (add-to-list 'pref-results
                      (list bench-file bench-projectile-basedir
                            bench-profile test-name (car bench-result)))
         (message "                  time: %s" (car bench-result)))
       (sit-for 0.1))))

(defvar bench-projectile-paths
  (make-hash-table :test 'eq)
  "Project paths dictionary.")

(mapc
 #'(lambda (v)
     (puthash  (car v) (cdr v) bench-projectile-paths))
 '((projectile . "someuser/repos/github/projectile/")
   (linux . "someuser/repos/linux/" )
   (emacs . "someuser/downloads/source/emacs/emacs-24.4/" )
   (nofiles-shallow . "nofiles/shallow/")
   (nofiles-medium . "nofiles/shallow/c/d/e/f/g/h/medium/")
   (nofiles-deep
    . "nofiles/shallow/c/d/e/f/g/h/medium/j/k/l/m/n/o/p/q/r/s/t/u/v/x/deep/")
   (nofiles-edge
    . "nofiles/shallow/c/d/e/f/g/h/medium/j/k/l/m/n/o/p/q/r/s/t/u/v/x/deep/edge/")))

(defun bp (&optional name subpath)
  "benchmark path concatinator"
  (let ((path (gethash name bench-projectile-paths)))
    (if (and name (not path))
        (error "bp got non existent named path"))
    (concat bench-projectile-basedir (or path "") (or subpath ""))))

;; (setq tramp-verbose 2)

(defun bench-elp-start ()
  "A sane set of elp instrumentations for investigating performance issues."
  (interactive)
  (require 'tramp)
  (require 'tramp-sh)

  (setq elp-use-standard-output t
        elp-report-limit 1
        elp-recycle-buffers-p nil)

  ;;;; --- projectile ---
  (elp-instrument-package "projectile-")

  ;;;; --- MISC ---
  (elp-instrument-function 'process-file)

  ;;;; file
  ;; --- (elp-instrument-package "file-") ---
  (elp-instrument-function 'directory-file-name)
  (elp-instrument-function 'expand-file-name)
  (elp-instrument-function 'file-exists-p)
  (elp-instrument-function 'file-name-as-directory)
  (elp-instrument-function 'file-relative-name)
  (elp-instrument-function 'file-remote-p)
  (elp-instrument-function 'file-truename)
  (elp-instrument-function 'shell-command-to-string)

  ;;;; ---  tramp related things ---
  ;; (elp-instrument-function 'tramp-file-name-handler)
  (elp-instrument-function 'tramp-handle-file-exists-p)
  (elp-instrument-function 'tramp-handle-file-name-as-directory)
  (elp-instrument-function 'tramp-handle-file-name-nondirectory)
  (elp-instrument-function 'tramp-handle-file-remote-p)
  (elp-instrument-function 'tramp-handle-shell-command)
  (elp-instrument-function 'tramp-sh-handle-file-truename)
  (elp-instrument-function 'tramp-sh-handle-process-file)
  (elp-instrument-function 'tramp-sh-handle-expand-file-name)

  ;;;; potential candidates for general tramp profiling
  ;; (elp-instrument-function 'tramp-sh-handle-make-symbolic-link)
  ;; (elp-instrument-function 'tramp-sh-handle-file-attributes)
  ;; (elp-instrument-function 'tramp-sh-handle-set-visited-file-modtime)
  ;; (elp-instrument-function 'tramp-sh-handle-verify-visited-file-modtime)
  ;; (elp-instrument-function 'tramp-sh-handle-set-file-modes)
  ;; (elp-instrument-function 'tramp-sh-handle-set-file-times)
  ;; (elp-instrument-function 'tramp-sh-handle-file-selinux-context)
  ;; (elp-instrument-function 'tramp-sh-handle-set-file-selinux-context)
  ;; (elp-instrument-function 'tramp-sh-handle-file-acl)
  ;; (elp-instrument-function 'tramp-sh-handle-set-file-acl)
  ;; (elp-instrument-function 'tramp-sh-handle-file-executable-p)
  ;; (elp-instrument-function 'tramp-sh-handle-file-readable-p)
  ;; (elp-instrument-function 'tramp-sh-handle-file-newer-than-file-p)
  ;; (elp-instrument-function 'tramp-sh-handle-file-directory-p)
  ;; (elp-instrument-function 'tramp-sh-handle-file-writable-p)
  ;; (elp-instrument-function 'tramp-sh-handle-file-ownership-preserved-p)
  ;; (elp-instrument-function 'tramp-sh-handle-directory-files-and-attributes)
  ;; (elp-instrument-function 'tramp-sh-handle-file-name-all-completions)
  ;; (elp-instrument-function 'tramp-sh-handle-add-name-to-file)
  ;; (elp-instrument-function 'tramp-sh-handle-copy-file)
  ;; (elp-instrument-function 'tramp-sh-handle-copy-directory)
  ;; (elp-instrument-function 'tramp-sh-handle-rename-file)
  ;; (elp-instrument-function 'tramp-sh-handle-make-directory)
  ;; (elp-instrument-function 'tramp-sh-handle-delete-directory)
  ;; (elp-instrument-function 'tramp-sh-handle-delete-file)
  ;; (elp-instrument-function 'tramp-sh-handle-dired-recursive-delete-directory)
  ;; (elp-instrument-function 'tramp-sh-handle-dired-compress-file)
  ;; (elp-instrument-function 'tramp-sh-handle-insert-directory)
  ;; (elp-instrument-function 'tramp-sh-handle-start-file-process)
  ;; (elp-instrument-function 'tramp-sh-handle-file-local-copy)
  ;; (elp-instrument-function 'tramp-sh-handle-insert-file-contents-literally)
  ;; (elp-instrument-function 'tramp-sh-handle-write-region)
  ;; (elp-instrument-function 'tramp-sh-handle-vc-registered)
  ;; (elp-instrument-function 'tramp-sh-handle-file-notify-add-watch)
  ;;;
  ;; (elp-instrument-package "tramp-")
  ;; (elp-set-master 'projectile-project-root)
  )

(defun bench-elp-end ()
  (sit-for 0.1)
  (message "")
  (elp-results)
  (sit-for 0.1)
  (elp-restore-all))


(provide 'bench-helper)
