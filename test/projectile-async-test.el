;;; projectile-async-test.el --- Tests for asynchronous project indexing -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov

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

;; Tests for the asynchronous file-indexing primitives:
;; `projectile-files-via-ext-command-async', `projectile-dir-files-alien-async'
;; and `projectile-index-project-async'.

;;; Code:

(require 'projectile-test-helpers)

;; A captured (FILES . ERROR) result from an async callback, plus a `done'
;; flag so `projectile-test-wait-for' knows when the sentinel has fired.
(defmacro projectile-test-with-async-result (result-var &rest body)
  "Bind RESULT-VAR to a fresh result cell and evaluate BODY.
The cell is a list (DONE FILES ERROR); pass `(lambda (files err) ...)'
that stores into it as the async callback."
  (declare (indent 1) (debug (sexp &rest form)))
  `(let ((,result-var (list nil nil nil)))
     ,@body))

(describe "projectile-files-via-ext-command-async"
  (it "returns the parsed NUL-separated file list to the callback"
    (projectile-test-with-async-result result
      (projectile-files-via-ext-command-async
       temporary-file-directory "printf 'a.el\\0b/c.el\\0'"
       (lambda (files err)
         (setf (nth 1 result) files (nth 2 result) err (nth 0 result) t)))
      (expect (projectile-test-wait-for (lambda () (nth 0 result))) :to-be-truthy)
      (expect (nth 1 result) :to-equal '("a.el" "b/c.el"))
      (expect (nth 2 result) :to-be nil)))

  (it "strips a leading ./ from each record"
    (projectile-test-with-async-result result
      (projectile-files-via-ext-command-async
       temporary-file-directory "printf './x\\0./y/z\\0'"
       (lambda (files err)
         (setf (nth 1 result) files (nth 2 result) err (nth 0 result) t)))
      (expect (projectile-test-wait-for (lambda () (nth 0 result))) :to-be-truthy)
      (expect (nth 1 result) :to-equal '("x" "y/z"))))

  (it "appends shell-quoted pathspecs to the command"
    (projectile-test-with-async-result result
      (projectile-files-via-ext-command-async
       temporary-file-directory "printf 'args: %s\\0' --"
       (lambda (files err)
         (setf (nth 1 result) files (nth 2 result) err (nth 0 result) t))
       '("src dir" "test"))
      (expect (projectile-test-wait-for (lambda () (nth 0 result))) :to-be-truthy)
      (expect (nth 1 result) :to-equal '("args: --" "args: src dir" "args: test"))))

  (it "reports an error (and leaves files nil) when the command exits non-zero"
    (projectile-test-with-async-result result
      (projectile-files-via-ext-command-async
       temporary-file-directory "echo boom >&2; exit 3"
       (lambda (files err)
         (setf (nth 1 result) files (nth 2 result) err (nth 0 result) t)))
      (expect (projectile-test-wait-for (lambda () (nth 0 result))) :to-be-truthy)
      (expect (nth 1 result) :to-be nil)
      (expect (nth 2 result) :to-match "exit code 3")
      ;; stderr was captured into the shared errors buffer
      (expect (with-current-buffer "*projectile-files-errors*" (buffer-string))
              :to-match "boom")))

  (it "invokes the callback with an empty list for a disabled (nil/empty) command"
    (projectile-test-with-async-result result
      (expect (projectile-files-via-ext-command-async
               temporary-file-directory ""
               (lambda (files err)
                 (setf (nth 1 result) files (nth 2 result) err (nth 0 result) t)))
              :to-be nil)
      ;; called synchronously, no process spawned
      (expect (nth 0 result) :to-be-truthy)
      (expect (nth 1 result) :to-be nil)
      (expect (nth 2 result) :to-be nil)))

  (it "does not report failure for a process aborted mid-flight"
    ;; When a consumer (e.g. the responsive await on C-g) marks the process
    ;; aborted and kills it, the sentinel must clean up silently rather than
    ;; invoke the callback with a spurious failure.
    (let ((called nil))
      (let ((proc (projectile-files-via-ext-command-async
                   temporary-file-directory "echo boom >&2; exit 1"
                   (lambda (&rest _) (setq called t)))))
        ;; `process-put' happens before any sentinel can run (sentinels fire
        ;; from the event loop, not synchronously from make-process).
        (process-put proc 'projectile-aborted t)
        (projectile-test-wait-for
         (lambda () (memq (process-status proc) '(exit signal))))
        (accept-process-output nil 0.05)
        (expect called :to-be nil))))

  (it "reports an error when the process cannot be started (e.g. remote unsupported)"
    ;; A file-name handler may decline make-process and return nil; we must
    ;; still honour the callback contract instead of going silent.
    (spy-on 'make-process :and-return-value nil)
    (projectile-test-with-async-result result
      (expect (projectile-files-via-ext-command-async
               temporary-file-directory "echo hi"
               (lambda (files err)
                 (setf (nth 1 result) files (nth 2 result) err (nth 0 result) t)))
              :to-be nil)
      (expect (nth 0 result) :to-be-truthy)
      (expect (nth 1 result) :to-be nil)
      (expect (nth 2 result) :to-be-truthy))))

(describe "projectile-dir-files-alien-async"
  (it "assembles main + submodule files and removes deleted ones (git)"
    (spy-on 'projectile-get-ext-command :and-return-value "the-command")
    (spy-on 'projectile-fd-executable-for :and-return-value nil)
    (spy-on 'projectile--restricted-sub-projects-files :and-return-value '("sub.el"))
    (spy-on 'projectile-git-deleted-files :and-return-value '("gone.el"))
    ;; Stand in for the real process: invoke the callback synchronously.
    (spy-on 'projectile-files-via-ext-command-async :and-call-fake
            (lambda (_dir _command callback &optional _pathspecs)
              ;; Build the list with `list' (not a quoted literal): the git
              ;; assembly `nconc's onto it, which must not mutate a constant.
              (funcall callback (list "keep.el" "gone.el") nil)
              'fake-process))
    (let (captured)
      (projectile-dir-files-alien-async
       "/root/" (lambda (files err) (setq captured (cons files err))) 'git)
      ;; "gone.el" dropped, "sub.el" appended
      (expect (car captured) :to-equal '("keep.el" "sub.el"))
      (expect (cdr captured) :to-be nil)))

  (it "propagates an error from the underlying command (git)"
    (spy-on 'projectile-get-ext-command :and-return-value "the-command")
    (spy-on 'projectile-fd-executable-for :and-return-value nil)
    (spy-on 'projectile--restricted-sub-projects-files)
    (spy-on 'projectile-git-deleted-files)
    (spy-on 'projectile-files-via-ext-command-async :and-call-fake
            (lambda (_dir _command callback &optional _pathspecs)
              (funcall callback nil "kaboom")
              'fake-process))
    (let (captured)
      (projectile-dir-files-alien-async
       "/root/" (lambda (files err) (setq captured (cons files err))) 'git)
      (expect (car captured) :to-be nil)
      (expect (cdr captured) :to-equal "kaboom")
      ;; we never bother computing submodule/deleted files on failure
      (expect 'projectile-git-deleted-files :not :to-have-been-called)))

  (it "runs the bare command for a non-git VCS without git post-processing"
    (spy-on 'projectile-get-ext-command :and-return-value "the-command")
    (spy-on 'projectile-files-via-ext-command-async :and-call-fake
            (lambda (_dir _command callback &optional _pathspecs)
              (funcall callback '("a.txt") nil)
              'fake-process))
    (spy-on 'projectile-git-deleted-files)
    (let (captured)
      (projectile-dir-files-alien-async
       "/root/" (lambda (files err) (setq captured (cons files err))) 'none)
      (expect (car captured) :to-equal '("a.txt"))
      (expect 'projectile-git-deleted-files :not :to-have-been-called))))

(describe "projectile-index-project-async"
  (before-each
    (clrhash projectile--async-index-processes))

  (it "caches the indexed files on success and clears the registry"
    (spy-on 'projectile-dir-files-alien-async :and-call-fake
            (lambda (_root callback &rest _)
              (funcall callback '("f1.el" "f2.el") nil)
              nil))
    (let ((projectile-indexing-method 'alien)
          (projectile-enable-caching 'transient)
          (projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-projects-cache-time (make-hash-table :test 'equal)))
      (projectile-index-project-async "/proj/")
      (expect (gethash "/proj/" projectile-projects-cache) :to-equal '("f1.el" "f2.el"))
      (expect (gethash "/proj/" projectile--async-index-processes) :to-be nil)))

  (it "does not cache when the background command fails"
    (spy-on 'projectile-dir-files-alien-async :and-call-fake
            (lambda (_root callback &rest _)
              (funcall callback nil "boom")
              nil))
    (let ((projectile-indexing-method 'alien)
          (projectile-enable-caching 'transient)
          (projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-projects-cache-time (make-hash-table :test 'equal)))
      (projectile-index-project-async "/proj/")
      (expect (gethash "/proj/" projectile-projects-cache) :to-be nil)))

  (it "is a no-op under native indexing"
    (spy-on 'projectile-dir-files-alien-async)
    (let ((projectile-indexing-method 'native)
          (projectile-enable-caching 'transient))
      (expect (projectile-index-project-async "/proj/") :to-be nil)
      (expect 'projectile-dir-files-alien-async :not :to-have-been-called)))

  (it "is a no-op when caching is disabled"
    (spy-on 'projectile-dir-files-alien-async)
    (let ((projectile-indexing-method 'alien)
          (projectile-enable-caching nil))
      (expect (projectile-index-project-async "/proj/") :to-be nil)
      (expect 'projectile-dir-files-alien-async :not :to-have-been-called)))

  (it "refuses to start a second index while one is already running"
    (spy-on 'projectile-dir-files-alien-async)
    (spy-on 'process-live-p :and-return-value t)
    (puthash "/proj/" 'pretend-process projectile--async-index-processes)
    (let ((projectile-indexing-method 'alien)
          (projectile-enable-caching 'transient))
      (expect (projectile-index-project-async "/proj/") :to-be nil)
      (expect 'projectile-dir-files-alien-async :not :to-have-been-called)))

  (it "discards a stale result whose registry entry was cleared mid-run"
    (let ((projectile-indexing-method 'alien)
          (projectile-enable-caching 'transient)
          (projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-projects-cache-time (make-hash-table :test 'equal))
          captured)
      ;; Capture the callback without invoking it, and look like a real
      ;; process so the result is registered.
      (spy-on 'projectile-dir-files-alien-async :and-call-fake
              (lambda (_root callback &rest _) (setq captured callback) 'fake-proc))
      (spy-on 'processp :and-return-value t)
      (projectile-index-project-async "/proj/")
      (expect (gethash "/proj/" projectile--async-index-processes) :to-equal 'fake-proc)
      ;; Simulate an invalidation clearing the registry while it ran.
      (remhash "/proj/" projectile--async-index-processes)
      (funcall captured '("stale.el") nil)
      ;; The stale result must not repopulate the cleared cache.
      (expect (gethash "/proj/" projectile-projects-cache) :to-be nil))))

(describe "projectile-invalidate-cache with a background index"
  (before-each
    (clrhash projectile--async-index-processes))

  (it "cancels an in-flight background index for the invalidated project"
    (let ((projectile-enable-caching 'transient)
          (projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-projects-cache-time (make-hash-table :test 'equal)))
      (spy-on 'projectile-project-root :and-return-value "/proj/")
      (spy-on 'process-live-p :and-return-value t)
      (spy-on 'delete-process)
      (puthash "/proj/" 'fake-proc projectile--async-index-processes)
      (projectile-invalidate-cache nil)
      (expect 'delete-process :to-have-been-called-with 'fake-proc)
      (expect (gethash "/proj/" projectile--async-index-processes) :to-be nil))))

(describe "projectile--dir-files-alien-await"
  (it "returns the same files as the synchronous indexer (real git repo)"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/" "project/src/" "project/a.el" "project/src/b.el")
      (let ((default-directory (file-truename (expand-file-name "project/")))
            (projectile-git-use-fd nil))
        (call-process "git" nil nil nil "init")
        (call-process "git" nil nil nil "add" "-A")
        (let ((sync (sort (projectile-dir-files-alien default-directory) #'string<))
              (async (sort (copy-sequence
                            (projectile--dir-files-alien-await default-directory))
                           #'string<)))
          (expect async :to-equal sync)
          (expect async :to-contain "a.el")
          (expect async :to-contain "src/b.el"))))))

  (it "falls back to the synchronous indexer when no process can be started"
    ;; e.g. a remote handler that declines make-process: the async runner
    ;; returns nil and calls back with an error; we must still produce files.
    (spy-on 'projectile-dir-files-alien-async :and-call-fake
            (lambda (_dir callback &rest _)
              (funcall callback nil "could not start the indexing process")
              nil))
    (spy-on 'projectile-dir-files-alien :and-return-value '("fallback.el"))
    (expect (projectile--dir-files-alien-await "/proj/") :to-equal '("fallback.el"))
    (expect 'projectile-dir-files-alien :to-have-been-called))

  (it "signals a user-error, and kills the process, when indexing fails"
    (let ((proc (start-process "projectile-test-sleep" nil "sleep" "30")))
      ;; Hand back a live process (so the await path, not the can't-start
      ;; fallback, runs) but report failure straight away - deterministic,
      ;; no timer race.
      (spy-on 'projectile-dir-files-alien-async :and-call-fake
              (lambda (_dir callback &rest _)
                (funcall callback nil "exit code 1: cmd")
                proc))
      (expect (projectile--dir-files-alien-await "/proj/") :to-throw 'user-error)
      ;; the await must not leave its process running
      (expect (process-live-p proc) :to-be nil))))

(describe "projectile--dir-files-alien-maybe-async"
  (it "uses the synchronous indexer when async indexing is disabled"
    (spy-on 'projectile--dir-files-alien-await)
    (spy-on 'projectile-dir-files-alien :and-return-value '("x"))
    (let ((projectile-async-indexing nil))
      (expect (projectile--dir-files-alien-maybe-async "/proj/") :to-equal '("x")))
    (expect 'projectile--dir-files-alien-await :not :to-have-been-called))

  (it "uses the synchronous indexer in batch / macro contexts even when enabled"
    ;; The test process itself runs with `noninteractive' non-nil, which is
    ;; exactly the batch guard we want to exercise.
    (spy-on 'projectile--dir-files-alien-await)
    (spy-on 'projectile-dir-files-alien :and-return-value '("x"))
    (let ((projectile-async-indexing t))
      (expect (projectile--dir-files-alien-maybe-async "/proj/") :to-equal '("x")))
    (expect 'projectile--dir-files-alien-await :not :to-have-been-called))

  (it "uses the responsive async indexer when enabled and interactive"
    (spy-on 'projectile--dir-files-alien-await :and-return-value '("async"))
    (spy-on 'projectile-dir-files-alien :and-return-value '("sync"))
    ;; Pretend we're interactive by clearing the batch flag for the call.
    (let ((projectile-async-indexing t)
          (noninteractive nil)
          (executing-kbd-macro nil))
      (expect (projectile--dir-files-alien-maybe-async "/proj/") :to-equal '("async")))
    (expect 'projectile-dir-files-alien :not :to-have-been-called)))

;;; projectile-async-test.el ends here
