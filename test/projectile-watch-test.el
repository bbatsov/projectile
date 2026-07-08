;;; projectile-watch-test.el --- Tests for file-notify cache updates -*- lexical-binding: t -*-

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

;; Tests for the opt-in file-notify based cache updates
;; (`projectile-auto-update-cache-with-watches').  Most specs spy on
;; `file-notify-add-watch' / `file-notify-rm-watch' and drive the event
;; pipeline with synthetic events, so they don't depend on any specific
;; file notification backend; one integration spec at the bottom
;; exercises a real watch when the running Emacs provides a backend.

;;; Code:

(require 'projectile-test-helpers)

(defmacro projectile-watch-test-env (&rest body)
  "Evaluate BODY with fresh watch/cache state and the watch feature enabled.
All the hash-table registries involved are rebound so specs can't leak
watch bookkeeping into each other, caching is on (transient) and
`projectile-auto-update-cache-with-watches' is non-nil."
  (declare (indent 0) (debug (&rest form)))
  `(let ((projectile-projects-cache (make-hash-table :test 'equal))
         (projectile-projects-cache-time (make-hash-table :test 'equal))
         (projectile--project-watches (make-hash-table :test 'equal))
         (projectile--watch-pending-events (make-hash-table :test 'equal))
         (projectile--watch-debounce-timers (make-hash-table :test 'equal))
         (projectile--watch-skipped-projects (make-hash-table :test 'equal))
         (projectile--pending-cache-flush-timers (make-hash-table :test 'equal))
         (projectile--dirconfig-cache (make-hash-table :test 'equal))
         (projectile-auto-update-cache-with-watches t)
         (projectile-enable-caching t)
         (projectile-indexing-method 'native))
     ,@body))

(defun projectile-watch-test--spy-file-notify ()
  "Spy on the file-notify entry points with fake integer descriptors."
  (let ((counter 0))
    (spy-on 'file-notify-add-watch :and-call-fake
            (lambda (&rest _) (setq counter (1+ counter))))
    (spy-on 'file-notify-rm-watch)))

(defun projectile-watch-test--descriptor-for (project dir)
  "Return the registered descriptor for DIR (relative to PROJECT)."
  (let ((absolute (file-name-as-directory (expand-file-name dir project))))
    (car (rassoc absolute (gethash project projectile--project-watches)))))

(describe "projectile--watch-directories"
  (it "derives the project root and the directory chain of every cached file"
    (let ((root "/tmp/watched-project/"))
      (expect (projectile--watch-directories
               root '("a.el" "src/b.el" "src/sub/c.el" "doc/d.adoc"))
              :to-have-same-items-as
              (list root
                    (concat root "doc/")
                    (concat root "src/")
                    (concat root "src/sub/"))))))

(describe "projectile--watch-project"
  (it "registers one watch per directory when the file list is cached"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el" "src/sub/c.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el" "src/sub/c.el"))
          (expect (spy-calls-count 'file-notify-add-watch) :to-equal 3)
          (expect (mapcar #'cdr (gethash root projectile--project-watches))
                  :to-have-same-items-as
                  (list root (concat root "src/") (concat root "src/sub/")))))))

  (it "skips directories that no longer exist on disk"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          ;; "ghost/" is in the cached list but was never created.
          (projectile-cache-project root '("a.el" "ghost/g.el"))
          (expect (spy-calls-count 'file-notify-add-watch) :to-equal 1)
          (expect (mapcar #'cdr (gethash root projectile--project-watches))
                  :to-equal (list root))))))

  (it "does nothing when the option is disabled"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((projectile-auto-update-cache-with-watches nil)
              (root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (expect 'file-notify-add-watch :not :to-have-been-called)
          (expect (hash-table-count projectile--project-watches) :to-equal 0)))))

  (it "never watches remote projects"
    (projectile-watch-test-env
      (projectile-watch-test--spy-file-notify)
      (projectile--maybe-watch-project "/ssh:host:/home/user/project/" '("a.el"))
      (expect 'file-notify-add-watch :not :to-have-been-called)
      (expect (hash-table-count projectile--project-watches) :to-equal 0)))

  (it "skips projects with more directories than projectile-watch-directory-limit"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (spy-on 'message)
        (let ((projectile-watch-directory-limit 1)
              (projectile-verbose t)
              (root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el"))
          (projectile-cache-project root '("a.el" "src/b.el"))
          (expect 'file-notify-add-watch :not :to-have-been-called)
          (expect (hash-table-count projectile--project-watches) :to-equal 0)
          ;; The skip is reported only once per project.
          (expect (spy-calls-count 'message) :to-equal 1)))))

  (it "rolls back the partial registration when arming fails midway"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el" "doc/c.txt")
      (projectile-watch-test-env
        ;; The first two directories register fine, the third one errors
        ;; (as when the OS runs out of inotify watches).
        (let ((counter 0))
          (spy-on 'file-notify-add-watch :and-call-fake
                  (lambda (&rest _)
                    (setq counter (1+ counter))
                    (if (>= counter 3)
                        (error "No file notification program found")
                      counter))))
        (spy-on 'file-notify-rm-watch)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el" "doc/c.txt"))
          ;; Both successfully registered descriptors were removed again...
          (expect 'file-notify-rm-watch :to-have-been-called-with 1)
          (expect 'file-notify-rm-watch :to-have-been-called-with 2)
          ;; ...no registry entry survived, and the project was marked
          ;; skipped so the failure isn't retried on every cache fill.
          (expect (hash-table-count projectile--project-watches) :to-equal 0)
          (expect (gethash root projectile--watch-skipped-projects)
                  :not :to-be nil)))))

  (it "replaces the previous watches when the project is re-cached"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el"))
          (let ((old (mapcar #'car (gethash root projectile--project-watches))))
            (projectile-cache-project root '("a.el"))
            (dolist (descriptor old)
              (expect 'file-notify-rm-watch :to-have-been-called-with descriptor))
            (expect (mapcar #'cdr (gethash root projectile--project-watches))
                    :to-equal (list root))))))))

(describe "projectile--handle-watch-event"
  (it "queues events and schedules a single debounce timer per project"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (let ((descriptor (projectile-watch-test--descriptor-for root ".")))
            (projectile--handle-watch-event
             root (list descriptor 'created (expand-file-name "b.el" root)))
            (projectile--handle-watch-event
             root (list descriptor 'created (expand-file-name "c.el" root)))
            (expect (length (gethash root projectile--watch-pending-events))
                    :to-equal 2)
            (expect (hash-table-count projectile--watch-debounce-timers)
                    :to-equal 1))
          (projectile--unwatch-project root)))))

  (it "drops events from descriptors that are no longer registered"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (projectile--handle-watch-event
           root (list 'bogus 'created (expand-file-name "b.el" root)))
          (expect (gethash root projectile--watch-pending-events) :to-be nil)
          (expect (hash-table-count projectile--watch-debounce-timers)
                  :to-equal 0))))))

(describe "projectile--process-watch-events"
  ;; Specs enqueue synthetic events and run the (normally timer-driven)
  ;; processing function directly, so no timing is involved.
  (it "adds a created file to the cached file list"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (with-temp-file (expand-file-name "b.el" root))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'created (expand-file-name "b.el" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache)
                  :to-have-same-items-as '("a.el" "b.el"))
          (projectile--unwatch-project root)))))

  (it "does not add ignored files"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((projectile-globally-ignored-file-suffixes '(".log"))
              (root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (with-temp-file (expand-file-name "noise.log" root))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'created (expand-file-name "noise.log" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-equal '("a.el"))
          (projectile--unwatch-project root)))))

  (it "ignores editor artifacts like lockfiles, auto-saves and backups"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (let ((descriptor (projectile-watch-test--descriptor-for root ".")))
            (dolist (name (list ".#a.el" "#a.el#" "a.el~" projectile-cache-file))
              (projectile--handle-watch-event
               root (list descriptor 'created (expand-file-name name root)))))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-equal '("a.el"))
          (projectile--unwatch-project root)))))

  (it "removes a deleted file from the cached file list"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el"))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'deleted (expand-file-name "a.el" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-equal '("src/b.el"))
          (projectile--unwatch-project root)))))

  (it "prunes a deleted directory's files and drops its watch"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el" "src/c.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el" "src/c.el"))
          (let ((src-descriptor (projectile-watch-test--descriptor-for root "src")))
            (projectile--handle-watch-event
             root (list (projectile-watch-test--descriptor-for root ".")
                        'deleted (expand-file-name "src" root)))
            (projectile--process-watch-events root)
            (expect (gethash root projectile-projects-cache) :to-equal '("a.el"))
            (expect 'file-notify-rm-watch :to-have-been-called-with src-descriptor)
            (expect (mapcar #'cdr (gethash root projectile--project-watches))
                    :to-equal (list root)))
          (projectile--unwatch-project root)))))

  (it "treats a rename as a deletion plus a creation"
    (projectile-test-with-stub-root "project" ("a.el" "renamed.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          ;; "renamed.el" exists on disk but isn't cached yet - as if "a.el"
          ;; had just been moved onto it outside Emacs.
          (projectile-cache-project root '("a.el"))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'renamed
                      (expand-file-name "a.el" root)
                      (expand-file-name "renamed.el" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache)
                  :to-equal '("renamed.el"))
          (projectile--unwatch-project root)))))

  (it "does not cache the destination of a rename that leaves the project"
    (projectile-test-with-stub-root "project" ("a.el" "b.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "b.el"))
          ;; "a.el" was moved out of the project, next to its root.
          (let ((destination (expand-file-name "../a.el" root)))
            (with-temp-file destination)
            (projectile--handle-watch-event
             root (list (projectile-watch-test--descriptor-for root ".")
                        'renamed (expand-file-name "a.el" root) destination)))
          (projectile--process-watch-events root)
          ;; The source is gone from the cache; the destination (which
          ;; would show up as "../a.el") was not added.
          (expect (gethash root projectile-projects-cache) :to-equal '("b.el"))
          (projectile--unwatch-project root)))))

  (it "does not duplicate a cache entry on duplicate created events"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (with-temp-file (expand-file-name "b.el" root))
          (let ((descriptor (projectile-watch-test--descriptor-for root "."))
                (file (expand-file-name "b.el" root)))
            (projectile--handle-watch-event root (list descriptor 'created file))
            (projectile--handle-watch-event root (list descriptor 'created file)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache)
                  :to-have-same-items-as '("a.el" "b.el"))
          (projectile--unwatch-project root)))))

  (it "invalidates the cache when the project root itself is deleted"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'deleted (directory-file-name root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-be nil)
          ;; No zombie registry entry either - not even a nil one.
          (expect (hash-table-count projectile--project-watches) :to-equal 0)))))

  (it "keeps watching a project whose cached file list becomes empty"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root))
              (descriptor nil))
          (projectile-cache-project root '("a.el"))
          (setq descriptor (projectile-watch-test--descriptor-for root "."))
          (projectile--handle-watch-event
           root (list descriptor 'deleted (expand-file-name "a.el" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-be nil)
          (expect (gethash root projectile--project-watches) :not :to-be nil)
          ;; The genuinely-empty project still applies later events.
          (with-temp-file (expand-file-name "b.el" root))
          (projectile--handle-watch-event
           root (list descriptor 'created (expand-file-name "b.el" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-equal '("b.el"))
          (projectile--unwatch-project root)))))

  (it "adopts a newly created directory: watches it and caches its files"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          ;; Simulate `mkdir newdir && cp inside.el newdir/' - the file inside
          ;; produces no event of its own because the watch comes later.
          (make-directory (expand-file-name "newdir" root))
          (with-temp-file (expand-file-name "newdir/inside.el" root))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'created (expand-file-name "newdir" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache)
                  :to-have-same-items-as '("a.el" "newdir/inside.el"))
          (expect (projectile-watch-test--descriptor-for root "newdir")
                  :not :to-be nil)
          (projectile--unwatch-project root)))))

  (it "invalidates the cache when a new directory would exceed the watch limit"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((projectile-watch-directory-limit 1)
              (root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (make-directory (expand-file-name "newdir" root))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'created (expand-file-name "newdir" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-be nil)
          (expect (gethash root projectile--project-watches) :to-be nil)))))

  (it "invalidates the cache when a watch stops while its directory survives"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el"))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root "src") 'stopped
                      (expand-file-name "src" root)))
          (projectile--process-watch-events root)
          (expect (gethash root projectile-projects-cache) :to-be nil)
          (expect (gethash root projectile--project-watches) :to-be nil)))))

  (it "only drops the bookkeeping when a watch stops for a vanished directory"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el"))
          (let ((src-descriptor (projectile-watch-test--descriptor-for root "src")))
            (delete-directory (expand-file-name "src" root) t)
            (projectile--handle-watch-event
             root (list src-descriptor 'stopped (expand-file-name "src" root)))
            (projectile--process-watch-events root)
            ;; No invalidation: the parent watch's `deleted' event (tested
            ;; separately) is responsible for pruning the file list.
            (expect (gethash root projectile-projects-cache)
                    :to-equal '("a.el" "src/b.el"))
            (expect (mapcar #'cdr (gethash root projectile--project-watches))
                    :to-equal (list root)))
          (projectile--unwatch-project root)))))

  (it "drops the watches of a project whose cache entry vanished"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'created (expand-file-name "b.el" root)))
          ;; Simulate the cache-TTL eviction, which bypasses invalidation.
          (remhash root projectile-projects-cache)
          (projectile--process-watch-events root)
          (expect (gethash root projectile--project-watches) :to-be nil)))))

  (it "schedules a persistent cache flush after mutating the file list"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (spy-on 'projectile--schedule-cache-flush)
        (let ((projectile-enable-caching 'persistent)
              (root (projectile-project-root)))
          (puthash root '("a.el") projectile-projects-cache)
          (projectile--watch-project root '("a.el"))
          (with-temp-file (expand-file-name "b.el" root))
          (projectile--handle-watch-event
           root (list (projectile-watch-test--descriptor-for root ".")
                      'created (expand-file-name "b.el" root)))
          (projectile--process-watch-events root)
          (expect 'projectile--schedule-cache-flush :to-have-been-called-with root)
          (projectile--unwatch-project root))))))

(describe "watch cleanup"
  (it "projectile--invalidate-project-cache drops the project's watches"
    (projectile-test-with-stub-root "project" ("a.el" "src/b.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el" "src/b.el"))
          (let ((descriptors (mapcar #'car (gethash root projectile--project-watches))))
            (projectile--invalidate-project-cache root)
            (dolist (descriptor descriptors)
              (expect 'file-notify-rm-watch :to-have-been-called-with descriptor)))
          (expect (gethash root projectile--project-watches) :to-be nil)
          (expect (gethash root projectile--watch-pending-events) :to-be nil)
          (expect (gethash root projectile--watch-debounce-timers) :to-be nil)))))

  (it "re-arms the watches when the cache is filled again after invalidation"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (projectile--invalidate-project-cache root)
          (expect (gethash root projectile--project-watches) :to-be nil)
          (projectile-cache-project root '("a.el"))
          (expect (gethash root projectile--project-watches) :not :to-be nil)))))

  (it "customizing the option to nil tears down all watches"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root)))
          (projectile-cache-project root '("a.el"))
          (funcall (get 'projectile-auto-update-cache-with-watches 'custom-set)
                   'projectile-auto-update-cache-with-watches nil)
          (expect projectile-auto-update-cache-with-watches :to-be nil)
          (expect (hash-table-count projectile--project-watches) :to-equal 0)))))

  (it "customizing the option to t arms watches for already-cached projects"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((projectile-auto-update-cache-with-watches nil)
              (projectile-mode t)
              (root (projectile-project-root)))
          (puthash root '("a.el") projectile-projects-cache)
          (funcall (get 'projectile-auto-update-cache-with-watches 'custom-set)
                   'projectile-auto-update-cache-with-watches t)
          (expect (gethash root projectile--project-watches) :not :to-be nil)))))

  (it "does not arm watches when the mode is off"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((projectile-auto-update-cache-with-watches nil)
              (projectile-mode nil)
              (root (projectile-project-root)))
          (puthash root '("a.el") projectile-projects-cache)
          ;; arming watches with the mode off would leave them with no
          ;; mode-disable teardown to clean them up
          (funcall (get 'projectile-auto-update-cache-with-watches 'custom-set)
                   'projectile-auto-update-cache-with-watches t)
          (expect (gethash root projectile--project-watches) :to-be nil)))))

  (it "disabling projectile-mode tears down all watches"
    (spy-on 'projectile--teardown-all-watches)
    (projectile-mode -1)
    (expect 'projectile--teardown-all-watches :to-have-been-called))

  (it "re-enabling projectile-mode re-arms the watches of cached projects"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (let ((root (projectile-project-root))
              (was-enabled projectile-mode))
          (unwind-protect
              (progn
                (projectile-cache-project root '("a.el"))
                (projectile-mode -1)
                (expect (hash-table-count projectile--project-watches)
                        :to-equal 0)
                ;; The cache is still warm, so no cache fill will re-arm the
                ;; watches; enabling the mode has to do it directly.
                (projectile-mode +1)
                (expect (gethash root projectile--project-watches)
                        :not :to-be nil)
                (projectile--unwatch-project root))
            (projectile-mode (if was-enabled +1 -1)))))))

  (it "removing a known project drops its watches"
    (projectile-test-with-stub-root "project" ("a.el")
      (projectile-watch-test-env
        (projectile-watch-test--spy-file-notify)
        (spy-on 'projectile-merge-known-projects)
        (let* ((root (projectile-project-root))
               (projectile-known-projects (list root)))
          (projectile-cache-project root '("a.el"))
          (projectile-remove-known-project root)
          (expect (gethash root projectile--project-watches) :to-be nil))))))

(describe "file watch integration"
  (it "picks up a file created outside Emacs in a watched project"
    (assume (bound-and-true-p file-notify--library)
            "no file notification backend available")
    (projectile-test-with-sandbox
      (projectile-test-with-files ("project/src/a.el")
        (projectile-watch-test-env
          (let ((root (file-name-as-directory
                       (file-truename (expand-file-name "project")))))
            (spy-on 'projectile-project-root :and-return-value root)
            (projectile-cache-project root '("src/a.el"))
            (expect (gethash root projectile--project-watches) :not :to-be nil)
            ;; Created behind Emacs's back: no find-file-hook involved.
            (with-temp-file (expand-file-name "src/b.el" root))
            ;; Wait out event delivery plus the 0.5s debounce.  File
            ;; notifications are delivered as *input* events (not process
            ;; output), so the loop must pump `read-event' - which also
            ;; lets the debounce timer fire - rather than
            ;; `accept-process-output' à la `projectile-test-wait-for'.
            (let ((deadline (+ (float-time) 10)))
              (while (and (not (member "src/b.el"
                                       (gethash root projectile-projects-cache)))
                          (< (float-time) deadline))
                (read-event nil nil 0.1)))
            (expect (gethash root projectile-projects-cache)
                    :to-have-same-items-as '("src/a.el" "src/b.el"))
            (projectile--unwatch-project root)))))))

(provide 'projectile-watch-test)

;;; projectile-watch-test.el ends here
