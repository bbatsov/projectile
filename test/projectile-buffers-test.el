;;; projectile-buffers-test.el --- Tests for project buffers -*- lexical-binding: t -*-

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

;; Tests for project buffers.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-find-file-hook-function"
  ;; The hook fans out into several operations; the contract under test
  ;; is which ones are gated behind the remote check.  Cheap ones must
  ;; run for remote buffers too; slow ones (mode-line / tags) must not.
  :var (called)
  (before-each
    (setq called nil)
    (cl-flet ((track (name) (lambda (&rest _) (push name called))))
      (spy-on 'projectile-maybe-limit-project-file-buffers
              :and-call-fake (track 'limit))
      (spy-on 'projectile-cache-files-find-file-hook
              :and-call-fake (track 'cache))
      (spy-on 'projectile-track-known-projects-find-file-hook
              :and-call-fake (track 'track))
      (spy-on 'projectile-update-mode-line
              :and-call-fake (track 'mode-line))
      (spy-on 'projectile-visit-project-tags-table
              :and-call-fake (track 'tags))))

  (it "runs every operation for local buffers"
    (let ((default-directory "/tmp/")
          (projectile-auto-update-cache t)
          (projectile-dynamic-mode-line t))
      (projectile-find-file-hook-function))
    (expect (memq 'limit called) :to-be-truthy)
    (expect (memq 'cache called) :to-be-truthy)
    (expect (memq 'track called) :to-be-truthy)
    (expect (memq 'mode-line called) :to-be-truthy)
    (expect (memq 'tags called) :to-be-truthy))

  (it "runs cheap operations for remote buffers but skips mode-line and tags"
    (let ((default-directory "/ssh:host:/proj/")
          (projectile-auto-update-cache t)
          (projectile-dynamic-mode-line t))
      (projectile-find-file-hook-function))
    ;; cheap ones still run
    (expect (memq 'limit called) :to-be-truthy)
    (expect (memq 'cache called) :to-be-truthy)
    (expect (memq 'track called) :to-be-truthy)
    ;; slow ones are skipped
    (expect (memq 'mode-line called) :not :to-be-truthy)
    (expect (memq 'tags called) :not :to-be-truthy)))

(describe "projectile-ignored-buffer-p"
  (it "checks if buffer should be ignored"
    (let ((projectile-globally-ignored-buffers '("*nrepl messages*" "*something*")))
      (expect (projectile-ignored-buffer-p (get-buffer-create "*nrepl messages*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "*something*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "test")) :not :to-be-truthy))))

(describe "projectile-process-current-project-buffers-current"
  (it "expects projectile-process-current-project-buffers and
projectile-process-current-project-buffers-current to have similar behaviour"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/"
       "projectA/.projectile"
       "projectA/bufferA"
       "projectA/fileA"
       "projectA/dirA/"
       "projectA/dirA/fileC")
      (let ((list-a '())
            (list-b '()))
        (projectile-process-current-project-buffers (lambda (b) (push b list-a)))
        (projectile-process-current-project-buffers-current (lambda () (push (current-buffer) list-b)))
        (expect list-a :to-equal list-b))))))

(describe "projectile-project-buffers"
          (it "return project buffers"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("project1/"
                 "project1/.projectile"
                 "project1/foo")
                (cd "project1")
                (with-current-buffer (find-file-noselect "foo" t))
                (expect (length (projectile-project-buffers)) :to-equal 1)))))

(describe "projectile-buffer-killed-p"
  (it "kills every buffer with the kill-all filter"
    (let ((projectile-kill-buffers-filter 'kill-all))
      (with-temp-buffer
        (expect (projectile-buffer-killed-p (current-buffer)) :to-be-truthy))))

  (it "kills only file-visiting buffers with the kill-only-files filter"
    (let ((projectile-kill-buffers-filter 'kill-only-files))
      (with-temp-buffer
        (expect (projectile-buffer-killed-p (current-buffer)) :not :to-be-truthy)
        (setq buffer-file-name "/tmp/projectile-killtest")
        (expect (projectile-buffer-killed-p (current-buffer)) :to-be-truthy))))

  (it "honors a predicate function filter"
    (let ((projectile-kill-buffers-filter
           (lambda (buf) (string-match-p "keep" (buffer-name buf)))))
      (with-current-buffer (get-buffer-create "keep-me")
        (expect (projectile-buffer-killed-p (current-buffer)) :to-be-truthy))
      (with-current-buffer (get-buffer-create "drop-me")
        (expect (projectile-buffer-killed-p (current-buffer)) :not :to-be-truthy))
      (kill-buffer "keep-me")
      (kill-buffer "drop-me")))

  (it "signals a user-error for an invalid filter value"
    (let ((projectile-kill-buffers-filter 42))
      (with-temp-buffer
        (expect (projectile-buffer-killed-p (current-buffer)) :to-throw 'user-error)))))

(describe "projectile--buffer-matches-conditions"
  (it "matches a buffer-name regexp condition"
    (with-current-buffer (get-buffer-create "*scratch-test*")
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '("\\`\\*scratch-test\\*\\'"))
              :to-be-truthy)
      (kill-buffer)))

  (it "matches a predicate-function condition"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/projectile-cond")
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '(buffer-file-name))
              :to-be-truthy)))

  (it "matches major-mode and derived-mode conditions"
    (with-temp-buffer
      (lisp-mode)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((major-mode . lisp-mode)))
              :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((major-mode . text-mode)))
              :not :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((derived-mode . prog-mode)))
              :to-be-truthy)))

  (it "composes conditions with and/or/not"
    (with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/projectile-compose")
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((and buffer-file-name (derived-mode . text-mode))))
              :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((and buffer-file-name (derived-mode . prog-mode))))
              :not :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((or (derived-mode . prog-mode) (derived-mode . text-mode))))
              :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((not (derived-mode . prog-mode))))
              :to-be-truthy)))

  (it "returns nil when no condition matches and for an empty list"
    (with-temp-buffer
      (fundamental-mode)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((derived-mode . prog-mode)))
              :not :to-be-truthy)
      (expect (projectile--buffer-matches-conditions (current-buffer) nil)
              :not :to-be-truthy))))

(describe "projectile-project-buffer-p"
  (it "uses the truename cache when provided"
    (let* ((project-root "/projects/foo/")
           (cache (make-hash-table :test 'equal)))
      (spy-on 'file-truename :and-call-fake (lambda (f) f))
      (with-temp-buffer
        (setq default-directory "/projects/foo/src/")
        (rename-buffer "test-buffer")
        (projectile-project-buffer-p (current-buffer) project-root cache)
        ;; The truename result should be cached
        (expect (gethash "/projects/foo/src/" cache) :to-equal "/projects/foo/src/")
        ;; A second call should use the cache, not call file-truename again
        (projectile-project-buffer-p (current-buffer) project-root cache)
        (expect 'file-truename :to-have-been-called-times 1))))

  (it "skips file-truename for buffers visiting remote files"
    ;; Each `file-truename' on a TRAMP path is a remote stat;
    ;; iterating `(buffer-list)' for a remote project should not
    ;; trigger any of them.
    (spy-on 'file-truename :and-call-fake (lambda (f) f))
    (with-temp-buffer
      (setq default-directory "/ssh:host:/proj/src/")
      (rename-buffer "remote-test-buffer")
      (expect (projectile-project-buffer-p (current-buffer) "/ssh:host:/proj/" nil)
              :to-be-truthy)
      (expect 'file-truename :not :to-have-been-called)))

  (it "does not match a remote buffer against a local project root"
    (with-temp-buffer
      (setq default-directory "/ssh:host:/proj/src/")
      (rename-buffer "remote-test-buffer-2")
      (expect (projectile-project-buffer-p (current-buffer) "/local/proj/" nil)
              :not :to-be-truthy))))

;; A bunch of tests that make sure Projectile commands handle
;; gracefully the case of being run outside of a project.

;;; projectile-buffers-test.el ends here
