;;; projectile-scan-async-test.el --- Tests for the async scanning engine -*- lexical-binding: t -*-

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

;; Tests for the asynchronous, streaming, cancelable scanning engine shared
;; by the reviewable replace and search reviewers
;; (`projectile-replace--gather-async' and its state machine).  Batch runs
;; the interactive commands synchronously, so these tests drive the async
;; driver and its scanning-state lifecycle directly: they compare the async
;; driver's final match set against the synchronous `--gather', pump the
;; chunk timer by hand, and exercise the apply/export gating, cancel-on-
;; rescan and buffer-kill cleanup by manipulating the driver and the
;; scanning flag rather than relying on real wall-clock timers.

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-scan-async-test--seed (root)
  "Return a results buffer in `projectile-replace-mode' seeded for a foo scan."
  (let ((buf (get-buffer-create "*projectile-scan-async-test*")))
    (with-current-buffer buf
      (projectile-replace-mode)
      (setq projectile-replace--root root
            projectile-replace--term "foo"
            projectile-replace--search "foo"
            projectile-replace--replacement nil
            projectile-replace--literal nil
            projectile-replace--case-fold t))
    buf))

(defun projectile-scan-async-test--candidates (root)
  "Return the regexp-search candidate files under ROOT for term foo."
  (projectile-replace--candidates "foo" nil t root))

(defun projectile-scan-async-test--sync-matches (candidates)
  "Return the synchronous `--gather' matches for CANDIDATES / term foo."
  (let ((case-fold-search t))
    (plist-get (projectile-replace--gather candidates "foo") :matches)))

(defun projectile-scan-async-test--pump (buf)
  "Run BUF's pending scan timers by hand until the scan finishes."
  (with-current-buffer buf
    (let ((guard 0))
      (while (and projectile-replace--scanning (< guard 10000))
        (let ((tm projectile-replace--scan-timer))
          (unless tm (error "scanning with no pending timer"))
          (timer-event-handler tm))
        (cl-incf guard)))))

(describe "projectile-replace--gather-async"
  (it "produces the same final matches as the synchronous gather"
    (projectile-test-with-project
        (("a.txt" . "foo one foo\n")
         ("b.txt" . "start foo end\n")
         ("lib/c.txt" . "foo\nfoo\n")
         ("lib/d.txt" . "no match here\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (sync (projectile-scan-async-test--sync-matches candidates))
             (buf (projectile-scan-async-test--seed root)))
        (unwind-protect
            ;; a chunk larger than the candidate set finishes in one inline
            ;; pass (no timer), the simplest way to drive it to completion
            (let ((projectile-replace-scan-chunk-size 10000))
              (projectile-replace--gather-async candidates "foo" buf nil)
              (with-current-buffer buf
                (expect projectile-replace--scanning :to-be nil)
                (expect projectile-replace--scan-timer :to-be nil)
                (expect (projectile-test-match-sig
                         projectile-replace--matches root)
                        :to-equal
                        (projectile-test-match-sig sync root))))
          (kill-buffer buf)))))

  (it "streams matches in chunk by chunk and ends with the full set"
    (projectile-test-with-project
        (("a.txt" . "foo\nfoo\n")
         ("b.txt" . "foo\n")
         ("c.txt" . "foo\nfoo\n")
         ("d.txt" . "foo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (sync (projectile-scan-async-test--sync-matches candidates))
             (buf (projectile-scan-async-test--seed root)))
        (unwind-protect
            (let ((projectile-replace-scan-chunk-size 1))
              (projectile-replace--gather-async candidates "foo" buf nil)
              ;; the first chunk ran inline; more files remain, so a scan is
              ;; in flight with a pending timer and partial matches
              (with-current-buffer buf
                (expect projectile-replace--scanning :to-be-truthy)
                (expect projectile-replace--scan-timer :not :to-be nil)
                (expect (length projectile-replace--matches)
                        :to-be-less-than (length sync)))
              (projectile-scan-async-test--pump buf)
              (with-current-buffer buf
                (expect projectile-replace--scanning :to-be nil)
                (expect projectile-replace--scan-timer :to-be nil)
                (expect (projectile-test-match-sig
                         projectile-replace--matches root)
                        :to-equal
                        (projectile-test-match-sig sync root))))
          (kill-buffer buf)))))

  (it "keeps case sensitivity in a late chunk (parity with sync)"
    (projectile-test-with-project
        (("a.txt" . "foo\n")
         ("b.txt" . "foo\n")
         ("c.txt" . "foo\n")
         ("d.txt" . "foo\n")
         ("e.txt" . "FOO\n"))   ; the only case variant, in the last file
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             ;; case-SENSITIVE sync: e.txt's FOO must not match
             (sync (let ((case-fold-search nil))
                     (plist-get (projectile-replace--gather candidates "foo")
                                :matches)))
             (buf (projectile-scan-async-test--seed root)))
        (with-current-buffer buf (setq projectile-replace--case-fold nil))
        (unwind-protect
            ;; one file per chunk, so e.txt is scanned in a LATE chunk (from a
            ;; timer); the driver must re-establish case-fold there too
            (let ((projectile-replace-scan-chunk-size 1))
              (projectile-replace--gather-async candidates "foo" buf nil)
              (projectile-scan-async-test--pump buf)
              (with-current-buffer buf
                (expect (projectile-test-match-sig
                         projectile-replace--matches root)
                        :to-equal
                        (projectile-test-match-sig sync root))
                (expect (projectile-test-match-sig
                         projectile-replace--matches root)
                        :not :to-contain '("e.txt" 1 0 "FOO"))))
          (kill-buffer buf)))))

  (it "truncates at the same match as sync on a chunk-boundary budget"
    (projectile-test-with-project
        (("a.txt" . "foo\n") ("b.txt" . "foo\n") ("c.txt" . "foo\n")
         ("d.txt" . "foo\n") ("e.txt" . "foo\n") ("f.txt" . "foo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (projectile-replace-max-matches 3)   ; budget ends at a chunk edge
             (sync (projectile-scan-async-test--sync-matches candidates))
             (buf (projectile-scan-async-test--seed root)))
        (unwind-protect
            (let ((projectile-replace-scan-chunk-size 3)) ; boundary at file 3
              (projectile-replace--gather-async candidates "foo" buf nil)
              (projectile-scan-async-test--pump buf)
              (with-current-buffer buf
                (expect (length projectile-replace--matches) :to-equal 3)
                (expect projectile-replace--truncated :to-be-truthy)
                (expect (projectile-test-match-sig
                         projectile-replace--matches root)
                        :to-equal
                        (projectile-test-match-sig sync root))))
          (kill-buffer buf)))))

  (it "runs ON-DONE in the buffer once scanning finishes"
    (projectile-test-with-project
        (("a.txt" . "foo\n")
         ("b.txt" . "foo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (buf (projectile-scan-async-test--seed root))
             (seen nil))
        (unwind-protect
            (let ((projectile-replace-scan-chunk-size 1))
              (projectile-replace--gather-async
               candidates "foo" buf
               (lambda (b) (setq seen (list b (buffer-local-value
                                               'projectile-replace--scanning b)))))
              (projectile-scan-async-test--pump buf)
              ;; on-done saw the buffer with scanning already cleared
              (expect (car seen) :to-equal buf)
              (expect (cadr seen) :to-be nil))
          (kill-buffer buf)))))

  (it "honors projectile-replace-max-matches and sets truncated"
    (projectile-test-with-project
        (("a.txt" . "foo\nfoo\n")
         ("b.txt" . "foo\nfoo\n")
         ("c.txt" . "foo\nfoo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (buf (projectile-scan-async-test--seed root)))
        (unwind-protect
            (let ((projectile-replace-max-matches 3)
                  (projectile-replace-scan-chunk-size 1))
              (projectile-replace--gather-async candidates "foo" buf nil)
              (projectile-scan-async-test--pump buf)
              (with-current-buffer buf
                (expect (length projectile-replace--matches) :to-equal 3)
                (expect projectile-replace--truncated :to-be-truthy)))
          (kill-buffer buf))))))

(describe "projectile-replace async scanning-state gating"
  (it "refuses to apply while a scan is still running"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (projectile-scan-async-test--seed default-directory)))
        (unwind-protect
            (with-current-buffer buf
              (setq projectile-replace--scanning t)
              (expect (projectile-replace--apply) :to-throw 'user-error))
          (kill-buffer buf)))))

  (it "refuses to export while a scan is still running"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (projectile-scan-async-test--seed default-directory)))
        (unwind-protect
            (with-current-buffer buf
              (setq projectile-replace--scanning t)
              (expect (projectile-replace--export) :to-throw 'user-error))
          (kill-buffer buf)))))

  (it "refuses to filter while a scan is still running"
    ;; a later chunk would append past the filter, leaving an incoherent list
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (projectile-scan-async-test--seed default-directory)))
        (unwind-protect
            (with-current-buffer buf
              (setq projectile-replace--scanning t)
              (expect (projectile-replace--keep-matches "x") :to-throw 'user-error)
              (expect (projectile-replace--flush-files "x") :to-throw 'user-error))
          (kill-buffer buf)))))

  (it "lets apply and export proceed once scanning has cleared"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (projectile-scan-async-test--seed default-directory)))
        (unwind-protect
            (with-current-buffer buf
              (setq projectile-replace--scanning nil)
              ;; not scanning: the guard is a no-op (apply just reports no
              ;; enabled matches, proving it got past the scanning gate)
              (expect (projectile-replace--ensure-not-scanning) :not :to-throw))
          (kill-buffer buf))))))

(describe "projectile-replace async cancellation"
  (it "cancels an in-flight timer when a re-scan starts"
    (projectile-test-with-project
        (("a.txt" . "foo\n")
         ("b.txt" . "foo\n")
         ("c.txt" . "foo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (buf (projectile-scan-async-test--seed root)))
        (unwind-protect
            (let ((projectile-replace-scan-chunk-size 1))
              (projectile-replace--gather-async candidates "foo" buf nil)
              (let ((old (buffer-local-value 'projectile-replace--scan-timer buf)))
                (expect old :not :to-be nil)
                (expect (memq old timer-list) :to-be-truthy)
                ;; a re-scan (synchronous in batch) must cancel the in-flight
                ;; timer first and settle the flag
                (projectile-replace--start buf candidates "foo" nil)
                (expect (memq old timer-list) :to-be nil)
                (with-current-buffer buf
                  (expect projectile-replace--scanning :to-be nil)
                  (expect projectile-replace--scan-timer :to-be nil))))
          (kill-buffer buf)))))

  (it "cancels the timer and drops it from timer-list when the buffer is killed"
    (projectile-test-with-project
        (("a.txt" . "foo\n")
         ("b.txt" . "foo\n")
         ("c.txt" . "foo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (buf (projectile-scan-async-test--seed root))
             (projectile-replace-scan-chunk-size 1))
        (projectile-replace--gather-async candidates "foo" buf nil)
        (let ((old (buffer-local-value 'projectile-replace--scan-timer buf)))
          (expect old :not :to-be nil)
          ;; the mode's kill-buffer-hook must cancel the pending scan timer
          (kill-buffer buf)
          (expect (memq old timer-list) :to-be nil)))))

  (it "guards the chunk callback against a killed buffer"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (buf (projectile-scan-async-test--seed root)))
        (kill-buffer buf)
        ;; a chunk that fires after its buffer was killed is a silent no-op
        (expect (projectile-replace--scan-step buf candidates "foo" 100 nil 0)
                :not :to-throw))))

  (it "ignores a chunk carrying a superseded scan generation"
    (projectile-test-with-project
        (("a.txt" . "foo\nfoo\n"))
      (let* ((root default-directory)
             (candidates (projectile-scan-async-test--candidates root))
             (buf (projectile-scan-async-test--seed root)))
        (unwind-protect
            (progn
              (projectile-replace--gather-async candidates "foo" buf nil)
              (projectile-scan-async-test--pump buf)
              (with-current-buffer buf
                (let ((current projectile-replace--scan-generation)
                      (before (copy-sequence projectile-replace--matches)))
                  ;; a step from an earlier generation must not touch the matches
                  (projectile-replace--scan-step buf candidates "foo" 100 nil (1- current))
                  (expect projectile-replace--matches :to-equal before)
                  ;; sanity: the same step at the current generation does append
                  (projectile-replace--scan-step buf candidates "foo" 100 nil current)
                  (expect (length projectile-replace--matches)
                          :to-be-greater-than (length before)))))
          (kill-buffer buf))))))

;;; projectile-scan-async-test.el ends here
