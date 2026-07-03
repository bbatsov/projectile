;;; projectile-frecency-test.el --- Tests for the file frecency ranking -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov

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

;; Tests for the per-project file visit tracking and the frecency-based
;; completion sorting.

;;; Code:

(require 'projectile-test-helpers)

(defmacro projectile-test-with-frecency (&rest body)
  "Run BODY with isolated, empty frecency state."
  `(let ((projectile-enable-frecency t)
         (projectile-frecency-max-files 200)
         (projectile--frecency-table (make-hash-table :test 'equal))
         (projectile--frecency-dirty nil)
         (projectile-frecency-file
          (expand-file-name (format "frecency-%d.eld" (random 100000))
                            temporary-file-directory)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p projectile-frecency-file)
         (delete-file projectile-frecency-file)))))

(defun projectile-test--record-visit (root file &optional time)
  "Record a visit to FILE under ROOT at TIME (defaults to now)."
  (when time
    (spy-on 'projectile-time-seconds :and-return-value time))
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name file root))
    (projectile--frecency-record root)))

(describe "projectile--frecency-record"
  (it "records visits with counts and timestamps"
    (projectile-test-with-frecency
     (projectile-test--record-visit "/proj/" "src/a.el" 1000)
     (projectile-test--record-visit "/proj/" "src/a.el" 2000)
     (let ((entry (gethash "src/a.el"
                           (gethash "/proj/" projectile--frecency-table))))
       (expect (car entry) :to-equal 2)
       (expect (cdr entry) :to-equal 2000)
       (expect projectile--frecency-dirty :to-be-truthy))))

  (it "does nothing when frecency is disabled"
    (projectile-test-with-frecency
     (let ((projectile-enable-frecency nil))
       (projectile-test--record-visit "/proj/" "a.el")
       (expect (hash-table-count projectile--frecency-table) :to-equal 0))))

  (it "does not track remote projects"
    (projectile-test-with-frecency
     (projectile-test--record-visit "/ssh:host:/proj/" "a.el")
     (expect (hash-table-count projectile--frecency-table) :to-equal 0)))

  (it "does not track files resolving outside the project"
    (projectile-test-with-frecency
     (with-temp-buffer
       (setq buffer-file-name "/elsewhere/a.el")
       (projectile--frecency-record "/proj/"))
     (expect (gethash "/proj/" projectile--frecency-table) :to-be nil))))

(describe "projectile--frecency-score"
  (it "ranks a recent visit above an old one with the same count"
    (let ((now 1000000))
      (expect (projectile--frecency-score (cons 1 now) now)
              :to-be-greater-than
              (projectile--frecency-score (cons 1 (- now (* 14 86400))) now))))

  (it "lets recency overtake a higher count eventually"
    (let ((now 1000000))
      ;; 10 visits four weeks ago decay to 2.5; 3 visits now stay 3.
      (expect (projectile--frecency-score (cons 3 now) now)
              :to-be-greater-than
              (projectile--frecency-score (cons 10 (- now (* 28 86400))) now)))))

(describe "projectile--frecency-sort-function"
  (it "returns nil when nothing is tracked"
    (projectile-test-with-frecency
     (expect (projectile--frecency-sort-function "/proj/") :to-be nil)))

  (it "returns nil when frecency is disabled"
    (projectile-test-with-frecency
     (projectile-test--record-visit "/proj/" "a.el")
     (let ((projectile-enable-frecency nil))
       (expect (projectile--frecency-sort-function "/proj/") :to-be nil))))

  (it "puts tracked files first by score and preserves the rest"
    (projectile-test-with-frecency
     (projectile-test--record-visit "/proj/" "often.el" 1000)
     (projectile-test--record-visit "/proj/" "often.el" 1000)
     (projectile-test--record-visit "/proj/" "once.el" 1000)
     (spy-on 'projectile-time-seconds :and-return-value 1000)
     (let ((sorter (projectile--frecency-sort-function "/proj/")))
       (expect (funcall sorter '("z.el" "once.el" "a.el" "often.el"))
               :to-equal '("often.el" "once.el" "z.el" "a.el"))))))

(describe "projectile--frecency-prune"
  (it "drops the lowest scoring entries beyond the limit"
    (projectile-test-with-frecency
     (let ((projectile-frecency-max-files 2))
       (projectile-test--record-visit "/proj/" "a.el" 1000)
       (projectile-test--record-visit "/proj/" "a.el" 1000)
       (projectile-test--record-visit "/proj/" "b.el" 1000)
       (projectile-test--record-visit "/proj/" "b.el" 1000)
       (projectile-test--record-visit "/proj/" "b.el" 1000)
       (projectile-test--record-visit "/proj/" "old.el" 10)
       (let ((files (gethash "/proj/" projectile--frecency-table)))
         (projectile--frecency-prune files)
         (expect (hash-table-count files) :to-equal 2)
         (expect (gethash "old.el" files) :to-be nil))))))

(describe "frecency persistence"
  (it "round-trips the data through the frecency file"
    (projectile-test-with-frecency
     (projectile-test--record-visit "/proj/" "src/a.el" 1234)
     (projectile--frecency-save)
     (expect projectile--frecency-dirty :to-be nil)
     ;; Force a reload from disk.
     (setq projectile--frecency-table nil)
     (let ((entry (gethash "src/a.el"
                           (gethash "/proj/" (projectile--frecency-data)))))
       (expect entry :to-equal '(1 . 1234)))))

  (it "does not write when nothing changed"
    (projectile-test-with-frecency
     (projectile--frecency-save)
     (expect (file-exists-p projectile-frecency-file) :to-be nil)))

  (it "ignores a malformed frecency file instead of breaking recording"
    ;; A corrupted cache file must never make `find-file' error - the
    ;; load used to re-signal on every visit.
    (projectile-test-with-frecency
     (with-temp-file projectile-frecency-file (insert "hello world"))
     (setq projectile--frecency-table nil)
     (spy-on 'display-warning)
     (expect (hash-table-count (projectile--frecency-data)) :to-equal 0)
     (expect 'display-warning :to-have-been-called)
     ;; Recording still works afterwards.
     (projectile-test--record-visit "/proj/" "a.el" 1000)
     (expect (gethash "a.el" (gethash "/proj/" projectile--frecency-table))
             :to-equal '(1 . 1000))))

  (it "caps the saved data at projectile-frecency-max-files per project"
    (projectile-test-with-frecency
     (let ((projectile-frecency-max-files 2))
       (projectile-test--record-visit "/proj/" "a.el" 1000)
       (projectile-test--record-visit "/proj/" "a.el" 1000)
       (projectile-test--record-visit "/proj/" "b.el" 1000)
       (projectile-test--record-visit "/proj/" "b.el" 1000)
       (projectile-test--record-visit "/proj/" "c.el" 900)
       (projectile--frecency-save)
       (setq projectile--frecency-table nil)
       (expect (hash-table-count
                (gethash "/proj/" (projectile--frecency-data)))
               :to-equal 2))))

  (it "merges another session's newer data instead of overwriting it"
    (projectile-test-with-frecency
     ;; Another session saved while we were running: its file has an
     ;; entry we don't have, plus different numbers for one we do.
     (projectile-test--record-visit "/proj/" "mine.el" 1000)
     (projectile-test--record-visit "/proj/" "shared.el" 1000)
     (with-temp-file projectile-frecency-file
       (insert (prin1-to-string
                '(("/proj/" ("theirs.el" 3 2000) ("shared.el" 7 500))))))
     (projectile--frecency-save)
     (setq projectile--frecency-table nil)
     (let ((files (gethash "/proj/" (projectile--frecency-data))))
       (expect (gethash "mine.el" files) :to-equal '(1 . 1000))
       (expect (gethash "theirs.el" files) :to-equal '(3 . 2000))
       ;; max count, latest timestamp
       (expect (gethash "shared.el" files) :to-equal '(7 . 1000))))))

(describe "projectile-completing-read sort metadata"
  (it "exposes the sort function via completion metadata"
    (let (captured)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (setq captured (funcall collection "" nil 'metadata))
                "x"))
      (spy-on 'projectile-prepend-project-name :and-call-fake #'identity)
      (let ((sorter #'identity))
        (projectile-completing-read "Prompt: " '("x") :sort-function sorter)
        (expect (alist-get 'display-sort-function (cdr captured)) :to-be sorter)
        (expect (alist-get 'cycle-sort-function (cdr captured)) :to-be sorter)
        (expect (alist-get 'category (cdr captured)) :to-equal 'project-file))))

  (it "omits the sort metadata when no sort function is given"
    (let (captured)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _)
                (setq captured (funcall collection "" nil 'metadata))
                "x"))
      (spy-on 'projectile-prepend-project-name :and-call-fake #'identity)
      (projectile-completing-read "Prompt: " '("x"))
      (expect (alist-get 'display-sort-function (cdr captured)) :to-be nil))))

(provide 'projectile-frecency-test)
;;; projectile-frecency-test.el ends here
