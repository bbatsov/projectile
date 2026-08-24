;;; function-coverage.el --- Which Projectile functions the suite never calls  -*- lexical-binding: t; -*-

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

;; Function-level coverage for the test suite, with no external dependency.
;; Every `defun' in projectile.el is advised to record that it ran, the suite
;; is run, and whatever was never called is reported.
;;
;; Usage, from the repository root:
;;
;;   eldev -S "(load \"$PWD/dev/function-coverage.el\")" -p -dtT -C test
;;
;; The path has to be absolute: `-S' forms are evaluated before Eldev settles
;; on the project directory, so a relative one is not found.
;;
;; It writes `dev/function-coverage.txt' - one `HIT'/`MISS' line per function -
;; and prints a summary.  Point `projectile-coverage-output' elsewhere to keep
;; the file out of the working tree.
;;
;; What it is and is not: this says whether a function was ever entered, not
;; which of its branches were.  A `MISS' is therefore a hard fact - nothing
;; exercises that code at all - while a `HIT' only means something reached it.
;; Read the MISS list, not the percentage; the useful property is that the list
;; shrinks.
;;
;; One caveat worth knowing: a `defsubst' inlined into its callers at
;; compile time can be reported as a MISS even though its body runs.
;; projectile.el has one, so this is noise rather than a problem.

;;; Code:

(defvar projectile-coverage-source
  (expand-file-name "projectile.el"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name buffer-file-name)))))
  "The file whose `defun's are watched.")

(defvar projectile-coverage-output
  (expand-file-name "function-coverage.txt"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Where the HIT/MISS report is written.")

(defvar projectile-coverage--called (make-hash-table :test 'eq)
  "Functions that ran at least once during the suite.")

(defvar projectile-coverage--watched nil
  "Every function being watched, in definition order.")

(defun projectile-coverage--instrument (&rest _)
  "Advise every `defun' of `projectile-coverage-source' to record that it ran.
Idempotent, so it can be attached to something that runs more than once."
  (unless projectile-coverage--watched
    (with-temp-buffer
      (insert-file-contents projectile-coverage-source)
      (goto-char (point-min))
      (while (re-search-forward "^(defun \\([^ ()\n]+\\)" nil t)
        (push (intern (match-string 1)) projectile-coverage--watched)))
    (setq projectile-coverage--watched (nreverse projectile-coverage--watched))
    (dolist (fn projectile-coverage--watched)
      ;; A macro or special form cannot be advised, and neither can a name
      ;; that never got defined (a `defun' inside a `when' that was false).
      (when (and (fboundp fn) (not (macrop fn)) (not (special-form-p fn)))
        (let ((watched fn))
          (ignore-errors
            (advice-add watched :before
                        (lambda (&rest _)
                          (puthash watched t projectile-coverage--called)))))))
    (message "function-coverage: watching %d functions"
             (length projectile-coverage--watched))))

(defun projectile-coverage--report (&rest _)
  "Write the HIT/MISS report and print a one-line summary."
  (when projectile-coverage--watched
    (let ((hit 0) (missed 0))
      (with-temp-file projectile-coverage-output
        (dolist (fn (sort (copy-sequence projectile-coverage--watched)
                          (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
          (if (gethash fn projectile-coverage--called)
              (progn (setq hit (1+ hit)) (insert (format "HIT\t%s\n" fn)))
            (setq missed (1+ missed))
            (insert (format "MISS\t%s\n" fn)))))
      (message "function-coverage: %d/%d called (%.1f%%), %d never called - see %s"
               hit (+ hit missed)
               (if (> (+ hit missed) 0) (* 100.0 (/ (float hit) (+ hit missed))) 0.0)
               missed projectile-coverage-output))))

;; `buttercup-run' is the last thing that happens before the specs, by which
;; point projectile and every test file have been loaded.
(advice-add 'buttercup-run :before #'projectile-coverage--instrument)
(add-hook 'kill-emacs-hook #'projectile-coverage--report)

(provide 'function-coverage)

;;; function-coverage.el ends here
