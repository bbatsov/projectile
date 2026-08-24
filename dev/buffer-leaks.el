;;; buffer-leaks.el --- Find specs that leave a foreign buffer current  -*- lexical-binding: t; -*-

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

;; A spec that calls `set-buffer' outside `save-current-buffer' leaves that
;; buffer current for everything that runs after it.  Most specs don't care,
;; which is why such a leak sits unnoticed - until one that reads ambient
;; buffer state runs next and fails, on one Emacs version and not another.
;; That is not hypothetical: it is how `projectile-default-compilation-command'
;; (which asserts on `(point)') started failing on Emacs 30.2 alone.
;;
;; Usage, from the repository root:
;;
;;   eldev -S "(load \"$PWD/dev/buffer-leaks.el\")" -p -dtT -C test
;;
;; Each offending spec is printed as a LEAK line naming the buffer it left
;; behind.  A leak is worth fixing even when nothing currently trips over it,
;; because what trips over it is decided by spec order and by which buffers
;; the cleanup happens to kill.

;;; Code:

;; Buttercup is a test-time dependency, not a build one, so its functions are
;; not known when this file is byte-compiled.
(declare-function buttercup-spec-full-name "buttercup" (spec))

(defun projectile-buffer-leaks--check (orig spec &rest args)
  "Run SPEC via ORIG with ARGS, reporting if it changes the current buffer."
  (let ((before (current-buffer)))
    (prog1 (apply orig spec args)
      (unless (eq before (current-buffer))
        (princ (format "LEAK\t%s\t-> %s\n"
                       (buttercup-spec-full-name spec)
                       (buffer-name (current-buffer))))))))

(defun projectile-buffer-leaks--arm (&rest _)
  "Wrap every spec once the suite is about to run."
  (advice-add 'buttercup--run-spec :around #'projectile-buffer-leaks--check))

(advice-add 'buttercup-run :before #'projectile-buffer-leaks--arm)

(provide 'buffer-leaks)

;;; buffer-leaks.el ends here
