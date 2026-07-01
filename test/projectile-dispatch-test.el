;;; projectile-dispatch-test.el --- Tests for the projectile-dispatch modifiers -*- lexical-binding: t -*-

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

;; Tests for the `projectile-dispatch' modifier wrapper commands.  These don't
;; require `transient' to be installed: the wrappers are plain commands and the
;; active switches are stubbed via `projectile-dispatch--args'.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-dispatch modifier wrappers"
  ;; Captures the command `call-interactively' was asked to run together with
  ;; the `current-prefix-arg' in effect at that moment.
  :var (captured)
  (before-each
    (setq captured nil)
    (spy-on 'projectile-dispatch--args :and-return-value nil)
    (spy-on 'call-interactively :and-call-fake
            (lambda (command) (setq captured (cons command current-prefix-arg)))))

  (it "runs the base command with no modifiers"
    (projectile-dispatch-find-file)
    (expect captured :to-equal (cons 'projectile-find-file nil)))

  (it "sets the prefix arg when --invalidate-cache is active"
    (spy-on 'projectile-dispatch--args :and-return-value '("--invalidate-cache"))
    (projectile-dispatch-find-file)
    (expect captured :to-equal (cons 'projectile-find-file '(4))))

  (it "dispatches to the other-window variant for --display=window"
    (spy-on 'projectile-dispatch--args :and-return-value '("--display=window"))
    (projectile-dispatch-find-file)
    (expect (car captured) :to-be 'projectile-find-file-other-window))

  (it "dispatches to the other-frame variant and sets the prefix when both apply"
    (spy-on 'projectile-dispatch--args
            :and-return-value '("--display=frame" "--invalidate-cache"))
    (projectile-dispatch-find-file)
    (expect captured :to-equal (cons 'projectile-find-file-other-frame '(4))))

  (it "maps --regexp to the search commands' prefix arg"
    (spy-on 'projectile-dispatch--args :and-return-value '("--regexp"))
    (projectile-dispatch-ag)
    (expect captured :to-equal (cons 'projectile-ag '(4))))

  (it "maps --new-process to the shell commands' prefix arg"
    (spy-on 'projectile-dispatch--args :and-return-value '("--new-process"))
    (projectile-dispatch-run-eshell)
    (expect captured :to-equal (cons 'projectile-run-eshell '(4))))

  (it "does not set a prefix for a switch that doesn't apply to the command"
    ;; switch-project is display-only; --new-process must not leak into it
    ;; (its prefix argument means \"open the dispatch menu\").
    (spy-on 'projectile-dispatch--args :and-return-value '("--new-process"))
    (projectile-dispatch-switch-project)
    (expect captured :to-equal (cons 'projectile-switch-project nil))))

(describe "projectile-dispatch prefix"
  (it "is a transient prefix exactly when transient is available"
    (if (featurep 'transient)
        (expect (get 'projectile-dispatch 'transient--prefix) :to-be-truthy)
      (expect (fboundp 'projectile-dispatch) :to-be nil))))

(provide 'projectile-dispatch-test)

;;; projectile-dispatch-test.el ends here
