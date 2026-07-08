;;; projectile-dir-locals-test.el --- Tests for the .dir-locals editing helpers -*- lexical-binding: t -*-

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

;; Tests for `projectile-skel-dir-locals' and `projectile-read-variable'.
;; Minibuffer input is stubbed: variable names come through
;; `completing-read', values through `read-string' (via `skeleton-read').

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-test--skel-dir-locals (variables values)
  "Run `projectile-skel-dir-locals' feeding it VARIABLES and VALUES.
Return the resulting buffer contents."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) (pop variables)))
            ((symbol-function 'read-string)
             (lambda (&rest _) (pop values))))
    (with-temp-buffer
      (projectile-skel-dir-locals)
      (buffer-string))))

(describe "projectile-read-variable"
  (it "returns the selected variable name"
    (spy-on 'completing-read :and-return-value "compile-command")
    (expect (projectile-read-variable) :to-equal "compile-command"))

  (it "returns nil on empty input"
    (spy-on 'completing-read :and-return-value "")
    (expect (projectile-read-variable) :to-be nil)))

(describe "projectile-skel-dir-locals"
  (it "keeps entered variables when the loop ends with an empty variable name"
    (let ((result (projectile-test--skel-dir-locals
                   '("compile-command" "")
                   '("\"make -k\""))))
      (expect (read result) :to-equal
              '((nil . ((compile-command . "make -k")))))))

  (it "collects several variable entries"
    (let ((result (projectile-test--skel-dir-locals
                   '("compile-command" "indent-tabs-mode" "")
                   '("\"make -k\"" "nil"))))
      (expect (read result) :to-equal
              '((nil . ((compile-command . "make -k")
                        (indent-tabs-mode . nil)))))))

  (it "produces a valid empty template when no variable is entered"
    (let ((result (projectile-test--skel-dir-locals '("") '())))
      (expect (read result) :to-equal '((nil))))))

(provide 'projectile-dir-locals-test)
;;; projectile-dir-locals-test.el ends here
