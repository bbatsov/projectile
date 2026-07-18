;;; projectile-consult-test.el --- Tests for the Consult integration -*- lexical-binding: t -*-

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

;; Tests for `projectile-consult'.  Consult is an optional dependency, so
;; every spec is skipped when it isn't available.

;;; Code:

(require 'projectile-test-helpers)

(defvar projectile-consult-available
  ;; The module loads on any Emacs (it only soft-requires Consult), so its
  ;; presence says nothing about whether the specs can run - what matters is
  ;; Consult itself, which needs Emacs 29.1+ and drives every command under
  ;; test.  Probe for Consult directly and skip when it's absent.  Wrapped in
  ;; `ignore-errors' because the `require' can signal rather than return nil.
  (and (ignore-errors (require 'projectile-consult nil t))
       (ignore-errors (require 'consult nil t))
       t)
  "Non-nil when Consult is available so the integration specs can run.")

(describe "projectile-consult--file-command"
  (it "wraps the indexing command in a shell with a NUL->newline translation"
    (assume projectile-consult-available "consult is not available")
    (let ((cmd (projectile-consult--file-command "git ls-files -z")))
      (expect (car cmd) :to-equal shell-file-name)
      (expect (nth 1 cmd) :to-equal shell-command-switch)
      (expect (nth 2 cmd) :to-equal "git ls-files -z | tr '\\000' '\\n'"))))

(describe "projectile-consult--builder"
  (it "returns the same command regardless of the typed input"
    (assume projectile-consult-available "consult is not available")
    (let ((builder (projectile-consult--builder "git ls-files -z")))
      (expect (funcall builder "foo") :to-equal (funcall builder "bar"))
      (expect (nth 2 (funcall builder "anything"))
              :to-equal "git ls-files -z | tr '\\000' '\\n'"))))

(describe "projectile-consult find-file pipeline"
  (it "produces a Consult async pipeline that Consult accepts"
    (assume projectile-consult-available "consult is not available")
    ;; Exercising the real `consult--process-collection' confirms the keyword
    ;; arguments we pass are accepted by the installed Consult version.
    (expect (functionp
             (consult--process-collection (projectile-consult--builder "echo x")
               :min-input 0
               :transform (consult--async-map (lambda (p) (string-remove-prefix "./" p)))
               :file-handler t))
            :to-be-truthy))

  (it "lists the project's files through the wrapped command (real git repo)"
    (assume projectile-consult-available "consult is not available")
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/" "project/src/" "project/a.el" "project/src/b.el")
      (let ((default-directory (projectile-test-project-root))
            (projectile-git-use-fd nil))
        (call-process "git" nil nil nil "init")
        (call-process "git" nil nil nil "add" "-A")
        (let* ((command (plist-get (projectile-project-files-producer default-directory)
                                   :command))
               (wrapped (projectile-consult--file-command command))
               (output (with-temp-buffer
                         (apply #'process-file (car wrapped) nil t nil (cdr wrapped))
                         (buffer-string)))
               (files (split-string output "\n" t)))
          ;; Same set Projectile itself would index, one per line (no NULs).
          (expect files :to-have-same-items-as '("a.el" "src/b.el"))
          (expect output :not :to-match "\0"))))))

  (it "errors when external-command indexing is disabled"
    (assume projectile-consult-available "consult is not available")
    (spy-on 'projectile-project-files-producer
            :and-return-value '(:directory "/proj/" :vcs none :command nil :separator "\0"))
    (expect (projectile-consult-find-file) :to-throw 'user-error)))

;;; projectile-consult-test.el ends here
