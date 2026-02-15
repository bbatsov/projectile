;;; projectile-commands-test.el --- Tests for lifecycle commands (compile/test/run, shell) -*- lexical-binding: t -*-

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

;; Tests for lifecycle commands (compile/test/run, shell).

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-compilation-dir"
  (it "returns the compilation directory for a project"
    (cl-flet ((compilation-dir-for (project-root rel-dir)
                (spy-on 'projectile-project-root :and-return-value project-root)
                (spy-on 'projectile-project-type :and-return-value 'generic)
                (let ((projectile-project-compilation-dir rel-dir))
                  (projectile-compilation-dir))))
      (expect (compilation-dir-for "/root/" "build") :to-equal "/root/build/")
      (expect (compilation-dir-for "/root/" "build/") :to-equal "/root/build/")
      (expect (compilation-dir-for "/root/" "./build") :to-equal "/root/build/")
      (expect (compilation-dir-for "/root/" "local/build") :to-equal "/root/local/build/")))
  (it "returns the default compilation dir based on project-type"
    ;; Bind the alist so the registration doesn't leak into other suites.
    (let ((projectile-project-types projectile-project-types))
      (projectile-register-project-type 'default-dir-project '("file.txt")
                                        :compilation-dir "build")
      (cl-flet ((compilation-dir-for (project-root &optional rel-dir)
                  (spy-on 'projectile-project-root :and-return-value project-root)
                  (spy-on 'projectile-project-type :and-return-value 'default-dir-project)
                  (if (null rel-dir)
                      (projectile-compilation-dir)
                    (let ((projectile-project-compilation-dir rel-dir))
                      (projectile-compilation-dir)))))
        (expect (compilation-dir-for "/root/") :to-equal "/root/build/")
        (expect (compilation-dir-for "/root/" "buildings") :to-equal "/root/buildings/"))))
  (it "should not fail on bad compilation dir config"
    (defun -compilation-test-function ()
      1)
    (let ((projectile-project-types projectile-project-types)
          (projectile-project-type 'has-command-at-point)
          (projectile-project-compilation-dir nil))
      (projectile-register-project-type 'has-command-at-point '("file.txt")
                                        :compile (-compilation-test-function))
      (expect (projectile-compilation-dir) :to-equal (concat (expand-file-name ".") "/")))))

(describe "projectile-default-compilation-command"
  (it "returns the default compilation command for project-type"
    (defun -compilation-test-function ()
      (if (= (point) 1)
          "my-make"
        "./run-extra"))
    (let ((projectile-project-types projectile-project-types))
      (projectile-register-project-type 'has-command-at-point '("file.txt")
                                        :compile '-compilation-test-function)
      (expect (projectile-default-compilation-command 'has-command-at-point) :to-equal "my-make")
      (with-temp-buffer
        (insert "ABCDE")
        (goto-char 2)
        (expect (projectile-default-compilation-command 'has-command-at-point) :to-equal "./run-extra"))))
  (it "fails on bad project-type config"
    (defun -compilation-test-function ()
      1)
    (let ((projectile-project-types projectile-project-types))
      (projectile-register-project-type 'has-command-at-point '("file.txt")
                                        :compile (-compilation-test-function))
      (expect (projectile-default-compilation-command 'has-command-at-point) :to-throw))))

(describe "projectile-configure-command"
  (it "configure command for generic project type"
    (spy-on 'projectile-default-configure-command :and-return-value nil)
    (spy-on 'projectile-project-type :and-return-value 'generic)
    (expect (projectile-configure-command "fsdf") :to-equal nil)))

;;; known projects tests

(describe "projectile-run-shell-command-in-root"
  (describe "when called directly in elisp"
    (before-each (spy-on 'shell-command))
    (describe "when called with all three parameters"
      (it "expects to call shell-command with the same parameters"
        (projectile-run-shell-command-in-root "cmd" "output-buffer" "error-buffer")
        (expect 'shell-command :to-have-been-called-with "cmd" "output-buffer" "error-buffer")))
    (describe "when called with only one optional parameter"
      (it "expects to call shell-command with the same parameters"
        (projectile-run-shell-command-in-root "cmd" "output-buffer")
        (expect 'shell-command :to-have-been-called-with "cmd" "output-buffer" nil)))
    (describe "when called with no optional parameters"
      (it "expects to call shell-command with the same parameters"
        (projectile-run-shell-command-in-root "cmd")
        (expect 'shell-command :to-have-been-called-with "cmd" nil nil))))
  (describe "when called interactively"
    (before-each (spy-on 'shell-command))
    (it "expects to be interactive"
      (expect (interactive-form 'projectile-run-shell-command-in-root) :not :to-be nil))
    (it "expects to call shell-command with the given command"
      (funcall-interactively 'projectile-run-shell-command-in-root "cmd")
      (expect 'shell-command :to-have-been-called-with "cmd" nil nil))))

(describe "projectile-run-async-shell-command-in-root"
  (describe "when called directly in elisp"
    (before-each (spy-on 'async-shell-command))
    (describe "when called with all three parameters"
      (it "expects to call async-shell-command with the same parameters"
        (projectile-run-async-shell-command-in-root "cmd" "output-buffer" "error-buffer")
        (expect 'async-shell-command :to-have-been-called-with "cmd" "output-buffer" "error-buffer")))
    (describe "when called with only one optional parameter"
      (it "expects to call async-shell-command with the same parameters"
        (projectile-run-async-shell-command-in-root "cmd" "output-buffer")
        (expect 'async-shell-command :to-have-been-called-with "cmd" "output-buffer" nil)))
    (describe "when called with no optional parameters"
      (it "expects to call async-shell-command with the same parameters"
        (projectile-run-async-shell-command-in-root "cmd")
        (expect 'async-shell-command :to-have-been-called-with "cmd" nil nil))))
  (describe "when called interactively"
    (before-each (spy-on 'async-shell-command))
    (it "expects to be interactive"
      (expect (interactive-form 'projectile-run-async-shell-command-in-root) :not :to-be nil))
    (it "expects to call async-shell-command with the given command"
      (funcall-interactively 'projectile-run-async-shell-command-in-root "cmd")
      (expect 'async-shell-command :to-have-been-called-with "cmd" nil nil))))

(describe "projectile-discard-command-cache"
  (it "removes cached commands for the current project across all maps"
    (let ((projectile-configure-cmd-map (make-hash-table :test 'equal))
          (projectile-compilation-cmd-map (make-hash-table :test 'equal))
          (projectile-install-cmd-map (make-hash-table :test 'equal))
          (projectile-package-cmd-map (make-hash-table :test 'equal))
          (projectile-test-cmd-map (make-hash-table :test 'equal))
          (projectile-run-cmd-map (make-hash-table :test 'equal)))
      (spy-on 'projectile-acquire-root :and-return-value "/proj/a/")
      ;; one entry per map, plus a subdir compile dir
      (puthash "/proj/a/" "configure" projectile-configure-cmd-map)
      (puthash "/proj/a/build/" "make" projectile-compilation-cmd-map)
      (puthash "/proj/a/" "make install" projectile-install-cmd-map)
      (puthash "/proj/a/" "make package" projectile-package-cmd-map)
      (puthash "/proj/a/" "make test" projectile-test-cmd-map)
      (puthash "/proj/a/" "./run" projectile-run-cmd-map)
      ;; a different project and a prefix-sharing sibling must survive
      (puthash "/proj/b/" "other" projectile-compilation-cmd-map)
      (puthash "/proj/a-extra/" "sibling" projectile-compilation-cmd-map)
      (projectile-discard-command-cache)
      (expect (gethash "/proj/a/" projectile-configure-cmd-map) :to-be nil)
      (expect (gethash "/proj/a/build/" projectile-compilation-cmd-map) :to-be nil)
      (expect (gethash "/proj/a/" projectile-install-cmd-map) :to-be nil)
      (expect (gethash "/proj/a/" projectile-package-cmd-map) :to-be nil)
      (expect (gethash "/proj/a/" projectile-test-cmd-map) :to-be nil)
      (expect (gethash "/proj/a/" projectile-run-cmd-map) :to-be nil)
      ;; unrelated project and prefix-sharing sibling are left untouched
      (expect (gethash "/proj/b/" projectile-compilation-cmd-map) :to-equal "other")
      (expect (gethash "/proj/a-extra/" projectile-compilation-cmd-map) :to-equal "sibling"))))

(describe "projectile--run-project-cmd"

  (before-each
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-maybe-read-command :and-call-fake
            (lambda (arg default-cmd prompt &optional command-type) default-cmd))
    ;; Stops projectile--run-project-cmd from creating a new directory for
    ;; the compilation dir
    (spy-on 'file-directory-p :and-return-value t))

  (it "projectile-cmd-hist-ignoredups set to t"

    (let ((command-map (make-hash-table :test 'equal))
          (projectile-cmd-hist-ignoredups t)
          ;; history is based on the project root, so we set it to a random
          ;; path to ensure there are no existing commands in history
          (projectile-project-root "/a/random/path"))
      (projectile--run-project-cmd "foo" command-map)
      (projectile--run-project-cmd "foo" command-map)
      (projectile--run-project-cmd "bar" command-map)
      (projectile--run-project-cmd "foo" command-map)
      (expect 'projectile-run-compilation :to-have-been-called-times 4)
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root))
              :to-equal '("foo" "bar" "foo"))))

  (it "projectile-cmd-hist-ignoredups set to erase"
    (let ((command-map (make-hash-table :test 'equal))
          (projectile-cmd-hist-ignoredups 'erase)
          (projectile-project-root "/a/random/path"))
      (projectile--run-project-cmd "foo" command-map)
      (projectile--run-project-cmd "bar" command-map)
      (projectile--run-project-cmd "foo" command-map)
      (projectile--run-project-cmd "foo" command-map)
      (expect 'projectile-run-compilation :to-have-been-called-times 4)
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root))
              :to-equal '("foo" "bar"))))

  (it "keeps a separate history per command type without bleeding"
    (let ((command-map (make-hash-table :test 'equal))
          (projectile-cmd-hist-ignoredups t)
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-project-root "/a/random/path"))
      (projectile--run-project-cmd "make" command-map :command-type 'compile)
      (projectile--run-project-cmd "make test" command-map :command-type 'test)
      (projectile--run-project-cmd "make check" command-map :command-type 'test)
      ;; per-type histories stay distinct
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root 'compile))
              :to-equal '("make"))
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root 'test))
              :to-equal '("make check" "make test"))
      ;; the combined history still records every command, so
      ;; projectile-repeat-last-command keeps working across types
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root))
              :to-equal '("make check" "make test" "make"))))

  (it "does not record a per-type history when command-type is nil"
    (let ((command-map (make-hash-table :test 'equal))
          (projectile-cmd-hist-ignoredups t)
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-project-root "/a/random/path"))
      (projectile--run-project-cmd "foo" command-map)
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root))
              :to-equal '("foo"))
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root 'compile))
              :to-equal '()))))

(describe "projectile-default-generic-command"
  (it "returns a string command as-is"
    (let ((projectile-project-types '((test-type compile-command "make"))))
      (expect (projectile-default-generic-command 'test-type 'compile-command) :to-equal "make")))
  (it "calls a function symbol and returns its result"
    (let ((projectile-project-types '((test-type compile-command my-compile-fn))))
      (spy-on 'my-compile-fn :and-return-value "custom-build")
      (expect (projectile-default-generic-command 'test-type 'compile-command) :to-equal "custom-build")))
  (it "calls a lambda command and returns its result"
    (let ((projectile-project-types
           `((test-type compile-command ,(lambda () "lambda-build")))))
      (expect (projectile-default-generic-command 'test-type 'compile-command) :to-equal "lambda-build")))
  (it "returns nil for missing command"
    (let ((projectile-project-types '((test-type))))
      (expect (projectile-default-generic-command 'test-type 'compile-command) :to-equal nil))))

(describe "display-variant commands"
  ;; The other-window/-frame variants share a helper and differ only in the
  ;; display function they hand off to.
  (it "projectile-dired opens the project root with dired"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'dired)
    (projectile-dired)
    (expect 'dired :to-have-been-called-with "/proj/"))

  (it "projectile-dired-other-window uses dired-other-window"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'dired-other-window)
    (projectile-dired-other-window)
    (expect 'dired-other-window :to-have-been-called-with "/proj/"))

  (it "projectile-switch-to-buffer-other-frame uses switch-to-buffer-other-frame"
    (spy-on 'projectile-read-buffer-to-switch :and-return-value "buf")
    (spy-on 'switch-to-buffer-other-frame)
    (projectile-switch-to-buffer-other-frame)
    (expect 'switch-to-buffer-other-frame :to-have-been-called-with "buf"))

  (it "projectile-find-implementation-or-test-other-window uses find-file-other-window"
    (spy-on 'buffer-file-name :and-return-value "/proj/x_test.el")
    (spy-on 'projectile-find-implementation-or-test :and-return-value "/proj/x.el")
    (spy-on 'find-file-other-window)
    (projectile-find-implementation-or-test-other-window)
    (expect 'find-file-other-window :to-have-been-called-with "/proj/x.el")))

(describe "projectile--run-project-cmd %p substitution"
  (it "replaces %p with the project name before running the command"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-compilation-dir :and-return-value temporary-file-directory)
    (spy-on 'projectile-maybe-read-command :and-call-fake (lambda (_show cmd &rest _) cmd))
    (spy-on 'projectile-project-name :and-return-value "myproj")
    (spy-on 'projectile-run-compilation)
    (projectile--run-project-cmd "make -C %p build" nil)
    (expect 'projectile-run-compilation
            :to-have-been-called-with "make -C myproj build" nil))

  (it "leaves a command without %p untouched"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-compilation-dir :and-return-value temporary-file-directory)
    (spy-on 'projectile-maybe-read-command :and-call-fake (lambda (_show cmd &rest _) cmd))
    (spy-on 'projectile-project-name :and-return-value "myproj")
    (spy-on 'projectile-run-compilation)
    (projectile--run-project-cmd "make build" nil)
    (expect 'projectile-run-compilation
            :to-have-been-called-with "make build" nil)))

;;; projectile-commands-test.el ends here
