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

  (it "with :no-cache does not cache the command but still records history"
    (let ((command-map (make-hash-table :test 'equal))
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-project-root "/a/random/path"))
      (projectile--run-project-cmd "cmake . --preset debug" command-map
                                   :command-type 'configure :no-cache t)
      ;; nothing frozen in the cache...
      (expect (hash-table-count command-map) :to-equal 0)
      ;; ...but the executed command still feeds both histories, so
      ;; projectile-repeat-last-command keeps working
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root 'configure))
              :to-equal '("cmake . --preset debug"))
      (expect (ring-elements
               (projectile--get-command-history projectile-project-root))
              :to-equal '("cmake . --preset debug"))))

  (it "caches the command by default"
    (let ((command-map (make-hash-table :test 'equal))
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-project-root "/a/random/path"))
      (projectile--run-project-cmd "make" command-map :command-type 'compile)
      (expect (hash-table-count command-map) :to-equal 1)))

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

(describe "function-valued lifecycle commands"
  ;; #1549 / #1676: a project type can register a lifecycle command as a
  ;; function (e.g. CMake's preset pickers, or a user's own :run/:test
  ;; function).  It must be re-invoked - and may prompt - on every run rather
  ;; than being frozen once its first result lands in the command cache.
  (before-each
    (spy-on 'projectile-run-compilation)
    (spy-on 'file-directory-p :and-return-value t)
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-project-root :and-return-value "/proj/")
    (spy-on 'projectile-compilation-dir :and-return-value "/proj/")
    (spy-on 'projectile-project-name :and-return-value "proj"))

  (it "re-invokes a function :run command each run and never caches it"
    (let ((projectile-project-types projectile-project-types)
          (projectile-run-cmd-map (make-hash-table :test 'equal))
          (projectile-project-run-cmd nil)
          (projectile-per-project-compilation-buffer nil)
          (compilation-read-command nil))
      (defun projectile-test--dyn-run () "run-it")
      (projectile-register-project-type 'dyn-run-project '("dyn.marker")
                                        :run 'projectile-test--dyn-run)
      (spy-on 'projectile-project-type :and-return-value 'dyn-run-project)
      (spy-on 'projectile-test--dyn-run :and-call-through)
      (projectile-run-project nil)
      (projectile-run-project nil)
      ;; the function is consulted on BOTH runs...
      (expect 'projectile-test--dyn-run :to-have-been-called-times 2)
      ;; ...and nothing is frozen in the run-command cache
      (expect (hash-table-count projectile-run-cmd-map) :to-equal 0)
      (expect 'projectile-run-compilation :to-have-been-called-with "run-it" nil)))

  (it "re-runs CMake preset selection on every configure"
    (let ((projectile-configure-cmd-map (make-hash-table :test 'equal))
          (projectile-project-configure-cmd nil)
          (projectile-per-project-compilation-buffer nil)
          (compilation-read-command nil))
      (spy-on 'projectile-project-type :and-return-value 'cmake)
      (spy-on 'projectile--cmake-select-command
              :and-return-value projectile--cmake-no-preset)
      (projectile-configure-project nil)
      (projectile-configure-project nil)
      ;; the preset picker runs on each configure, not just the first
      (expect 'projectile--cmake-select-command :to-have-been-called-times 2)
      (expect (hash-table-count projectile-configure-cmd-map) :to-equal 0))))

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
    (expect 'find-file-other-window :to-have-been-called-with "/proj/x.el"))

  ;; The variants are generated by `projectile--define-display-variants':
  ;; the -other-frame body is derived from the -other-window one by symbol
  ;; substitution, so check a generated pair end to end.
  (it "the generated find-file variants hand the right ff-variant to the helper"
    (spy-on 'projectile--find-file)
    (projectile-find-file-other-window)
    (expect 'projectile--find-file :to-have-been-called-with nil #'find-file-other-window)
    (projectile-find-file-other-frame '(4))
    (expect 'projectile--find-file :to-have-been-called-with '(4) #'find-file-other-frame))

  (it "the generated find-dir variants use the dired display functions"
    (spy-on 'projectile--find-dir)
    (projectile-find-dir-other-frame)
    (expect 'projectile--find-dir :to-have-been-called-with nil #'dired-other-frame))

  (it "the terminal commands get only an other-window variant"
    (spy-on 'projectile--run)
    (projectile-run-vterm-other-window)
    (expect 'projectile--run :to-have-been-called-with 'vterm nil t)
    (expect (fboundp 'projectile-run-vterm-other-frame) :to-be nil)
    (expect (fboundp 'projectile-run-eat-other-frame) :to-be nil)
    (expect (fboundp 'projectile-run-ghostel-other-frame) :to-be nil))

  (it "the generated variants are interactive and take the prefix argument"
    (expect (commandp #'projectile-find-file-other-window) :to-be-truthy)
    (expect (interactive-form 'projectile-find-file-other-frame)
            :to-equal '(interactive "P"))
    (expect (cadr (interactive-form 'projectile-switch-to-buffer-other-window))
            :to-be nil)))

(describe "projectile-other-window-command"
  ;; Tear down whatever the prefix command armed (the display override
  ;; and, on Emacs 28, Projectile's switch-to-buffer shim) the way the
  ;; command loop would after the next command.
  (after-each
    (let ((this-command 'ignore))
      (run-hooks 'pre-command-hook)
      (run-hooks 'post-command-hook)))

  (it "arms the display-buffer override and the Projectile transient map"
    (spy-on 'other-window-prefix)
    (spy-on 'set-transient-map)
    (call-interactively #'projectile-other-window-command)
    (expect 'other-window-prefix :to-have-been-called)
    (expect 'set-transient-map :to-have-been-called-with projectile-command-map))

  (it "keeps the Projectile keys active for the next key sequence"
    (spy-on 'other-window-prefix)
    (call-interactively #'projectile-other-window-command)
    (unwind-protect
        (progn
          (expect (key-binding (kbd "f")) :to-be #'projectile-find-file)
          ;; ...including commands that never had -other-window variants.
          (expect (key-binding (kbd "x s")) :to-be #'projectile-run-shell))
      ;; Deactivate the transient map the way the command loop would.
      (let ((this-command 'ignore))
        (run-hooks 'pre-command-hook)))
    (expect (key-binding (kbd "f")) :not :to-be #'projectile-find-file))

  (it "displays the next command's buffer in another window, once"
    ;; Real `other-window-prefix'; only the transient map is stubbed out.
    (spy-on 'set-transient-map)
    (delete-other-windows)
    (let ((start-window (selected-window))
          (obey-display switch-to-buffer-obey-display-actions)
          (buf (get-buffer-create "projectile-other-window-test")))
      (unwind-protect
          (progn
            (call-interactively #'projectile-other-window-command)
            ;; Run the next "command"; even a `switch-to-buffer'-based one
            ;; gets the other-window treatment (on Emacs 28 via
            ;; Projectile's own shim, on 29+ via window.el itself).
            (let ((this-command 'projectile-switch-to-buffer))
              (switch-to-buffer buf)
              ;; Let the "command loop" finish so the exit functions run.
              (run-hooks 'post-command-hook))
            (expect (length (window-list)) :to-equal 2)
            (expect (selected-window) :not :to-be start-window)
            (expect (window-buffer) :to-be buf)
            ;; The override is single-shot and restores the previous state.
            (expect (car display-buffer-overriding-action) :to-equal nil)
            (expect switch-to-buffer-obey-display-actions :to-be obey-display))
        (kill-buffer buf)
        (delete-other-windows)))))

(describe "projectile-other-frame-command"
  (after-each
    (let ((this-command 'ignore))
      (run-hooks 'pre-command-hook)
      (run-hooks 'post-command-hook)))

  (it "arms the display-buffer override and the Projectile transient map"
    (spy-on 'other-frame-prefix)
    (spy-on 'set-transient-map)
    (call-interactively #'projectile-other-frame-command)
    (expect 'other-frame-prefix :to-have-been-called)
    (expect 'set-transient-map :to-have-been-called-with projectile-command-map)))

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

(describe "projectile-subproject-root"
  (it "returns the nearest ancestor directory that holds a project-file marker"
    (let* ((root (file-name-as-directory (make-temp-file "projectile-sub" t)))
           (sub (file-name-as-directory (expand-file-name "mod" root)))
           (deep (file-name-as-directory (expand-file-name "src" sub))))
      (unwind-protect
          (progn
            (make-directory deep t)
            (write-region "" nil (expand-file-name "pom.xml" root))
            (write-region "" nil (expand-file-name "pom.xml" sub))
            (spy-on 'projectile-acquire-root :and-return-value root)
            (spy-on 'projectile-project-type :and-return-value 'maven)
            (spy-on 'projectile-project-type-attribute :and-return-value "pom.xml")
            (let ((default-directory deep))
              (expect (projectile-subproject-root) :to-equal sub)))
        (delete-directory root t))))

  (it "falls back to the project root when only the root holds a marker"
    (let* ((root (file-name-as-directory (make-temp-file "projectile-sub" t)))
           (sub (file-name-as-directory (expand-file-name "mod" root)))
           (deep (file-name-as-directory (expand-file-name "src" sub))))
      (unwind-protect
          (progn
            (make-directory deep t)
            (write-region "" nil (expand-file-name "pom.xml" root))
            (spy-on 'projectile-acquire-root :and-return-value root)
            (spy-on 'projectile-project-type :and-return-value 'maven)
            (spy-on 'projectile-project-type-attribute :and-return-value "pom.xml")
            (let ((default-directory deep))
              (expect (projectile-subproject-root) :to-equal root)))
        (delete-directory root t))))

  (it "errors when there is no marker up to the project root"
    (let* ((root (file-name-as-directory (make-temp-file "projectile-sub" t)))
           (deep (file-name-as-directory (expand-file-name "mod/src" root))))
      (unwind-protect
          (progn
            (make-directory deep t)
            (spy-on 'projectile-acquire-root :and-return-value root)
            (spy-on 'projectile-project-type :and-return-value 'maven)
            (spy-on 'projectile-project-type-attribute :and-return-value "pom.xml")
            (let ((default-directory deep))
              (expect (projectile-subproject-root) :to-throw 'user-error)))
        (delete-directory root t)))))

(describe "projectile-compile-subproject"
  (it "compiles with the subproject as the relative compilation dir"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-subproject-root :and-return-value "/proj/mod/")
    (spy-on 'projectile-compilation-dir :and-return-value "/proj/mod/")
    (spy-on 'projectile-compilation-command :and-return-value "make")
    (spy-on 'projectile--cache-project-commands-p :and-return-value nil)
    (let (seen-dir)
      (spy-on 'projectile--run-project-cmd :and-call-fake
              (lambda (&rest _) (setq seen-dir projectile-project-compilation-dir)))
      (projectile-compile-subproject nil)
      (expect 'projectile--run-project-cmd :to-have-been-called)
      ;; the subproject dir is passed relative to the project root
      (expect seen-dir :to-equal "mod/"))))

;;; projectile-commands-test.el ends here
