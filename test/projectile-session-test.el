;;; projectile-session-test.el --- Tests for per-project session tabs -*- lexical-binding: t -*-

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

;; Tests for `projectile-session-mode' and its tab-per-project behavior.

;;; Code:

(require 'projectile-test-helpers)
(require 'tab-bar)

(defun projectile-session-test--reset-tabs ()
  "Reset the selected frame to a single, unowned tab."
  (set-frame-parameter nil 'tabs nil)
  ;; Recreate a fresh single current-tab.
  (tab-bar-tabs))

(defun projectile-session-test--populate ()
  "Stand-in populate action; spied on in tests."
  nil)

(defun projectile-session-test--tab-names ()
  "Return the names of all open tabs on the selected frame."
  (mapcar (lambda (tab) (alist-get 'name (cdr tab))) (tab-bar-tabs)))

(describe "projectile-session-mode"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (when projectile-session-mode
      (projectile-session-mode -1))
    (projectile-session-test--reset-tabs))

  (describe "wiring"
    (it "installs and restores the switch-project action"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        (expect projectile-switch-project-action
                :to-equal #'projectile-session-switch-project-action)
        (projectile-session-mode -1)
        (expect projectile-switch-project-action :to-equal 'projectile-find-file)
        (expect projectile-session--saved-switch-action :to-be nil)))

    (it "leaves a user-changed action alone on disable"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        ;; the user rebinds the action while the mode is on
        (setq projectile-switch-project-action 'projectile-dired)
        (projectile-session-mode -1)
        (expect projectile-switch-project-action :to-equal 'projectile-dired)))

    (it "does not lose the saved action when re-enabled while already on"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        ;; a second enable must not capture our own action as the saved value
        (projectile-session-mode 1)
        (projectile-session-mode -1)
        (expect projectile-switch-project-action :to-equal 'projectile-find-file))))

  (describe "adoption on enable"
    (it "stamps the current tab with the current project"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value "/x/proj/")
        (projectile-session-mode 1)
        (let ((tab (projectile-session--current-tab)))
          (expect (projectile-session--tab-root tab) :to-equal "/x/proj/")
          (expect (alist-get 'name (cdr tab)) :to-equal "proj"))))

    (it "does nothing when not inside a project"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        (expect (projectile-session--tab-root (projectile-session--current-tab))
                :to-be nil)))))

(describe "projectile-session-switch-project-action"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs))

  (it "creates and populates a new tab on first switch"
    (let ((projectile-session-default-action 'projectile-session-test--populate))
      (spy-on 'projectile-session-test--populate)
      (spy-on 'projectile-project-root :and-return-value "/one/foo/")
      (projectile-session-switch-project-action)
      (expect (length (projectile-session--project-tabs)) :to-equal 1)
      (expect (projectile-session--tab-root (projectile-session--current-tab))
              :to-equal "/one/foo/")
      (expect 'projectile-session-test--populate :to-have-been-called)))

  (it "selects the existing tab instead of re-populating on a repeat switch"
    (let ((projectile-session-default-action 'projectile-session-test--populate)
          (current-root nil))
      (spy-on 'projectile-session-test--populate)
      (spy-on 'projectile-project-root
              :and-call-fake (lambda (&optional _dir) current-root))
      ;; open foo, then bar; now the current tab is bar's
      (setq current-root "/one/foo/")
      (projectile-session-switch-project-action)
      (setq current-root "/two/bar/")
      (projectile-session-switch-project-action)
      (expect (projectile-session--tab-root (projectile-session--current-tab))
              :to-equal "/two/bar/")
      ;; switching back to foo must select its tab, not create/populate again
      (spy-calls-reset 'projectile-session-test--populate)
      (setq current-root "/one/foo/")
      (projectile-session-switch-project-action)
      (expect (projectile-session--tab-root (projectile-session--current-tab))
              :to-equal "/one/foo/")
      (expect 'projectile-session-test--populate :not :to-have-been-called)
      (expect (length (projectile-session--project-tabs)) :to-equal 2)))

  (it "binds default-directory to the project root while populating"
    (let ((projectile-session-default-action 'projectile-session-test--populate)
          (seen-dir nil))
      (spy-on 'projectile-session-test--populate
              :and-call-fake (lambda () (setq seen-dir default-directory)))
      (spy-on 'projectile-project-root :and-return-value "/one/foo/")
      (projectile-session-switch-project-action)
      (expect seen-dir :to-equal "/one/foo/"))))

(describe "projectile-session tab naming"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs))

  (it "names a lone project tab after the project"
    (projectile-session--make-project-tab "/solo/bar/")
    (expect (projectile-session--tab-root (projectile-session--current-tab))
            :to-equal "/solo/bar/")
    (expect (alist-get 'name (cdr (projectile-session--current-tab)))
            :to-equal "bar"))

  (it "disambiguates same-named projects with a parent component"
    (projectile-session--make-project-tab "/work/foo/")
    (projectile-session--make-project-tab "/home/foo/")
    (let ((names (projectile-session-test--tab-names)))
      (expect names :to-contain "work/foo")
      (expect names :to-contain "home/foo")))

  (it "adds a second parent component when the first also clashes"
    (projectile-session--make-project-tab "/p/shared/foo/")
    (projectile-session--make-project-tab "/q/shared/foo/")
    (let ((names (projectile-session-test--tab-names)))
      (expect names :to-contain "p/shared/foo")
      (expect names :to-contain "q/shared/foo")))

  (it "leaves a hand-renamed tab alone when refreshing names"
    (projectile-session--make-project-tab "/work/foo/")
    ;; the user renames the tab by hand (only the name, not the auto-name param)
    (setf (alist-get 'name (cdr (projectile-session--current-tab))) "MyWork")
    ;; opening another same-named project refreshes names, but must skip the
    ;; hand-renamed tab
    (projectile-session--make-project-tab "/home/foo/")
    (let ((names (projectile-session-test--tab-names)))
      (expect names :to-contain "MyWork")
      (expect names :not :to-contain "work/foo")))

  (it "honors a custom tab-name function"
    (let ((projectile-session-tab-name-function
           (lambda (root) (concat "P:" (directory-file-name root)))))
      (projectile-session--make-project-tab "/some/thing/")
      (expect (alist-get 'name (cdr (projectile-session--current-tab)))
              :to-equal "P:/some/thing"))))

(describe "projectile-session-switch-to-buffer"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs))

  (it "completes over just the current tab's project buffers"
    (let ((buf-a (get-buffer-create "session-a"))
          (buf-b (get-buffer-create "session-b")))
      (unwind-protect
          (progn
            (projectile-session--set-tab-root
             (projectile-session--current-tab) "/proj/")
            (spy-on 'projectile-project-buffers
                    :and-return-value (list buf-a buf-b))
            (spy-on 'projectile-completing-read
                    :and-call-fake (lambda (_prompt choices &rest _) (car choices)))
            (spy-on 'switch-to-buffer)
            (projectile-session-switch-to-buffer)
            (expect 'projectile-project-buffers :to-have-been-called-with "/proj/")
            (expect 'switch-to-buffer :to-have-been-called-with "session-a"))
        (kill-buffer buf-a)
        (kill-buffer buf-b))))

  (it "falls back to plain switch-to-buffer with no project tab"
    (spy-on 'projectile-project-buffers)
    (spy-on 'call-interactively)
    (projectile-session-switch-to-buffer)
    (expect 'projectile-project-buffers :not :to-have-been-called)
    (expect 'call-interactively :to-have-been-called-with #'switch-to-buffer)))

(provide 'projectile-session-test)

;;; projectile-session-test.el ends here
