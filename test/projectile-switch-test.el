;;; projectile-switch-test.el --- Tests for project switching and dir-locals -*- lexical-binding: t -*-

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

;; Tests for project switching and dir-locals.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-switch-project"
  (it "fails if there are no projects"
    (spy-on 'projectile-relevant-known-projects :and-return-value nil)
    (expect (projectile-switch-project) :to-throw)))

(describe "projectile-add-dir-local-variable"
  (it "adds new dir-local variables"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.dir-locals.el"
       "project/.projectile")
      (append-to-file "()" nil "project/.dir-locals.el")
      (with-current-buffer (find-file-noselect "project/.projectile" t)
        (let ((enable-local-variables :all))
          (expect (boundp 'fooo) :to-be nil)
          (projectile-add-dir-local-variable nil 'fooo 1)
          (hack-dir-local-variables-non-file-buffer)
          (expect (boundp 'fooo) :to-be 't)
          (expect fooo :to-be 1))))))

  (it "fails when there is no projectile project"
    (projectile-test-with-sandbox
     (let ((default-directory "/"))
       (expect (projectile-add-dir-local-variable nil 'fooo 1) :to-throw 'error)))))

(describe "projectile-delete-dir-local-variable"
  (it "deletes existing dir-local variables"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.dir-locals.el"
       "project/.projectile")
      (append-to-file
       "((nil . ((foo . bar))))" nil "project/.dir-locals.el")
      (let ((enable-local-variables :all))
        (with-current-buffer (find-file-noselect "project/.projectile" t)
          ;; Reload the file to ensure that this file wasn't already
          ;; opened from a previous test
          (revert-buffer :ignore-auto :noconfirm)
          ;; Check that the variable is bound
          (expect (boundp 'foo) :to-be 't)
          ;; Remove the variable
          (projectile-delete-dir-local-variable nil 'foo)
          ;; Reload the file
          (revert-buffer :ignore-auto :noconfirm)
          ;; Check that the variable is unbound
          (expect (boundp 'foo) :to-be nil))))))

  (it "fails when there is no projectile project"
    (projectile-test-with-sandbox
     (let ((default-directory "/"))
       (expect (projectile-delete-dir-local-variable nil 'fooo 1) :to-throw 'error)))))

(describe "projectile-most-recent-project"
  (it "records the project switched away from"
    (let ((projectile-most-recent-project nil)
          (projectile-switch-project-action #'ignore))
      (projectile-test-with-sandbox
        (projectile-test-with-files
            ("project-a/" "project-a/.projectile"
             "project-b/" "project-b/.projectile")
          (let ((a (file-name-as-directory (expand-file-name "project-a")))
                (b (file-name-as-directory (expand-file-name "project-b"))))
            (projectile-add-known-project a)
            (projectile-add-known-project b)
            (let ((default-directory a))
              (projectile-switch-project-by-name b))
            (expect (file-equal-p projectile-most-recent-project a) :to-be-truthy))))))

  (it "does not clobber the recent project when re-switching to the same one"
    (let ((projectile-most-recent-project "/sentinel/")
          (projectile-switch-project-action #'ignore))
      (projectile-test-with-sandbox
        (projectile-test-with-files
            ("project-a/" "project-a/.projectile")
          (let ((a (file-name-as-directory (expand-file-name "project-a"))))
            (projectile-add-known-project a)
            (let ((default-directory a))
              (projectile-switch-project-by-name a))
            (expect projectile-most-recent-project :to-equal "/sentinel/")))))))

(describe "projectile-switch-project-other-window/-frame"
  (it "runs switch-project with the other-window action bound (#1956)"
    (defvar projectile-test--captured-action)
    (let ((projectile-test--captured-action nil)
          (projectile-switch-project-other-window-action 'my-ow-action))
      (spy-on 'projectile-switch-project :and-call-fake
              (lambda (&optional _arg)
                (setq projectile-test--captured-action projectile-switch-project-action)))
      (projectile-switch-project-other-window)
      (expect projectile-test--captured-action :to-equal 'my-ow-action)))

  (it "runs switch-project with the other-frame action bound (#1956)"
    (defvar projectile-test--captured-action)
    (let ((projectile-test--captured-action nil)
          (projectile-switch-project-other-frame-action 'my-of-action))
      (spy-on 'projectile-switch-project :and-call-fake
              (lambda (&optional _arg)
                (setq projectile-test--captured-action projectile-switch-project-action)))
      (projectile-switch-project-other-frame)
      (expect projectile-test--captured-action :to-equal 'my-of-action)))

  (it "is bound to 4 p / 5 p in projectile-command-map (#1956)"
    (expect (lookup-key projectile-command-map (kbd "4 p"))
            :to-equal 'projectile-switch-project-other-window)
    (expect (lookup-key projectile-command-map (kbd "5 p"))
            :to-equal 'projectile-switch-project-other-frame)))

(describe "projectile-switch-to-most-recent-project"
  (it "errors when no recent project is recorded"
    (let ((projectile-most-recent-project nil))
      (expect (projectile-switch-to-most-recent-project) :to-throw 'user-error)))

  (it "switches to the recorded project"
    (let ((projectile-most-recent-project "/proj/a/"))
      (spy-on 'projectile-switch-project-by-name)
      (projectile-switch-to-most-recent-project)
      (expect 'projectile-switch-project-by-name
              :to-have-been-called-with "/proj/a/" nil))))

(describe "projectile--dispatch-in-directory"
  (it "runs the transient action with the project current and restores it on exit"
    ;; Regression guard for #2046: a transient action's suffix commands run
    ;; after the switch unwinds, so the project's directory must persist for
    ;; the lifetime of the menu (and be restored afterwards).
    (with-temp-buffer
      (setq default-directory "/old/")
      (let ((transient-exit-hook nil)
            seen-dir)
        (cl-letf (((symbol-function 'projectile-test-fake-prefix)
                   (lambda () (interactive) (setq seen-dir default-directory))))
          (projectile--dispatch-in-directory "/proj/" 'projectile-test-fake-prefix))
        ;; the menu (hence its suffixes) sees the switched-to project ...
        (expect seen-dir :to-equal "/proj/")
        ;; ... and it stays current while the transient is live ...
        (expect default-directory :to-equal "/proj/")
        ;; ... until the transient exits, which restores the directory.
        (run-hooks 'transient-exit-hook)
        (expect default-directory :to-equal "/old/")))))

(describe "projectile-switch-project-by-name with a transient action"
  ;; A transient (e.g. `projectile-dispatch') must be detected and routed
  ;; through `projectile--dispatch-in-directory' whether it's reached via the
  ;; prefix argument or set as `projectile-switch-project-action' (#2046).
  (before-each
    (spy-on 'projectile-project-p :and-return-value t)
    (spy-on 'projectile-project-root :and-return-value "/old/")
    (spy-on 'projectile--dispatch-in-directory)
    ;; A stand-in transient prefix: the routing keys off the
    ;; `transient--prefix' property, not on transient being installed.
    (fset 'projectile-test-fake-prefix #'ignore)
    (put 'projectile-test-fake-prefix 'transient--prefix t))
  (after-each
    (fmakunbound 'projectile-test-fake-prefix))

  (it "routes a transient `projectile-switch-project-action' (no prefix arg)"
    (let ((projectile-switch-project-action 'projectile-test-fake-prefix))
      (projectile-switch-project-by-name "/proj/"))
    (expect 'projectile--dispatch-in-directory
            :to-have-been-called-with "/proj/" 'projectile-test-fake-prefix))

  (it "routes the prefix-argument dispatch to the target project"
    (let ((orig (get 'projectile-dispatch 'transient--prefix)))
      (cl-letf (((symbol-function 'projectile-dispatch) #'ignore))
        (unwind-protect
            (progn
              (put 'projectile-dispatch 'transient--prefix t)
              (projectile-switch-project-by-name "/proj/" t))
          (put 'projectile-dispatch 'transient--prefix orig))))
    (expect 'projectile--dispatch-in-directory
            :to-have-been-called-with "/proj/" 'projectile-dispatch)))

(describe "projectile-switch-project-by-name"
  (it "calls the switch project action with project-to-switch's dir-locals loaded"
    (defvar switch-project-foo)
    (let ((foo 'bar)
          (switch-project-foo)
          (safe-local-variable-values '((foo . baz)))
          (projectile-switch-project-action (lambda () (setq switch-project-foo foo))))
      (projectile-test-with-sandbox
        (projectile-test-with-files
            ("project/"
             "project/.dir-locals.el"
             "project/.projectile")
          (append-to-file
           "((nil . ((foo . baz))))" nil "project/.dir-locals.el")
          (projectile-add-known-project (file-name-as-directory (expand-file-name "project")))
          (projectile-switch-project-by-name (file-name-as-directory (expand-file-name "project")))

          (expect switch-project-foo :to-be 'baz)))))

  (it "runs hooks from the project root directory"
      (defvar hook-dir)
      (let ((projectile-switch-project-action
             (lambda () (switch-to-buffer (find-file-noselect "file" t))))
            (hook (lambda () (setq hook-dir default-directory))))
        (add-hook 'projectile-after-switch-project-hook hook)
        (projectile-test-with-sandbox
         (projectile-test-with-files
          ("project/"
           "project/file")
          (let ((project-dir (file-name-as-directory (expand-file-name "project"))))
            (projectile-add-known-project project-dir)
            (projectile-switch-project-by-name project-dir)
            (remove-hook 'projectile-after-switch-project-hook hook)

            (expect hook-dir :to-equal project-dir))))))

  (it "ensures the buffer is switched immediately"
      (let ((projectile-switch-project-action
             (lambda () (switch-to-buffer (find-file-noselect "file" t)))))
        (projectile-test-with-sandbox
         (projectile-test-with-files
          ("project/"
           "project/file")
          (projectile-add-known-project (file-name-as-directory (expand-file-name "project")))
          (projectile-switch-project-by-name (file-name-as-directory (expand-file-name "project")))

          (expect (current-buffer) :to-be (get-file-buffer "project/file"))))))

  (it "recognizes Mercurial project roots when switching known projects"
      (defvar switch-project-root)
      (defvar switch-project-vcs)
      (let ((projectile-switch-project-action
             (lambda ()
               (setq switch-project-root (projectile-acquire-root))
               (setq switch-project-vcs (projectile-project-vcs switch-project-root)))))
        (projectile-test-with-sandbox
         (projectile-test-with-files
          ("project/.hg/" "project/file")
          (let ((project-dir (file-name-as-directory (expand-file-name "project"))))
            (projectile-add-known-project project-dir)
            (projectile-switch-project-by-name project-dir)
            (expect switch-project-root :to-equal project-dir)
            (expect switch-project-vcs :to-equal 'hg)
            (expect switch-project-root :not :to-equal (file-name-as-directory (expand-file-name "~")))))))))

;;; projectile-switch-test.el ends here
