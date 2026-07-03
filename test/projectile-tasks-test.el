;;; projectile-tasks-test.el --- Tests for named project tasks -*- lexical-binding: t -*-

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

;; Tests for named project tasks (`projectile-tasks', `projectile-run-task',
;; `projectile-repeat-last-task').

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-tasks-safe-p"
  (it "accepts nil"
    (expect (projectile-tasks-safe-p nil) :to-be-truthy))

  (it "accepts an alist of string pairs"
    (expect (projectile-tasks-safe-p '(("lint" . "make lint")
                                       ("docs" . "make docs")))
            :to-be-truthy))

  (it "rejects entries with function commands"
    (expect (projectile-tasks-safe-p '(("lint" . ignore))) :to-be nil)
    (expect (projectile-tasks-safe-p '(("lint" . (lambda () "make lint"))))
            :to-be nil)
    (expect (projectile-tasks-safe-p `(("lint" . ,(lambda () "make lint"))))
            :to-be nil))

  (it "rejects malformed values"
    (expect (projectile-tasks-safe-p "make lint") :to-be nil)
    (expect (projectile-tasks-safe-p '("lint")) :to-be nil)
    (expect (projectile-tasks-safe-p '((lint . "make lint"))) :to-be nil)
    ;; improper lists mustn't error, just be unsafe
    (expect (projectile-tasks-safe-p '(("lint" . "make lint") . "junk"))
            :to-be nil)))

(describe "projectile-project-tasks"
  (it "merges the project type's tasks with projectile-tasks, the latter winning"
    (let ((projectile-project-types projectile-project-types)
          (projectile-project-root-files projectile-project-root-files)
          (projectile-tasks '(("lint" . "make custom-lint")
                              ("docs" . "make docs"))))
      (projectile-register-project-type 'tasked-project '("Taskedfile")
                                        :tasks '(("lint" . "make lint")
                                                 ("bench" . "make bench")))
      (spy-on 'projectile-project-type :and-return-value 'tasked-project)
      (expect (projectile-project-tasks)
              :to-equal '(("lint" . "make custom-lint")
                          ("docs" . "make docs")
                          ("bench" . "make bench")))))

  (it "returns just the project type's tasks when projectile-tasks is nil"
    (let ((projectile-project-types projectile-project-types)
          (projectile-project-root-files projectile-project-root-files)
          (projectile-tasks nil))
      (projectile-register-project-type 'tasked-project '("Taskedfile")
                                        :tasks '(("bench" . "make bench")))
      (expect (projectile-project-tasks 'tasked-project)
              :to-equal '(("bench" . "make bench")))))

  (it "returns just projectile-tasks for a type without tasks"
    (let ((projectile-tasks '(("lint" . "make lint"))))
      (expect (projectile-project-tasks 'generic)
              :to-equal '(("lint" . "make lint")))))

  (it "picks up tasks added via projectile-update-project-type"
    (let ((projectile-project-types projectile-project-types)
          (projectile-project-root-files projectile-project-root-files)
          (projectile-tasks nil))
      (projectile-register-project-type 'tasked-project '("Taskedfile"))
      (projectile-update-project-type 'tasked-project
                                      :tasks '(("bench" . "make bench")))
      (expect (projectile-project-tasks 'tasked-project)
              :to-equal '(("bench" . "make bench"))))))

(describe "projectile-run-task"
  (before-each
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-project-name :and-return-value "myproj")
    (spy-on 'projectile-project-type :and-return-value 'generic)
    (spy-on 'projectile-compilation-dir :and-return-value "/proj/")
    (spy-on 'file-directory-p :and-return-value t)
    (spy-on 'save-some-buffers)
    ;; The default confirmation prompt (see the security specs below)
    ;; is accepted unchanged, so the mechanical specs stay hands-off.
    (spy-on 'projectile-read-command :and-call-fake
            (lambda (_prompt command &optional _history) command)))

  (it "runs the selected task through the compile machinery"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "lint")
    (let ((projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("lint" . "make lint"))))
      (projectile-run-task nil)
      (expect 'projectile-run-compilation
              :to-have-been-called-with "make lint" nil)
      ;; recorded into both the per-task and the combined history
      (expect (ring-elements
               (projectile--get-command-history "/proj/" '(task . "lint")))
              :to-equal '("make lint"))
      (expect (ring-elements (projectile--get-command-history "/proj/"))
              :to-equal '("make lint"))))

  (it "confirms the command before running by default"
    ;; Task commands can come from a checked-out .dir-locals.el, so the
    ;; run-time confirmation (like compile's) is a security requirement,
    ;; not a convenience - see projectile--run-task.
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "lint")
    (let ((compilation-read-command t)
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("lint" . "make lint"))))
      (projectile-run-task nil)
      (expect 'projectile-read-command :to-have-been-called)))

  (it "runs without confirmation when compilation-read-command is nil"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "lint")
    (let ((compilation-read-command nil)
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("lint" . "make lint"))))
      (projectile-run-task nil)
      (expect 'projectile-read-command :not :to-have-been-called)
      (expect 'projectile-run-compilation
              :to-have-been-called-with "make lint" nil)))

  (it "expands %p to the project name"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "dist")
    (let ((projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("dist" . "tar czf %p.tar.gz ."))))
      (projectile-run-task nil)
      (expect 'projectile-run-compilation
              :to-have-been-called-with "tar czf myproj.tar.gz ." nil)))

  (it "resolves function commands at the project root"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "gen")
    (let* ((seen-dir nil)
           (projectile-project-command-history (make-hash-table :test 'equal))
           (projectile-last-task-map (make-hash-table :test 'equal))
           (projectile-tasks `(("gen" . ,(lambda ()
                                           (setq seen-dir default-directory)
                                           "make generate")))))
      (projectile-run-task nil)
      (expect seen-dir :to-equal "/proj/")
      (expect 'projectile-run-compilation
              :to-have-been-called-with "make generate" nil)))

  (it "errors when a task's function command doesn't return a string"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "bad")
    (let ((projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("bad" . ignore))))
      (expect (projectile-run-task nil) :to-throw 'user-error)
      (expect 'projectile-run-compilation :not :to-have-been-called)))

  (it "lets the user edit the command with a prefix arg"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "lint")
    (spy-on 'projectile-read-command :and-return-value "make lint --fix")
    (let ((projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("lint" . "make lint"))))
      (projectile-run-task '(4))
      ;; the prompt was prefilled with the task's command
      (expect (nth 1 (spy-calls-args-for 'projectile-read-command 0))
              :to-equal "make lint")
      (expect 'projectile-run-compilation
              :to-have-been-called-with "make lint --fix" nil)
      (expect (ring-elements
               (projectile--get-command-history "/proj/" '(task . "lint")))
              :to-equal '("make lint --fix"))))

  (it "errors when the project defines no tasks"
    (spy-on 'projectile-run-compilation)
    (let ((projectile-tasks nil))
      (expect (projectile-run-task nil) :to-throw 'user-error)
      (expect 'projectile-run-compilation :not :to-have-been-called)))

  (it "errors when the selected task doesn't exist"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "nope")
    (let ((projectile-tasks '(("lint" . "make lint"))))
      (expect (projectile-run-task nil) :to-throw 'user-error)
      (expect 'projectile-run-compilation :not :to-have-been-called)))

  (it "names the task buffer after the task"
    (let ((buffer-name nil))
      (spy-on 'projectile-run-compilation :and-call-fake
              (lambda (&rest _)
                (setq buffer-name
                      (funcall compilation-buffer-name-function "compilation"))))
      (spy-on 'projectile-completing-read :and-return-value "lint")
      (let ((projectile-project-command-history (make-hash-table :test 'equal))
            (projectile-last-task-map (make-hash-table :test 'equal))
            (projectile-per-project-compilation-buffer nil)
            (projectile-tasks '(("lint" . "make lint"))))
        (projectile-run-task nil)
        (expect buffer-name :to-equal "*projectile-task: lint*"))))

  (it "appends the project name to the task buffer with per-project compilation buffers"
    (let ((buffer-name nil))
      (spy-on 'projectile-run-compilation :and-call-fake
              (lambda (&rest _)
                (setq buffer-name
                      (funcall compilation-buffer-name-function "compilation"))))
      (spy-on 'projectile-completing-read :and-return-value "lint")
      (let ((projectile-project-command-history (make-hash-table :test 'equal))
            (projectile-last-task-map (make-hash-table :test 'equal))
            (projectile-per-project-compilation-buffer t)
            (projectile-tasks '(("lint" . "make lint"))))
        (projectile-run-task nil)
        (expect buffer-name :to-equal "*projectile-task: lint*<myproj>")))))

(describe "projectile-repeat-last-task"
  (before-each
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-project-name :and-return-value "myproj")
    (spy-on 'projectile-project-type :and-return-value 'generic)
    (spy-on 'projectile-compilation-dir :and-return-value "/proj/")
    (spy-on 'file-directory-p :and-return-value t)
    (spy-on 'save-some-buffers)
    (spy-on 'projectile-read-command :and-call-fake
            (lambda (_prompt command &optional _history) command)))

  (it "errors when no task has been run yet"
    (spy-on 'projectile-run-compilation)
    (let ((projectile-last-task-map (make-hash-table :test 'equal)))
      (expect (projectile-repeat-last-task nil) :to-throw 'user-error)
      (expect 'projectile-run-compilation :not :to-have-been-called)))

  (it "re-runs the last executed task"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "lint")
    (let ((projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("lint" . "make lint"))))
      (projectile-run-task nil)
      (projectile-repeat-last-task nil)
      (expect 'projectile-run-compilation :to-have-been-called-times 2)
      (expect (spy-calls-args-for 'projectile-run-compilation 1)
              :to-equal '("make lint" nil))))

  (it "does not re-confirm the already-confirmed command"
    ;; Repeating mirrors projectile-repeat-last-command: the command was
    ;; confirmed when it first ran.
    (spy-on 'projectile-run-compilation)
    (let ((compilation-read-command t)
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal)))
      (puthash "/proj/" '("lint" . "make lint") projectile-last-task-map)
      (projectile-repeat-last-task nil)
      (expect 'projectile-read-command :not :to-have-been-called)
      (expect 'projectile-run-compilation
              :to-have-been-called-with "make lint" nil)))

  (it "repeats the edited command, not the task's original one"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "lint")
    (spy-on 'projectile-read-command :and-return-value "make lint --fix")
    (let ((projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("lint" . "make lint"))))
      (projectile-run-task '(4))
      (projectile-repeat-last-task nil)
      (expect (spy-calls-args-for 'projectile-run-compilation 1)
              :to-equal '("make lint --fix" nil))))

  (it "is scoped to the current project"
    (spy-on 'projectile-run-compilation)
    (let ((projectile-last-task-map (make-hash-table :test 'equal)))
      (puthash "/other-proj/" '("lint" . "make lint") projectile-last-task-map)
      (expect (projectile-repeat-last-task nil) :to-throw 'user-error))))

(provide 'projectile-tasks-test)
;;; projectile-tasks-test.el ends here
