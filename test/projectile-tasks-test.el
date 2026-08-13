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
          (projectile-discover-tasks nil)
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
          (projectile-discover-tasks nil)
          (projectile-tasks nil))
      (projectile-register-project-type 'tasked-project '("Taskedfile")
                                        :tasks '(("bench" . "make bench")))
      (expect (projectile-project-tasks 'tasked-project)
              :to-equal '(("bench" . "make bench")))))

  (it "returns just projectile-tasks for a type without tasks"
    (let ((projectile-tasks '(("lint" . "make lint")))
          (projectile-discover-tasks nil))
      (expect (projectile-project-tasks 'generic)
              :to-equal '(("lint" . "make lint")))))

  (it "picks up tasks added via projectile-update-project-type"
    (let ((projectile-project-types projectile-project-types)
          (projectile-project-root-files projectile-project-root-files)
          (projectile-discover-tasks nil)
          (projectile-tasks nil))
      (projectile-register-project-type 'tasked-project '("Taskedfile"))
      (projectile-update-project-type 'tasked-project
                                      :tasks '(("bench" . "make bench")))
      (expect (projectile-project-tasks 'tasked-project)
              :to-equal '(("bench" . "make bench"))))))

(describe "task discovery"
  (describe "projectile-tasks-from-npm"
    (it "reads the scripts of a package.json"
      (projectile-test-with-project
          (("package.json" . "{\"name\": \"demo\",
  \"scripts\": {\"build\": \"tsc\", \"test\": \"vitest run\"}}"))
        (expect (projectile-tasks-from-npm (projectile-project-root))
                :to-equal '(("npm:build" . "npm run build")
                            ("npm:test" . "npm run test")))))

    (it "runs the scripts through the package manager the lock file points at"
      (projectile-test-with-project
          (("package.json" . "{\"scripts\": {\"build\": \"tsc\"}}")
           ("pnpm-lock.yaml" . "lockfileVersion: '9.0'\n"))
        (expect (projectile-tasks-from-npm (projectile-project-root))
                :to-equal '(("pnpm:build" . "pnpm run build"))))
      (projectile-test-with-project
          (("package.json" . "{\"scripts\": {\"build\": \"tsc\"}}")
           ("bun.lock" . "{}"))
        (expect (projectile-tasks-from-npm (projectile-project-root))
                :to-equal '(("bun:build" . "bun run build")))))

    (it "returns nothing for a package.json without scripts"
      (projectile-test-with-project (("package.json" . "{\"name\": \"demo\"}"))
        (expect (projectile-tasks-from-npm (projectile-project-root)) :to-be nil)))

    (it "returns nothing when there is no package.json"
      (projectile-test-with-project (("README" . "hi"))
        (expect (projectile-tasks-from-npm (projectile-project-root)) :to-be nil))))

  (describe "projectile-tasks-from-deno"
    (it "reads the tasks of a deno.json"
      (projectile-test-with-project
          (("deno.json" . "{\"tasks\": {\"dev\": \"deno run -A main.ts\"}}"))
        (expect (projectile-tasks-from-deno (projectile-project-root))
                :to-equal '(("deno:dev" . "deno task dev"))))))

  (describe "projectile-tasks-from-composer"
    (it "reads the scripts of a composer.json"
      (projectile-test-with-project
          (("composer.json" . "{\"scripts\": {\"lint\": \"php-cs-fixer fix\"}}"))
        (expect (projectile-tasks-from-composer (projectile-project-root))
                :to-equal '(("composer:lint" . "composer run-script lint"))))))

  (describe "projectile-tasks-from-just"
    (it "reads the recipes of a justfile, including quiet ones and ones with parameters"
      (projectile-test-with-project
          (("justfile" . "set shell := [\"bash\", \"-c\"]
version := \"1.0\"

# build everything
build:
    cargo build

@fmt:
    cargo fmt

test filter=\"\":
    cargo test {{filter}}
"))
        (expect (projectile-tasks-from-just (projectile-project-root))
                :to-equal '(("just:build" . "just build")
                            ("just:fmt" . "just fmt")
                            ("just:test" . "just test")))))

    (it "does not mistake an assignment for a recipe"
      (projectile-test-with-project (("justfile" . "export FOO := \"bar\"\n"))
        (expect (projectile-tasks-from-just (projectile-project-root)) :to-be nil))))

  (describe "projectile-tasks-from-taskfile"
    (it "reads the keys of the tasks mapping"
      (projectile-test-with-project
          (("Taskfile.yml" . "version: '3'

vars:
  GREETING: hello

tasks:
  build:
    cmds:
      - go build ./...
  test:
    cmds:
      - go test ./...
"))
        (expect (projectile-tasks-from-taskfile (projectile-project-root))
                :to-equal '(("task:build" . "task build")
                            ("task:test" . "task test")))))

    (it "does not pick up the keys nested inside a task"
      (projectile-test-with-project
          (("Taskfile.yml" . "tasks:
  build:
    desc: Build it
    cmds:
      - go build ./...
"))
        (expect (projectile-tasks-from-taskfile (projectile-project-root))
                :to-equal '(("task:build" . "task build"))))))

  (describe "projectile-tasks-from-rake"
    (it "reads the tasks of a Rakefile, in all the forms rake accepts"
      (projectile-test-with-project
          (("Rakefile" . "require 'rake/testtask'

desc 'Build the gem'
task build: :generate

task :coverage do
  puts 'coverage'
end

task 'legacy:import' do
end

multitask :parallel do
end
"))
        (expect (projectile-tasks-from-rake (projectile-project-root))
                :to-equal '(("rake:build" . "rake build")
                            ("rake:coverage" . "rake coverage")
                            ("rake:legacy:import" . "rake legacy:import")
                            ("rake:parallel" . "rake parallel")))))

    (it "qualifies a task with the namespaces it sits in"
      (projectile-test-with-project
          (("Rakefile" . "namespace :db do
  task :migrate do
  end

  namespace :schema do
    task :load do
    end
  end
end

task :console do
end
"))
        (expect (projectile-tasks-from-rake (projectile-project-root))
                :to-equal '(("rake:db:migrate" . "rake db:migrate")
                            ("rake:db:schema:load" . "rake db:schema:load")
                            ("rake:console" . "rake console")))))

    (it "skips a task whose name is built at runtime"
      ;; `task type, [:id]' names the task after a variable - there's no way
      ;; to know what it is without running rake.
      (projectile-test-with-project
          (("Rakefile" . "namespace :changelog do
  %w[new fix change].each do |type|
    task type, [:id] do |_task, args|
    end
  end

  task :merge do
  end
end
"))
        (expect (projectile-tasks-from-rake (projectile-project-root))
                :to-equal '(("rake:changelog:merge" . "rake changelog:merge")))))

    (it "does not mistake a method call on a block argument for a task"
      ;; `task.files = ...' inside a RakeTask block is not a definition.
      (projectile-test-with-project
          (("Rakefile" . "RuboCop::RakeTask.new(:internal_investigation) do |task|
  task.files = ['lib/rubocop/cop/*/*.rb']
  task.options = ['--no-output']
end

task :real do
end
"))
        (expect (projectile-tasks-from-rake (projectile-project-root))
                :to-equal '(("rake:real" . "rake real")))))

    (it "picks up the .rake files of the usual task directories"
      (projectile-test-with-project
          (("Rakefile" . "task :root_task do\nend\n")
           ("lib/tasks/db.rake" . "namespace :db do\n  task :seed do\n  end\nend\n")
           ("tasks/release.rake" . "task :cut_release do\nend\n")
           ("rakelib/extra.rake" . "task :extra do\nend\n")
           ("lib/tasks/notes.txt" . "task :not_a_rake_file do\nend\n"))
        (let ((names (mapcar #'car (projectile-tasks-from-rake (projectile-project-root)))))
          (expect names :to-contain "rake:root_task")
          (expect names :to-contain "rake:db:seed")
          (expect names :to-contain "rake:cut_release")
          (expect names :to-contain "rake:extra")
          (expect names :not :to-contain "rake:not_a_rake_file"))))

    (it "runs through bundler when the project has a Gemfile"
      (projectile-test-with-project
          (("Rakefile" . "task :spec do\nend\n")
           ("Gemfile" . "source 'https://rubygems.org'\n"))
        (expect (projectile-tasks-from-rake (projectile-project-root))
                :to-equal '(("rake:spec" . "bundle exec rake spec")))))

    (it "returns nothing without a Rakefile, even when .rake files exist"
      ;; Without a Rakefile there's nothing for rake to run, and skipping
      ;; the directory scan keeps this free for non-Ruby projects.
      (projectile-test-with-project
          (("lib/tasks/db.rake" . "task :seed do\nend\n"))
        (expect (projectile-tasks-from-rake (projectile-project-root)) :to-be nil))))

  (describe "projectile-tasks-from-make"
    (it "reads the named targets of a Makefile"
      (projectile-test-with-project
          (("Makefile" . "CC := gcc
.PHONY: all test

all: main.o
\t$(CC) -o demo main.o

test:
\t./run-tests

main.o: main.c
\t$(CC) -c main.c

%.o: %.c
\t$(CC) -c $<
"))
        (expect (projectile-tasks-from-make (projectile-project-root))
                :to-equal '(("make:all" . "make all")
                            ("make:test" . "make test")))))

    (it "does not mistake a variable assignment for a target"
      (projectile-test-with-project (("Makefile" . "CFLAGS := -O2\n"))
        (expect (projectile-tasks-from-make (projectile-project-root)) :to-be nil))))

  (describe "projectile-discovered-tasks"
    (it "collects the tasks of every provider"
      (projectile-test-with-project
          (("package.json" . "{\"scripts\": {\"build\": \"tsc\"}}")
           ("Makefile" . "test:\n\t./run-tests\n"))
        (expect (projectile-discovered-tasks (projectile-project-root))
                :to-equal '(("npm:build" . "npm run build")
                            ("make:test" . "make test")))))

    (it "returns nothing when discovery is off"
      (projectile-test-with-project (("package.json" . "{\"scripts\": {\"build\": \"tsc\"}}"))
        (let ((projectile-discover-tasks nil))
          (expect (projectile-discovered-tasks (projectile-project-root)) :to-be nil))))

    (it "skips a provider that signals instead of failing outright"
      (projectile-test-with-project (("Makefile" . "test:\n\t./run-tests\n"))
        (let ((projectile-task-providers
               (list (lambda (_root) (error "Boom")) #'projectile-tasks-from-make))
              (projectile-verbose nil))
          (expect (projectile-discovered-tasks (projectile-project-root))
                  :to-equal '(("make:test" . "make test"))))))

    (it "survives a malformed manifest"
      (projectile-test-with-project (("package.json" . "{not json at all"))
        (let ((projectile-verbose nil))
          (expect (projectile-discovered-tasks (projectile-project-root)) :to-be nil)))))

  (describe "projectile-project-tasks"
    (it "offers the discovered tasks after the configured ones"
      (projectile-test-with-project (("Makefile" . "test:\n\t./run-tests\n"))
        (let ((projectile-tasks '(("lint" . "make lint"))))
          (expect (projectile-project-tasks 'generic (projectile-project-root))
                  :to-equal '(("lint" . "make lint")
                              ("make:test" . "make test"))))))))

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

  (it "gives a task an interactive buffer when comint mode covers everything"
    ;; A task can be something like a sudo rebuild, which is unusable in a
    ;; read-only compilation buffer - there's nowhere to type the password
    ;; (issue #2156).
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "rebuild")
    (let ((projectile-use-comint-mode t)
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("rebuild" . "sudo make install"))))
      (projectile-run-task nil)
      (expect 'projectile-run-compilation
              :to-have-been-called-with "sudo make install" t)))

  (it "gives a task an interactive buffer when the list names `task'"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "rebuild")
    (let ((projectile-use-comint-mode '(task))
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("rebuild" . "sudo make install"))))
      (projectile-run-task nil)
      (expect 'projectile-run-compilation
              :to-have-been-called-with "sudo make install" t)))

  (it "leaves a task alone when the list names only lifecycle phases"
    (spy-on 'projectile-run-compilation)
    (spy-on 'projectile-completing-read :and-return-value "rebuild")
    (let ((projectile-use-comint-mode '(compile test))
          (projectile-project-command-history (make-hash-table :test 'equal))
          (projectile-last-task-map (make-hash-table :test 'equal))
          (projectile-tasks '(("rebuild" . "sudo make install"))))
      (projectile-run-task nil)
      (expect 'projectile-run-compilation
              :to-have-been-called-with "sudo make install" nil)))

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
    (let ((projectile-tasks '(("lint" . "make lint")))
          (projectile-discover-tasks nil))
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
            (projectile-compilation-buffer-scope nil)
            (projectile-tasks '(("lint" . "make lint"))))
        (projectile-run-task nil)
        (expect buffer-name :to-equal "*projectile-task: lint*"))))

  (it "appends the project name to the task buffer when the scope includes the project"
    (let ((buffer-name nil))
      (spy-on 'projectile-run-compilation :and-call-fake
              (lambda (&rest _)
                (setq buffer-name
                      (funcall compilation-buffer-name-function "compilation"))))
      (spy-on 'projectile-completing-read :and-return-value "lint")
      (let ((projectile-project-command-history (make-hash-table :test 'equal))
            (projectile-last-task-map (make-hash-table :test 'equal))
            (projectile-compilation-buffer-scope '(project))
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
