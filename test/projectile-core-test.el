;;; projectile-core-test.el --- Tests for core utilities (dispatch, naming, paths, mode) -*- lexical-binding: t -*-

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

;; Tests for core utilities (dispatch, naming, paths, mode).

;;; Code:

(require 'projectile-test-helpers)

;; Embark and Marginalia aren't dependencies, so their registries are usually
;; unbound here.  Declare them special so the integration specs can `let'-bind
;; them dynamically and watch the setup functions write to them.
(defvar embark-transformer-alist)
(defvar embark-keymap-alist)
(defvar embark-general-map)
(defvar marginalia-annotators)
(defvar marginalia-annotator-registry)

(describe "projectile-dispatch"
  (it "is defined as a command when transient is available"
    (assume (require 'transient nil t) "transient is not available")
    (expect (commandp 'projectile-dispatch) :to-be-truthy)))

(describe "projectile-project-name"
  (it "return projectile-project-name when present"
    (let ((projectile-project-name "name"))
      (expect (projectile-project-name) :to-equal "name")
      (expect (projectile-project-name "other") :to-equal "name")))
  (it "uses projectile-project-name-function to get the project name from the project dir"
    (let ((projectile-project-name-function (lambda (dir) dir)))
      (expect (projectile-project-name "some/dir") :to-equal "some/dir")))
  (it "acts on the current project is not passed a project dir explicitly"
    (spy-on 'projectile-project-root :and-return-value "current/project")
    (let ((projectile-project-name-function (lambda (dir) dir)))
      (expect (projectile-project-name) :to-equal "current/project"))))

(describe "projectile-uniquify-dirname-transform"
  (it "splices the project name into a directory inside a project"
    (spy-on 'projectile-project-root :and-return-value "/home/me/work/checkout-42/")
    (spy-on 'projectile-project-name :and-return-value "myproject")
    (expect (projectile-uniquify-dirname-transform "/home/me/work/checkout-42/src/")
            :to-equal "/home/me/work/checkout-42/myproject/src/"))
  (it "returns the directory unchanged outside a project"
    (spy-on 'projectile-project-root :and-return-value nil)
    (expect (projectile-uniquify-dirname-transform "/tmp/not-a-project/")
            :to-equal "/tmp/not-a-project/")))

(describe "projectile-prepend-project-name"
  (it "prepends the project name to its parameter"
    (spy-on 'projectile-project-name :and-return-value "project")
    (expect (projectile-prepend-project-name "Test") :to-equal "[project] Test")))

(describe "projectile-expand-root"
  (it "expands a relative path into an absolute path within a project"
    (spy-on  'projectile-project-root :and-return-value "/path/to/project")
    (expect (projectile-expand-root "foo") :to-equal "/path/to/project/foo")
    (expect (projectile-expand-root "foo/bar") :to-equal "/path/to/project/foo/bar")
    (expect (projectile-expand-root "./foo/bar") :to-equal "/path/to/project/foo/bar")))

(describe "projectile--project-relative-name"
  (it "spells an absolute path relative to a matching root"
    (expect (projectile--project-relative-name
             "/home/me/project/src/foo.el" "/home/me/project/")
            :to-equal "src/foo.el"))
  (it "signals a divergent spelling with a leading ../ segment"
    (expect (string-prefix-p
             ".." (projectile--project-relative-name
                   "/elsewhere/foo.el" "/home/me/project/"))
            :to-be t)))

(describe "projectile--known-project-root"
  (it "abbreviates the root and guarantees a trailing slash"
    (spy-on 'abbreviate-file-name :and-call-fake #'identity)
    (expect (projectile--known-project-root "/home/me/project")
            :to-equal "/home/me/project/")
    (expect (projectile--known-project-root "/home/me/project/")
            :to-equal "/home/me/project/")))

(describe "projectile-expand-file-name-wildcard"
  (it "expands a filename not containing wildcards"
    (expect (projectile-expand-file-name-wildcard "test" "/path/to/project/")
            :to-equal "/path/to/project/test"))
  (it "does not try to resolve wildcards if there are none in the pattern"
    (spy-on 'file-expand-wildcards)
    (expect (projectile-expand-file-name-wildcard "foo" "/path/to/project/")
            :to-equal "/path/to/project/foo")
    (expect (spy-calls-any 'file-expand-wildcards) :to-equal nil))
  (it "returns the first wildcard result if any exist"
    (spy-on 'file-expand-wildcards
            :and-return-value '("/path/to/project/one"
                                "/path/to/project/two"))
    (expect (projectile-expand-file-name-wildcard "*" "/path/to/project")
            :to-equal "/path/to/project/one"))
  (it "returns the expanded result if the are no wildcard results"
    (expect (projectile-expand-file-name-wildcard "*" "/path/to/project-b")
            :to-equal "/path/to/project-b/*")))

(describe "projectile-mode"
  (before-each
    (spy-on 'projectile--cleanup-known-projects)
    (spy-on 'projectile-discover-projects-in-search-path))
  (it "sets up hook functions"
    (projectile-mode 1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :to-be-truthy)
    (projectile-mode -1)
    (expect (memq 'projectile-find-file-hook-function find-file-hook) :not :to-be-truthy))
  (it "forgets the last-seen project when disabled"
    (spy-on 'projectile--teardown-all-watches)
    (setq projectile--current-project "/some/project/")
    (projectile-mode -1)
    (expect projectile--current-project :to-be nil)))

(describe "projectile-default-mode-line"
  (it "includes the project name and type when in a project"
    (spy-on 'projectile-project-name :and-return-value "foo")
    (spy-on 'projectile-project-type :and-return-value "bar")
    (expect (projectile-default-mode-line) :to-equal " Projectile[foo:bar]"))
  (it "returns also a - if called outside a project"
    (spy-on 'projectile-project-name :and-return-value nil)
    (spy-on 'projectile-project-type :and-return-value nil)
    (expect (projectile-default-mode-line) :to-equal " Projectile[-]"))
  (it "respects the value of projectile-mode-line-prefix"
    (spy-on 'projectile-project-name :and-return-value "foo")
    (spy-on 'projectile-project-type :and-return-value "bar")
    (let ((projectile-mode-line-prefix " Pro"))
      (expect (projectile-default-mode-line) :to-equal " Pro[foo:bar]"))))

(describe "projectile-purge-file-from-cache"
  (it "serializes the updated cache without the purged file"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching 'persistent))
      (puthash "/project/" '("foo.el" "bar.el" "baz.el") projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile-serialize)
      (spy-on 'projectile-project-cache-file :and-return-value "/tmp/cache.eld")
      (projectile-purge-file-from-cache "bar.el")
      ;; The in-memory cache should be updated
      (expect (gethash "/project/" projectile-projects-cache) :to-equal '("foo.el" "baz.el"))
      ;; projectile-serialize should be called with the updated list, not the stale one
      (expect 'projectile-serialize :to-have-been-called-with '("foo.el" "baz.el") "/tmp/cache.eld")))
  (it "re-derives file-notify watches from the updated cache"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching t))
      (puthash "/project/" '("foo.el" "bar.el") projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile--maybe-watch-project)
      (projectile-purge-file-from-cache "bar.el")
      (expect 'projectile--maybe-watch-project
              :to-have-been-called-with "/project/" '("foo.el")))))

(describe "projectile-purge-dir-from-cache"
  (it "removes files under the directory from the in-memory cache"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching t))
      (puthash "/project/"
               '("src/foo.el" "src/sub/bar.el" "test/baz.el")
               projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (projectile-purge-dir-from-cache "src/")
      (expect (gethash "/project/" projectile-projects-cache)
              :to-equal '("test/baz.el"))))
  (it "serializes the updated cache when persistent caching is enabled"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching 'persistent))
      (puthash "/project/"
               '("src/foo.el" "src/sub/bar.el" "test/baz.el")
               projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile-serialize)
      (spy-on 'projectile-project-cache-file :and-return-value "/tmp/cache.eld")
      (projectile-purge-dir-from-cache "src/")
      (expect 'projectile-serialize
              :to-have-been-called-with '("test/baz.el") "/tmp/cache.eld")))
  (it "does not touch disk when persistent caching is disabled"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching t))
      (puthash "/project/"
               '("src/foo.el" "test/baz.el")
               projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile-serialize)
      (projectile-purge-dir-from-cache "src/")
      (expect 'projectile-serialize :not :to-have-been-called)))
  (it "re-derives file-notify watches from the updated cache"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching t))
      (puthash "/project/" '("src/foo.el" "test/baz.el") projectile-projects-cache)
      (spy-on 'projectile-project-root :and-return-value "/project/")
      (spy-on 'projectile--maybe-watch-project)
      (projectile-purge-dir-from-cache "src/")
      (expect 'projectile--maybe-watch-project
              :to-have-been-called-with "/project/" '("test/baz.el")))))

(describe "projectile-sort-by-modification-time"
  (it "sorts files by modification time in descending order"
    (projectile-test-with-sandbox
      (projectile-test-with-files
        ("old.el" "new.el")
        ;; Touch old.el first, then new.el with a delay to ensure different mtimes
        (spy-on 'projectile-project-root :and-return-value default-directory)
        (let ((now (current-time))
              (old-time (time-subtract (current-time) 100)))
          (spy-on 'file-attributes :and-call-fake
                  (lambda (file &rest _)
                    (let ((attrs (make-list 12 nil)))
                      ;; Set modification time (index 5)
                      (setf (nth 5 attrs) (if (string-match-p "old" file) old-time now))
                      attrs)))
          (expect (projectile-sort-by-modification-time '("old.el" "new.el"))
                  :to-equal '("new.el" "old.el"))))))
  (it "handles deleted files (nil file-attributes) without error"
    (spy-on 'projectile-project-root :and-return-value "/project/")
    (spy-on 'file-attributes :and-return-value nil)
    (expect (projectile-sort-by-modification-time '("gone.el" "also-gone.el"))
            :not :to-throw)))

(describe "projectile-parent"
  (it "returns the parent directory of a file path"
    (expect (projectile-parent "/a/b/c.el") :to-equal "/a/b"))
  (it "returns the parent directory of a directory path with a trailing slash"
    (expect (projectile-parent "/a/b/") :to-equal "/a"))
  (it "treats a slash-less directory the same as one with a trailing slash"
    (expect (projectile-parent "/a/b") :to-equal "/a")))

(describe "projectile--directory-ancestors"
  (it "returns the file's directory and every ancestor, outermost first"
    (expect (projectile--directory-ancestors "src/foo/bar.el")
            :to-equal '("src/" "src/foo/")))
  (it "walks an absolute path up to the root"
    (expect (projectile--directory-ancestors "/a/b/c")
            :to-equal '("/" "/a/" "/a/b/")))
  (it "returns nil for a bare file name with no directory part"
    (expect (projectile--directory-ancestors "foo.el") :to-equal nil)))

(describe "projectile-default-project-name"
  (it "uses the last path segment as the project name"
    (expect (projectile-default-project-name "/home/me/projects/projectile/")
            :to-equal "projectile"))
  (it "does not care whether the root has a trailing slash"
    (expect (projectile-default-project-name "/home/me/projects/projectile")
            :to-equal "projectile")))

(describe "projectile-make-relative-to-root"
  (it "rewrites absolute project files as paths relative to the root"
    (spy-on 'projectile-project-root :and-return-value "/home/me/project/")
    (expect (projectile-make-relative-to-root
             '("/home/me/project/src/foo.el" "/home/me/project/README.md"))
            :to-equal '("src/foo.el" "README.md"))))

(describe "projectile-completing-read"
  (before-each
    ;; avoid resolving a real project name for the prompt
    (spy-on 'projectile-prepend-project-name :and-call-fake #'identity))

  (it "reads with completing-read by default and applies the action"
    (let ((projectile-completion-system 'default))
      (spy-on 'completing-read :and-return-value "picked")
      (expect (projectile-completing-read "Prompt" '("a" "b") :action #'upcase)
              :to-equal "PICKED")
      (expect 'completing-read :to-have-been-called)))

  (it "calls a custom function when `projectile-completion-system' is one"
    (let ((projectile-completion-system (lambda (_prompt choices) (car choices))))
      (expect (projectile-completing-read "Prompt" '("first" "second"))
              :to-equal "first")))

  (it "falls back to completing-read for removed legacy values like `ido'"
    (let ((projectile-completion-system 'ido))
      (spy-on 'completing-read :and-return-value "x")
      (expect (projectile-completing-read "Prompt" '("x")) :to-equal "x")
      (expect 'completing-read :to-have-been-called)))

  ;; The completion metadata category is what marginalia and embark key off
  ;; to annotate and act on candidates, so it must reflect what they are.
  (it "advertises the project-file category by default"
    (let ((projectile-completion-system 'default) table)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt coll &rest _) (setq table coll) "x"))
      (projectile-completing-read "Prompt" '("a"))
      (expect (alist-get 'category (cdr (funcall table "" nil 'metadata)))
              :to-be 'project-file)))

  (it "honors an explicit :category"
    (let ((projectile-completion-system 'default) table)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt coll &rest _) (setq table coll) "x"))
      (projectile-completing-read "Prompt" '("a") :category 'buffer)
      (expect (alist-get 'category (cdr (funcall table "" nil 'metadata)))
              :to-be 'buffer)))

  (it "omits the category when :category is nil"
    (let ((projectile-completion-system 'default) table)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt coll &rest _) (setq table coll) "x"))
      (projectile-completing-read "Prompt" '("a") :category nil)
      (expect (assq 'category (cdr (funcall table "" nil 'metadata)))
              :to-be nil)))

  (it "buffer switching advertises the buffer category"
    (let (table)
      (spy-on 'projectile-project-buffer-names :and-return-value '("b1" "b2"))
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt coll &rest _) (setq table coll) "b1"))
      (projectile-read-buffer-to-switch "Switch: ")
      (expect (alist-get 'category (cdr (funcall table "" nil 'metadata)))
              :to-be 'buffer))))

(describe "Embark integration"
  (it "resolves a project-file target that exists under the root"
    (spy-on 'projectile-project-root :and-return-value "/proj/")
    (spy-on 'file-exists-p :and-return-value t)
    (expect (projectile--embark-project-file-target "src/foo.el")
            :to-equal '(file . "/proj/src/foo.el")))

  (it "declines (returns nil) outside a project, so Embark's own handling wins"
    (spy-on 'projectile-project-root :and-return-value nil)
    (expect (projectile--embark-project-file-target "src/foo.el") :to-be nil))

  (it "declines when the target does not live under the project root"
    (spy-on 'projectile-project-root :and-return-value "/proj/")
    (spy-on 'file-exists-p :and-return-value nil)
    (expect (projectile--embark-project-file-target "src/foo.el") :to-be nil))

  (it "defers to Embark's previous transformer when Projectile declines"
    ;; augment, never replace: a non-Projectile project-file target must reach
    ;; whatever transformer Embark already had
    (spy-on 'projectile-project-root :and-return-value nil)
    (let ((projectile--embark-project-file-prev-transform
           (lambda (_type target) (cons 'file (concat "/other/" target)))))
      (expect (projectile--embark-project-file-transform 'project-file "x")
              :to-equal '(file . "/other/x"))))

  (it "leaves the target unchanged when neither Projectile nor a prior transformer resolves it"
    (spy-on 'projectile-project-root :and-return-value nil)
    (let ((projectile--embark-project-file-prev-transform nil))
      (expect (projectile--embark-project-file-transform 'project-file "x")
              :to-equal '(project-file . "x"))))

  (it "binds project actions in the project action keymap"
    (expect (keymapp projectile-embark-project-map) :to-be-truthy)
    (expect (lookup-key projectile-embark-project-map "s")
            :to-be 'projectile-embark-switch-project)
    (expect (lookup-key projectile-embark-project-map "D")
            :to-be 'projectile-remove-known-project))

  (it "the switch action delegates to projectile-switch-project-by-name"
    (spy-on 'projectile-switch-project-by-name)
    (projectile-embark-switch-project "/proj/")
    (expect 'projectile-switch-project-by-name :to-have-been-called-with "/proj/"))

  (it "registers the transformer and the keymap, preserving Embark's own transformer"
    (let* ((prev (lambda (type target) (cons type target)))
           (embark-general-map (make-sparse-keymap))
           (embark-transformer-alist (list (cons 'project-file prev)))
           (embark-keymap-alist nil)
           (projectile--embark-project-file-prev-transform nil))
      (projectile--embark-setup)
      (expect (alist-get 'project-file embark-transformer-alist)
              :to-be #'projectile--embark-project-file-transform)
      (expect projectile--embark-project-file-prev-transform :to-be prev)
      (expect (alist-get 'projectile-project embark-keymap-alist)
              :to-be 'projectile-embark-project-map)
      (expect (keymap-parent projectile-embark-project-map)
              :to-be embark-general-map)))

  (it "is idempotent, so re-loading Projectile doesn't wrap its own wrapper"
    (let* ((prev (lambda (type target) (cons type target)))
           (embark-general-map (make-sparse-keymap))
           (embark-transformer-alist (list (cons 'project-file prev)))
           (embark-keymap-alist nil)
           (projectile--embark-project-file-prev-transform nil))
      (projectile--embark-setup)
      (projectile--embark-setup)
      (expect projectile--embark-project-file-prev-transform :to-be prev)
      (expect (length embark-keymap-alist) :to-equal 1))))

(describe "Marginalia integration"
  (it "registers the file annotator for project candidates"
    ;; the registry Marginalia actually ships today
    (let ((marginalia-annotators '((file marginalia-annotate-file builtin none))))
      (projectile--marginalia-setup)
      (expect (alist-get 'projectile-project marginalia-annotators)
              :to-equal '(marginalia-annotate-file builtin none))))

  (it "also registers with the registry name older Marginalia releases used"
    (let ((marginalia-annotator-registry '((file marginalia-annotate-file builtin none))))
      (projectile--marginalia-setup)
      (expect (alist-get 'projectile-project marginalia-annotator-registry)
              :to-equal '(marginalia-annotate-file builtin none))))

  (it "does nothing when neither registry exists, instead of erroring"
    (expect (projectile--marginalia-setup) :not :to-throw)))

(describe "projectile-sort-files"
  (it "returns the files unchanged for the default sort order"
    (let ((projectile-sort-order 'default))
      (expect (projectile-sort-files '("b" "a")) :to-equal '("b" "a"))))

  (it "supports a custom sort function"
    (let ((projectile-sort-order
           (lambda (files) (seq-sort-by #'length #'< files))))
      (expect (projectile-sort-files '("dir/long.el" "a.el" "med.el"))
              :to-equal '("a.el" "med.el" "dir/long.el"))))

  (it "leaves the files unsorted for an unrecognized value"
    ;; A typo'd sort order must not present the project as empty.
    (let ((projectile-sort-order 'no-such-order))
      (expect (projectile-sort-files '("b" "a")) :to-equal '("b" "a")))))

(describe "projectile-verbose"
  ;; The line it draws: Projectile stays quiet about what it does off its
  ;; own bat, and still answers for what you asked it to do.

  (describe "caching the file you just opened"
    (it "says nothing when the find-file hook did it"
      (projectile-test-with-project (("main.el" . ";; x"))
        (let ((projectile-verbose nil)
              (projectile-enable-caching t)
              (root (projectile-project-root)))
          (puthash root '("other.el") projectile-projects-cache)
          (spy-on 'message)
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "main.el" root))
            (projectile-cache-current-file root))
          ;; it still cached the file...
          (expect (gethash root projectile-projects-cache) :to-contain "main.el")
          ;; ...without announcing it
          (expect 'message :not :to-have-been-called))))

    (it "still answers when you invoke it yourself"
      (projectile-test-with-project (("main.el" . ";; x"))
        (let ((projectile-verbose nil)
              (projectile-enable-caching t)
              (root (projectile-project-root)))
          (puthash root '("other.el") projectile-projects-cache)
          (spy-on 'message)
          (spy-on 'called-interactively-p :and-return-value t)
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "main.el" root))
            (projectile-cache-current-file root))
          (expect 'message :to-have-been-called)))))

  (describe "a search path entry that doesn't exist"
    (it "says nothing during the automatic scan"
      (let ((projectile-verbose nil))
        (spy-on 'message)
        (projectile-discover-projects-in-directory "/no/such/directory/")
        (expect 'message :not :to-have-been-called)))

    (it "still says so when you run the command"
      (let ((projectile-verbose nil))
        (spy-on 'message)
        (spy-on 'called-interactively-p :and-return-value t)
        (projectile-discover-projects-in-directory "/no/such/directory/")
        (expect 'message :to-have-been-called))))

  (describe "an indexing command that exits non-zero with output"
    (it "keeps the note to itself, but still uses the output"
      (projectile-test-with-sandbox
       (projectile-test-with-files ("project/" "project/.projectile")
         (let ((default-directory (projectile-test-project-root))
               (projectile-verbose nil))
           (spy-on 'message)
           (expect (projectile-files-via-ext-command
                    default-directory "printf 'a.el\\0'; echo boom >&2; exit 1")
                   :to-equal '("a.el"))
           (expect 'message :not :to-have-been-called)))))

    (it "mentions it when asked to be verbose"
      (projectile-test-with-sandbox
       (projectile-test-with-files ("project/" "project/.projectile")
         (let ((default-directory (projectile-test-project-root))
               (projectile-verbose t))
           (spy-on 'message)
           (projectile-files-via-ext-command
            default-directory "printf 'a.el\\0'; echo boom >&2; exit 1")
           (expect 'message :to-have-been-called))))))

  (describe "a session file from another format version"
    (it "is skipped silently, since restore-all can meet a directory of them"
      (projectile-test-with-temp-files ((file ".eld"))
        (let ((projectile-verbose nil))
          (projectile-serialize '(:projectile-session-version 0 :tabs nil) file)
          (spy-on 'message)
          (expect (projectile-session--read-file file) :to-be nil)
          (expect 'message :not :to-have-been-called))))

    (it "says which file it skipped when asked to be verbose"
      (projectile-test-with-temp-files ((file ".eld"))
        (let ((projectile-verbose t))
          (projectile-serialize '(:projectile-session-version 0 :tabs nil) file)
          (spy-on 'message)
          (expect (projectile-session--read-file file) :to-be nil)
          (expect 'message :to-have-been-called)))))

  (describe "what it deliberately does not cover"
    (it "leaves a command's own answer alone"
      ;; `projectile-invalidate-cache' exists to be invoked; saying nothing
      ;; would leave you wondering whether it ran.
      (projectile-test-with-project (("main.el" . ";; x"))
        (let ((projectile-verbose t)
              (root (projectile-project-root)))
          (puthash root '("main.el") projectile-projects-cache)
          (spy-on 'message)
          (projectile-invalidate-cache nil)
          (expect 'message :to-have-been-called))))))

;;; projectile-core-test.el ends here
