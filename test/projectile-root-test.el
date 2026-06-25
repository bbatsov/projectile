;;; projectile-root-test.el --- Tests for project root detection and caching -*- lexical-binding: t -*-

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

;; Tests for project root detection and caching.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-project-root return contract"
  (it "returns the absolute root directory of a project"
    (let* ((root-directory (make-temp-file "projectile-absolute" t))
           (root-file (concat root-directory "/.projectile"))
           (deep-directory (concat root-directory "/foo/bar/baz"))
           (project-file (concat deep-directory "/tmp.txt")))
      (unwind-protect
          (progn
            (mkdir deep-directory t)
            (with-temp-file root-file)
            (with-temp-file project-file)
            (with-current-buffer (find-file-noselect project-file t)
              (expect (file-name-absolute-p (projectile-project-root)) :to-be-truthy)))
        (ignore-errors (delete-directory root-directory t)))))

  (it "returns nil instead of erroring when default-directory is nil (#1829)"
    (let ((default-directory nil)
          (projectile-project-root-cache (make-hash-table :test 'equal)))
      (expect (projectile-project-root) :to-be nil))))

(describe "projectile-maybe-invalidate-cache"
  (it "should not invalidate cache if dirconfig is older than cache"
    (spy-on 'projectile-invalidate-cache :and-return-value t)
    (expect (projectile-maybe-invalidate-cache nil) :not :to-be-truthy))
  (it "should invalidate cache if force is t"
    (spy-on 'projectile-invalidate-cache :and-return-value t)
    (expect (projectile-maybe-invalidate-cache t) :to-be-truthy))
  (it "should invalidate cache if dirconfig is newer than cache"
    (spy-on 'projectile-invalidate-cache :and-return-value t)
    (spy-on 'file-newer-than-file-p :and-return-value t)
    (expect (projectile-maybe-invalidate-cache nil) :to-be-truthy)))

(describe "projectile-root-top-down"
  (it "identifies the root directory of a project by top-down search"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/.git/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/src/framework.conf"
       "projectA/src/html/index.html")
      ;; .git is a directory, so it's not matched by top-down (file markers only);
      ;; framework.conf at projectA/src/ is the only match
      (expect (projectile-root-top-down "projectA/src/framework/lib" '("framework.conf" ".git"))
              :to-equal
              (expand-file-name "projectA/src/"))
      (expect (projectile-root-top-down "projectA/src/framework/lib" '(".git" "framework.conf"))
              :to-equal
              (expand-file-name "projectA/src/"))
      (expect (projectile-root-top-down "projectA/src/html/" '("index.html"))
              :to-equal
              (expand-file-name "projectA/src/html/")))))
  (it "returns the topmost match when file markers exist at multiple levels"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/Makefile"
       "project/subdir/Makefile"
       "project/subdir/file.txt")
      (expect (projectile-root-top-down "project/subdir" '("Makefile"))
              :to-equal
              (expand-file-name "project/")))))
  (it "does not match directories for file-type markers"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/workspace/"
       "projectA/src/")
      (expect (projectile-root-top-down "projectA/src/" '("workspace"))
              :not :to-be-truthy)))))

(describe "projectile-root-top-down-recurring"
  (it "identifies the root directory of a project by recurring top-down search"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/.git/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/src/framework/framework.conf"
       "projectA/src/html/index.html"
       ".projectile")
      (expect (projectile-root-top-down-recurring "projectA/src/html/" '("something" ".svn" ".git"))
              :to-equal
              (expand-file-name "projectA/"))
      (expect (projectile-root-top-down-recurring "projectA/src/html/" '(".git"))
              :to-equal
              (expand-file-name "projectA/"))
      (expect (projectile-root-top-down-recurring "projectA/src/html/" '("elusivefile"))
              :not :to-be-truthy)))))

(describe "projectile-root-bottom-up"
  (it "identifies the root directory of a project by bottom-up search"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.git/"
       "projectA/.projectile"
       "projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/framework/framework.conf"
       "projectA/src/framework/lib/"
       "projectA/src/html/"
       "projectA/src/html/.svn/"
       "projectA/src/html/index.html")
      (expect (projectile-root-bottom-up "projectA/src/framework/lib" '(".git" ".svn"))
              :to-equal
              (expand-file-name "projectA/src/"))
      (expect (projectile-root-bottom-up "projectA/src/framework/lib" '(".git"))
              :to-equal
              (expand-file-name "projectA/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '(".git" ".svn"))
              :to-equal
              (expand-file-name "projectA/src/html/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '(".svn" ".git"))
              :to-equal
              (expand-file-name "projectA/src/html/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '(".projectile" "index.html"))
              :to-equal
              (expand-file-name "projectA/src/html/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '("index.html" ".projectile"))
              :to-equal
              (expand-file-name "projectA/src/html/"))
      (expect (projectile-root-bottom-up "projectA/src/html" '(".projectile"))
              :to-equal
              (expand-file-name "projectA/")))))
  (it "matches a regular-file marker (e.g. git worktree's .git file)"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("worktree/.git"
       "worktree/src/file.txt")
      (expect (projectile-root-bottom-up "worktree/src/" '(".git"))
              :to-equal
              (expand-file-name "worktree/")))))
  (it "lets an outer VC root win over a nearer manifest by default"
    ;; The default bottom-up list is VCS markers only, so in a monorepo
    ;; layout (`.git' at the top, a language manifest deeper down) the
    ;; enclosing repository wins - the git repo is the project.
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("monorepo/.git/"
       "monorepo/clj/deps.edn"
       "monorepo/clj/src/foo.clj")
      (expect (projectile-root-bottom-up
               "monorepo/clj/src/"
               projectile-project-root-files-bottom-up)
              :to-equal
              (expand-file-name "monorepo/")))))
  (it "lets a .projectile-marked subproject win over an outer VC root"
    ;; The escape hatch for polyglot/monorepo layouts: drop a
    ;; `.projectile' in the subproject and `projectile-root-marked'
    ;; (which runs before the bottom-up search) anchors the root there.
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("monorepo/.git/"
       "monorepo/clj/.projectile"
       "monorepo/clj/src/foo.clj")
      (expect (projectile-root-marked "monorepo/clj/src/")
              :to-equal
              (expand-file-name "monorepo/clj/"))))))

(describe "projectile-root-marked"
  (it "finds the closest dirconfig-file bottom-up"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("alpha/.projectile"
       "alpha/sub/file.txt")
      (expect (projectile-root-marked "alpha/sub/")
              :to-equal
              (expand-file-name "alpha/")))))
  (it "honors a custom projectile-dirconfig-file"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("alpha/.myproject"
       "alpha/sub/file.txt")
      (let ((projectile-dirconfig-file ".myproject"))
        (expect (projectile-root-marked "alpha/sub/")
                :to-equal
                (expand-file-name "alpha/"))))))
  (it "returns nil when no marker is present"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("alpha/sub/file.txt")
      (expect (projectile-root-marked "alpha/sub/") :not :to-be-truthy)))))

(describe "projectile-root-local"
  (it "returns the buffer-local projectile-project-root variable"
    (let ((projectile-project-root "/some/override/"))
      (expect (projectile-root-local "/totally/unrelated/") :to-equal "/some/override/")))
  (it "returns nil when the buffer-local variable is unset"
    (let ((projectile-project-root nil))
      (expect (projectile-root-local "/anywhere/") :not :to-be-truthy))))

(describe "projectile-project-root"
  (defun projectile-test-should-root-in (root directory)
    (let ((projectile-project-root-cache (make-hash-table :test 'equal)))
      (expect (let ((default-directory
                             (expand-file-name
                              (file-name-as-directory directory))))
                       (file-truename (projectile-project-root)))
              :to-equal
              (file-truename (file-name-as-directory root)))))

  (it "returns the root directory of a project"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/build/framework/lib/"
       "projectA/requirements/a/b/c/d/e/f/g/"
       "projectA/src/framework/framework.conf"
       "projectA/requirements/a/b/c/requirements.txt"
       "projectA/src/html/index.html"
       "projectA/.projectile"
       "override")
      (let ((projectile-project-root-files-bottom-up '("somefile" ".projectile"))
            (projectile-project-root-files '("otherfile" "framework.conf" "requirements.txt"))
            (projectile-project-root-files-top-down-recurring '(".svn" ".foo"))
            (projectile-project-root-functions '(projectile-root-bottom-up
                                                 projectile-root-top-down
                                                 projectile-root-top-down-recurring)))
        (projectile-test-should-root-in "projectA" "projectA/requirements/a/b/c/d/e/f/g")
        (projectile-test-should-root-in "projectA" "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA" "projectA/src/html")

        (setq projectile-project-root-functions '(projectile-root-top-down
                                                  projectile-root-top-down-recurring
                                                  projectile-root-bottom-up))
        (projectile-test-should-root-in "projectA/requirements/a/b/c"
                                        "projectA/requirements/a/b/c/d/e/f/g")
        (projectile-test-should-root-in "projectA/src/framework"
                                        "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA/src"
                                        "projectA/src/html"))

      (let ((projectile-project-root-files-bottom-up '("somefile" ".projectile"))
            (projectile-project-root-files '("otherfile" "noframework.conf"))
            (projectile-project-root-files-top-down-recurring '(".svn" ".foo"))
            (projectile-project-root-functions '(projectile-root-top-down-recurring
                                                 projectile-root-bottom-up
                                                 projectile-root-top-down)))
        (projectile-test-should-root-in "projectA/src" "projectA/src/framework/lib")
        (projectile-test-should-root-in "projectA/src" "projectA/src/html")
        (projectile-test-should-root-in "projectA/" "projectA/build/framework/lib"))

      (let ((projectile-project-root-files-bottom-up '("somefile" "override"))
            (projectile-project-root-files '("otherfile" "anotherfile"))
            (projectile-project-root-files-top-down-recurring '("someotherfile" "yetanotherfile"))
            (projectile-project-root-functions '(projectile-root-bottom-up
                                                 projectile-root-top-down
                                                 projectile-root-top-down-recurring)))
        (projectile-test-should-root-in default-directory "projectA/src/framework/lib")
        (projectile-test-should-root-in default-directory "projectA/src/html"))

      (let ((projectile-project-root-files-bottom-up '("somecoolfile"))
            (projectile-project-root-files nil)
            (projectile-project-root-files-top-down-recurring '(".svn"))
            (projectile-project-root-functions '(projectile-root-bottom-up
                                                 projectile-root-top-down
                                                 projectile-root-top-down-recurring)))
        (projectile-test-should-root-in "projectA/src/" "projectA/src/")
        (projectile-test-should-root-in "projectA/src/" "projectA/src/html")))))

  (it "caches permanent failure to find a project root"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/src/")
      (let* ((projectile-project-root-functions '())
             (dir "projectA/src")
             (cache-key (cons 'none dir))
             (projectile-project-root-cache (make-hash-table :test 'equal)))
        (expect (gethash cache-key projectile-project-root-cache) :to-be nil)
        (expect (projectile-project-root dir) :to-be nil)
        ;; now that this has run once, the cache should be populated with 'none
        (expect (gethash cache-key projectile-project-root-cache) :to-be 'none)
        ;; but projectile-project-root should still return nil
        (expect (projectile-project-root dir) :to-be nil)))))

  (it "does not cache transitory failure to find a project root"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/src/")
      ;; hackish, but override file-remote-p for a moment, which is called in
      ;; projectile-project-root with 1 argument to test if the file is remote,
      ;; and 3 arguments to see if the file is connected.  We want to return t
      ;; when checking if remote, and nil when checking if connected.
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (&rest args) (eql 1 (length args)))))
        (let* ((projectile-project-root-functions '())
               (dir "projectA/src")
               (cache-key (cons 'none dir))
               (projectile-project-root-cache (make-hash-table :test 'equal)))
          (expect (gethash cache-key projectile-project-root-cache) :to-be nil)
          (expect (projectile-project-root dir) :to-be nil)
          ;; since the failure was transitory, there should be nothing cached
          (expect (gethash cache-key projectile-project-root-cache) :to-be nil)
          ;; and projectile-project-root should still return nil
          (expect (projectile-project-root dir) :to-be nil))))))

  (it "honors a buffer-local projectile-project-root after it changes (#1211)"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("alpha/.projectile"
       "beta/.projectile"
       "host/notes.org")
      (let* ((dir (expand-file-name "host/"))
             (alpha-root (file-truename (expand-file-name "alpha/")))
             (beta-root (file-truename (expand-file-name "beta/")))
             (default-directory dir)
             (projectile-project-root-cache (make-hash-table :test 'equal)))
        ;; First "buffer": file-local override pointing at alpha.
        (let ((projectile-project-root alpha-root))
          (expect (projectile-project-root) :to-equal alpha-root))
        ;; Second "buffer": same default-directory, different override.
        ;; The cache must not return the previous buffer's answer.
        (let ((projectile-project-root beta-root))
          (expect (projectile-project-root) :to-equal beta-root))))))

  (it "caches per-function nil results so earlier misses aren't re-walked (#1836)"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.git/"
       "projectA/src/")
      (let* ((calls 0)
             (always-nil (lambda (_dir) (cl-incf calls) nil))
             (projectile-project-root-functions
              (list always-nil 'projectile-root-bottom-up))
             (projectile-project-root-files-bottom-up '(".git"))
             (dir (expand-file-name "projectA/src/"))
             (projectile-project-root-cache (make-hash-table :test 'equal)))
        ;; first call: always-nil is invoked, then bottom-up wins
        (expect (file-truename (projectile-project-root dir))
                :to-equal (file-truename (expand-file-name "projectA/")))
        (expect calls :to-equal 1)
        ;; second call: the cached 'none for always-nil should be honoured
        (expect (file-truename (projectile-project-root dir))
                :to-equal (file-truename (expand-file-name "projectA/")))
        (expect calls :to-equal 1)
        ;; and the cache holds the 'none sentinel for the failing function
        (expect (gethash (cons always-nil dir) projectile-project-root-cache)
                :to-be 'none)))))

  (it "unwraps tramp-archive paths to the enclosing directory"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("hostproj/.git/"
       "hostproj/archive.zip")
      (defvar projectile-test--root-fn-arg)
      (let* ((archive (expand-file-name "hostproj/archive.zip"))
             (inside (concat archive "/inside/"))
             (projectile-test--root-fn-arg nil)
             (projectile-project-root-cache (make-hash-table :test 'equal))
             (projectile-project-root-functions
              (list (lambda (d) (setq projectile-test--root-fn-arg d) nil))))
        (cl-letf (((symbol-function 'tramp-archive-file-name-p)
                   (lambda (d) (string-prefix-p archive d)))
                  ((symbol-function 'tramp-archive-file-name-archive)
                   (lambda (_d) archive)))
          (projectile-project-root inside)
          ;; The root function should have been handed the archive's enclosing
          ;; directory, not a path inside the archive.
          (expect projectile-test--root-fn-arg
                  :to-equal
                  (file-truename (expand-file-name "hostproj/")))))))))

(describe "projectile-ensure-project"
  (it "returns DIR unchanged when non-nil"
    (expect (projectile-ensure-project "/already/found/") :to-equal "/already/found/"))
  (it "falls back to default-directory when require-project-root is nil"
    (let ((projectile-require-project-root nil)
          (default-directory "/here/"))
      (expect (projectile-ensure-project nil) :to-equal "/here/")))
  (it "signals an error when require-project-root is t"
    (let ((projectile-require-project-root t)
          (default-directory "/here/"))
      (expect (projectile-ensure-project nil) :to-throw 'error)))
  (it "prompts when require-project-root is 'prompt"
    (let ((projectile-require-project-root 'prompt))
      (spy-on 'projectile-completing-read :and-return-value "/picked/")
      (expect (projectile-ensure-project nil) :to-equal "/picked/")
      (expect 'projectile-completing-read :to-have-been-called))))

(describe "projectile-acquire-root"
  (it "returns the resolved root when one is found"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("alpha/.projectile"
       "alpha/src/file.txt")
      (let* ((expected (file-truename (expand-file-name "alpha/")))
             (dir (expand-file-name "alpha/src/"))
             (default-directory dir)
             (projectile-project-root-cache (make-hash-table :test 'equal)))
        (expect (file-truename (projectile-acquire-root)) :to-equal expected)))))
  (it "delegates to projectile-ensure-project when no root is found"
    (let ((projectile-require-project-root nil)
          (default-directory "/here/"))
      (spy-on 'projectile-project-root :and-return-value nil)
      (expect (projectile-acquire-root) :to-equal "/here/"))))

(describe "projectile-discard-root-cache"
  (it "empties the root cache without touching other caches"
    (let ((projectile-project-root-cache (make-hash-table :test 'equal))
          (projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-project-type-cache (make-hash-table :test 'equal))
          (projectile-verbose nil))
      (puthash (cons 'projectile-root-bottom-up "/x/") "/x/"
               projectile-project-root-cache)
      (puthash (cons 'none "/y/") 'none projectile-project-root-cache)
      (puthash "/x/" '("a" "b") projectile-projects-cache)
      (puthash "/x/" 'emacs-eldev projectile-project-type-cache)
      (projectile-discard-root-cache)
      (expect (hash-table-count projectile-project-root-cache) :to-equal 0)
      ;; Other caches must be untouched.
      (expect (gethash "/x/" projectile-projects-cache) :to-equal '("a" "b"))
      (expect (gethash "/x/" projectile-project-type-cache) :to-equal 'emacs-eldev))))

(describe "projectile-file-exists-p"
  (it "returns t if file exists"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/dirA/dirB/"
       "project/fileA")
      (let ((projectile-file-exists-local-cache-expire nil)
            (projectile-file-exists-remote-cache-expire nil))
        (expect (projectile-file-exists-p "project/fileA") :to-be-truthy)
        (expect (projectile-file-exists-p "project/dirA/dirB") :to-be-truthy)
        (expect (projectile-file-exists-p "project/dirA/fileB") :not :to-be-truthy)
        (with-temp-file "project/dirA/fileB")
        (expect (projectile-file-exists-p "project/dirA/fileB") :to-be-truthy)
        (expect (projectile-file-exists-p "project/nofile") :not :to-be-truthy)
        (delete-file "project/fileA")
        (expect (projectile-file-exists-p "project/fileA") :not :to-be-truthy)))))
  (it "caches the results"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("dirA/dirB/"
       "fileA")
      (let ((initial-time (current-time))
            (projectile-file-exists-local-cache-expire 100)
            (projectile-file-exists-remote-cache-expire nil))

        (spy-on 'run-with-timer :and-return-value 'nooptimer)
        (spy-on 'current-time :and-return-value initial-time)
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (expect (projectile-file-exists-p "dirA/dirB") :to-be-truthy)
        (expect (projectile-file-exists-p "dirA/fileB") :not :to-be-truthy)
        (with-temp-file "dirA/fileB")
        (expect (projectile-file-exists-p "dirA/fileB") :not :to-be-truthy)
        (delete-file "fileA")
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 50)))
        (projectile-file-exists-cache-cleanup)
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (expect (projectile-file-exists-p "dirA/fileB") :not :to-be-truthy)
        (expect (projectile-file-exists-p "fileC") :not :to-be-truthy)
        (with-temp-file "fileC")
        (expect (projectile-file-exists-p "fileA") :to-be-truthy)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 120)))
        (projectile-file-exists-cache-cleanup)
        (expect (projectile-file-exists-p "dirA/fileB") :to-be-truthy)
        (expect (projectile-file-exists-p "fileA") :not :to-be-truthy)
        (expect (projectile-file-exists-p "fileC") :not :to-be-truthy)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 220)))
        (projectile-file-exists-cache-cleanup)
        (expect (projectile-file-exists-p "fileC") :to-be-truthy)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)

        (spy-on 'current-time :and-return-value (time-add initial-time (seconds-to-time 1000)))
        (expect projectile-file-exists-cache-timer :to-equal 'nooptimer)
        (projectile-file-exists-cache-cleanup)
        (expect projectile-file-exists-cache-timer :not :to-be-truthy))))))

(describe "projectile-project-root caching"
  (it "caches the current file"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile"
       "project/file1.el"
       "project/file2.el"
       "project/file3.el"
       "project/file4.el")
      (cd "project")
      (let ((projectile-projects-cache (make-hash-table :test #'equal))
            (projectile-projects-cache-time (make-hash-table :test #'equal))
            (projectile-enable-caching 'persistent))
        (puthash (projectile-project-root)
                 '("file1.el")
                 projectile-projects-cache)
        (spy-on 'projectile-project-root :and-call-fake (lambda (&optional _dir) (file-truename default-directory)))
        (spy-on 'projectile-project-vcs :and-return-value 'none)
        (with-current-buffer (find-file-noselect  "file2.el" t)
          (projectile-cache-current-file)
          (dolist (f '("file1.el" "file2.el"))
            (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy)))
        (with-current-buffer (find-file-noselect "file3.el" t)
          (projectile-cache-current-file)
          (dolist (f '("file1.el" "file2.el" "file3.el"))
            (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy)))
        (with-current-buffer (find-file-noselect "file4.el" t)
          (projectile-cache-current-file)
          (dolist (f '("file1.el" "file2.el" "file3.el" "file4.el"))
            (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy)))))))
  (it "ensures that we update the cache if it's expired"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile"
       "project/file1.el"
       "project/file2.el")
      (cd "project")
      (let ((projectile-projects-cache (make-hash-table :test #'equal))
            (projectile-projects-cache-time (make-hash-table :test #'equal))
            (projectile-enable-caching 'persistent)
            (projectile-files-cache-expire 10))
        ;; Create a stale cache with only one file in it.
        (puthash (projectile-project-root)
                 '("file1.el")
                 projectile-projects-cache)
        (puthash (projectile-project-root)
                 0 ;; Cached 1st of January 1970.
                 projectile-projects-cache-time)

        (spy-on 'projectile-acquire-root :and-call-fake (lambda () (file-truename default-directory)))
        (spy-on 'projectile-project-vcs :and-return-value 'none)
        ;; After listing all the files, the cache should have been updated.
        (projectile-current-project-files)
        ;; find returns the leading ./ therefore the somewhat odd notation here
        (dolist (f '("file1.el" "file2.el"))
          (expect (member f (gethash (projectile-project-root) projectile-projects-cache)) :to-be-truthy))))))
  (it "ensures that we don't cache a project root if the path has changed."
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile")
      (cd "project")
      (let ((projectile-project-root-cache (make-hash-table :test #'equal))
            (correct-project-root (projectile-project-root)))
        ;; If this project has been moved, then we will have stale
        ;; paths in the cache.
        (puthash
         (format "projectile-root-bottom-up-%s" correct-project-root)
         "/this/path/does/not/exist"
         projectile-project-root-cache)
        (expect (projectile-project-root) :to-equal correct-project-root))))))

;;; projectile-root-test.el ends here
