;;; projectile-worktree-test.el --- Tests for repository identity and worktrees -*- lexical-binding: t -*-

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

;; Tests for `projectile-switch-worktree' and the functions that find the
;; other checkouts of a project's repository.

;;; Code:

(require 'projectile-test-helpers)

;;; Parsing git's worktree listing

(describe "projectile--parse-git-worktree-list"
  (it "parses the path and branch of each worktree"
    (let ((worktrees (projectile--parse-git-worktree-list
                      (concat "worktree /src/main\nHEAD abc\nbranch refs/heads/master\n\n"
                              "worktree /src/feature\nHEAD abc\nbranch refs/heads/feature\n\n"))))
      (expect (length worktrees) :to-equal 2)
      (expect (plist-get (nth 0 worktrees) :path) :to-equal "/src/main/")
      (expect (plist-get (nth 0 worktrees) :branch) :to-equal "master")
      (expect (plist-get (nth 1 worktrees) :path) :to-equal "/src/feature/")
      (expect (plist-get (nth 1 worktrees) :branch) :to-equal "feature")))

  (it "leaves a detached worktree without a branch"
    (let ((worktrees (projectile--parse-git-worktree-list
                      "worktree /src/detached\nHEAD abc\ndetached\n\n")))
      (expect (length worktrees) :to-equal 1)
      (expect (plist-get (car worktrees) :branch) :to-be nil)))

  (it "skips a bare repository, which has no working tree to switch to"
    (let ((worktrees (projectile--parse-git-worktree-list
                      (concat "worktree /src/bare.git\nbare\n\n"
                              "worktree /src/feature\nHEAD abc\nbranch refs/heads/feature\n\n"))))
      (expect (length worktrees) :to-equal 1)
      (expect (plist-get (car worktrees) :path) :to-equal "/src/feature/")))

  (it "flags a prunable worktree"
    (let ((worktrees (projectile--parse-git-worktree-list
                      "worktree /src/gone\nHEAD abc\nbranch refs/heads/gone\nprunable gitdir file points to non-existent location\n\n")))
      (expect (plist-get (car worktrees) :prunable) :to-be-truthy)))

  (it "is not confused by a locked worktree"
    (let ((worktrees (projectile--parse-git-worktree-list
                      "worktree /src/locked\nHEAD abc\nbranch refs/heads/locked\nlocked\n\n")))
      (expect (length worktrees) :to-equal 1)
      (expect (plist-get (car worktrees) :branch) :to-equal "locked")))

  (it "parses a final record that isn't followed by a blank line"
    (let ((worktrees (projectile--parse-git-worktree-list
                      "worktree /src/main\nHEAD abc\nbranch refs/heads/master")))
      (expect (length worktrees) :to-equal 1)
      (expect (plist-get (car worktrees) :branch) :to-equal "master")))

  (it "returns nothing for empty output"
    (expect (projectile--parse-git-worktree-list "") :to-be nil)))


;;; Finding worktrees

(describe "projectile-worktrees-from-git"
  (it "lists the main checkout and its linked worktrees"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (let ((paths (mapcar (lambda (w) (file-truename (plist-get w :path)))
                            (projectile-worktrees-from-git repo))))
         (expect paths :to-have-same-items-as
                 (list (file-truename repo)
                       (file-truename (file-name-as-directory
                                       (expand-file-name "feature")))))))))

  (it "reports the branch each worktree has checked out"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (let* ((worktrees (projectile-worktrees-from-git repo))
              (feature (seq-find (lambda (w)
                                   (string-match-p "feature" (plist-get w :path)))
                                 worktrees)))
         (expect (plist-get feature :branch) :to-equal "feature")))))

  (it "returns nothing for a project that isn't under git"
    (spy-on 'projectile-project-vcs :and-return-value 'hg)
    (expect (projectile-worktrees-from-git "/src/repo/") :to-be nil)))

(describe "projectile-worktrees-from-known-projects"
  (it "finds another clone of the same upstream"
    (projectile-test-with-sandbox
     (let* ((remote "git@github.com:bbatsov/projectile.git")
            (one (projectile-test-init-git-repo "one" remote))
            (two (projectile-test-init-git-repo "two" remote))
            (projectile-known-projects (list one two)))
       (expect (mapcar (lambda (w) (file-truename (plist-get w :path)))
                       (projectile-worktrees-from-known-projects one))
               :to-have-same-items-as
               (list (file-truename one) (file-truename two))))))

  (it "reports the branch a sibling clone has checked out"
    (projectile-test-with-sandbox
     (let* ((remote "git@github.com:bbatsov/projectile.git")
            (one (projectile-test-init-git-repo "one" remote))
            (two (projectile-test-init-git-repo "two" remote))
            (projectile-known-projects (list one two)))
       (let ((default-directory two))
         (projectile-test-git "checkout" "-q" "-b" "topic"))
       (let ((found (seq-find (lambda (w)
                                (string-match-p "two" (plist-get w :path)))
                              (projectile-worktrees-from-known-projects one))))
         (expect (plist-get found :branch) :to-equal "topic")))))

  (it "does not launch a git process per known project"
    ;; The scan runs over every known project, so it reads git's own files
    ;; rather than shelling out - see `projectile--git-dir'.
    (projectile-test-with-sandbox
     (let* ((remote "git@github.com:bbatsov/projectile.git")
            (one (projectile-test-init-git-repo "one" remote))
            (two (projectile-test-init-git-repo "two" remote))
            (projectile-known-projects (list one two)))
       (spy-on 'projectile--git)
       (expect (length (projectile-worktrees-from-known-projects one))
               :to-equal 2)
       (expect 'projectile--git :not :to-have-been-called))))

  (it "leaves out a project that is a different repository"
    (projectile-test-with-sandbox
     (let* ((one (projectile-test-init-git-repo
                  "one" "git@github.com:bbatsov/projectile.git"))
            (other (projectile-test-init-git-repo
                    "other" "git@github.com:bbatsov/crux.git"))
            (projectile-known-projects (list one other)))
       (expect (mapcar (lambda (w) (file-truename (plist-get w :path)))
                       (projectile-worktrees-from-known-projects one))
               :to-equal (list (file-truename one))))))

  (it "returns nothing when the project has no identity to match on"
    (spy-on 'projectile-repo-identity)
    (expect (projectile-worktrees-from-known-projects "/src/plain/") :to-be nil)))

(describe "projectile-project-worktrees"
  (it "lists a worktree once even when several functions report it"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (let* ((feature (file-name-as-directory (expand-file-name "feature")))
              (projectile-known-projects (list repo feature))
              (worktrees (projectile-project-worktrees repo)))
         (expect (length worktrees) :to-equal 2)))))

  (it "keeps the branch git knew about when merging duplicate reports"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (let* ((feature (file-name-as-directory (expand-file-name "feature")))
              (projectile-known-projects (list repo feature))
              (found (seq-find (lambda (w)
                                 (string-match-p "feature" (plist-get w :path)))
                               (projectile-project-worktrees repo))))
         (expect (plist-get found :branch) :to-equal "feature")))))

  (it "survives a worktree function that throws"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (projectile-worktree-functions
             (list (lambda (_root) (error "boom"))
                   (lambda (_root) (list (list :path "/src/other/"))))))
       (expect (mapcar (lambda (w) (plist-get w :path))
                       (projectile-project-worktrees repo))
               :to-equal '("/src/other/"))))))


;;; Switching

(describe "projectile-switch-worktree"
  (it "errors when the project has no other checkout"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (projectile-known-projects (list repo)))
       (spy-on 'projectile-acquire-root :and-return-value repo)
       (expect (projectile-switch-worktree) :to-throw 'user-error))))

  (it "switches to the chosen worktree"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (let ((projectile-known-projects (list repo)))
         (spy-on 'projectile-acquire-root :and-return-value repo)
         (spy-on 'projectile-switch-project-by-name)
         (spy-on 'projectile-completing-read :and-call-fake
                 (lambda (_prompt choices &rest args)
                   (funcall (plist-get args :action) (car choices))))
         (projectile-switch-worktree)
         (expect 'projectile-switch-project-by-name :to-have-been-called)
         (expect (file-name-as-directory
                  (file-truename
                   (car (spy-calls-args-for 'projectile-switch-project-by-name 0))))
                 :to-equal
                 (file-name-as-directory
                  (file-truename (expand-file-name "feature"))))))))

  (it "does not offer the checkout we're already in"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (let ((projectile-known-projects (list repo))
             (offered nil))
         (spy-on 'projectile-acquire-root :and-return-value repo)
         (spy-on 'projectile-completing-read :and-call-fake
                 (lambda (_prompt choices &rest _args)
                   (setq offered choices)
                   nil))
         (projectile-switch-worktree)
         (expect (mapcar #'file-truename offered)
                 :to-equal
                 (list (file-truename (file-name-as-directory
                                       (expand-file-name "feature")))))))))

  (it "does not offer a worktree that is no longer on disk"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (projectile-worktree-functions
             (list (lambda (_root)
                     (list (list :path "/src/gone/" :prunable t)
                           (list :path "/src/here/" :branch "here"))))))
       (spy-on 'projectile-acquire-root :and-return-value repo)
       (spy-on 'projectile-completing-read :and-call-fake
               (lambda (_prompt choices &rest _args) (car choices)))
       (projectile-switch-worktree)
       (expect (spy-calls-args-for 'projectile-completing-read 0)
               :to-contain '("/src/here/")))))

  (it "annotates candidates with their branch"
    (expect (projectile--worktree-annotation '(:path "/src/x/" :branch "feature"))
            :to-equal " (feature)")
    (expect (projectile--worktree-annotation '(:path "/src/x/")) :to-be nil)))

(provide 'projectile-worktree-test)

;;; projectile-worktree-test.el ends here
