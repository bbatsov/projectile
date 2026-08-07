;;; projectile-repo-identity-test.el --- Tests for repository identity -*- lexical-binding: t -*-

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

;; Tests for `projectile-repo-identity' - the notion that tells two
;; checkouts of one repository apart from two unrelated projects.

;;; Code:

(require 'projectile-test-helpers)

;;; Remote URL normalization

(describe "projectile--normalize-repo-path"
  (it "strips trailing slashes"
    (expect (projectile--normalize-repo-path "owner/repo/") :to-equal "owner/repo")
    (expect (projectile--normalize-repo-path "owner/repo///") :to-equal "owner/repo"))

  (it "strips a .git suffix"
    (expect (projectile--normalize-repo-path "owner/repo.git") :to-equal "owner/repo"))

  (it "strips a .git suffix hidden behind a trailing slash"
    (expect (projectile--normalize-repo-path "owner/repo.git/") :to-equal "owner/repo"))

  (it "leaves a plain path alone"
    (expect (projectile--normalize-repo-path "owner/repo") :to-equal "owner/repo")))

(describe "projectile--normalize-repo-url"
  (it "collapses every spelling of one remote to the same identity"
    (let ((expected "github.com/bbatsov/projectile"))
      (dolist (url '("git@github.com:bbatsov/projectile.git"
                     "git@github.com:bbatsov/projectile"
                     "https://github.com/bbatsov/projectile.git"
                     "https://github.com/bbatsov/projectile"
                     "https://github.com/bbatsov/projectile/"
                     "https://user@github.com/bbatsov/projectile.git"
                     "ssh://git@github.com/bbatsov/projectile.git"
                     "ssh://git@github.com:22/bbatsov/projectile.git"))
        (expect (projectile--normalize-repo-url url) :to-equal expected))))

  (it "keeps distinct repositories distinct"
    (expect (projectile--normalize-repo-url "git@github.com:bbatsov/projectile.git")
            :not :to-equal
            (projectile--normalize-repo-url "git@github.com:bbatsov/crux.git"))
    (expect (projectile--normalize-repo-url "git@github.com:bbatsov/projectile.git")
            :not :to-equal
            (projectile--normalize-repo-url "git@gitlab.com:bbatsov/projectile.git")))

  (it "downcases the identity so case differences still match"
    (expect (projectile--normalize-repo-url "git@GitHub.com:BBatsov/Projectile.git")
            :to-equal "github.com/bbatsov/projectile"))

  (it "handles subgroup paths"
    (expect (projectile--normalize-repo-url "git@gitlab.com:group/sub/proj.git")
            :to-equal "gitlab.com/group/sub/proj"))

  (it "reads a file:// URL as the local path it is"
    (expect (projectile--normalize-repo-url "file:///srv/git/repo.git")
            :to-equal "/srv/git/repo"))

  (it "reads a bare path as a local repository"
    (expect (projectile--normalize-repo-url "/srv/git/repo.git")
            :to-equal "/srv/git/repo"))

  (it "does not mistake a Windows drive letter for a host"
    ;; A single-letter "host" is a drive, so this has to come back as a path
    ;; rather than as `c/src/repo'.
    (expect (projectile--normalize-repo-url "c:/src/repo.git")
            :not :to-equal "c/src/repo"))

  (it "returns nil for no URL at all"
    (expect (projectile--normalize-repo-url nil) :to-be nil)
    (expect (projectile--normalize-repo-url "") :to-be nil)
    (expect (projectile--normalize-repo-url "   ") :to-be nil)))


;;; Reading git's own files

(describe "projectile--git-dir"
  (it "returns the .git directory of an ordinary checkout"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (expect (file-truename (projectile--git-dir repo))
               :to-equal (file-name-as-directory
                          (file-truename (expand-file-name ".git" repo)))))))

  (it "follows the gitdir: file a linked worktree gets instead"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (worktree (projectile-test-add-git-worktree
                       repo (expand-file-name "feature") "feature")))
       ;; A linked worktree's `.git' is a file, not a directory.
       (expect (file-directory-p (expand-file-name ".git" worktree)) :to-be nil)
       (expect (projectile--git-dir worktree) :to-be-truthy)
       (expect (file-truename (projectile--git-dir worktree))
               :to-match "worktrees/feature/\\'"))))

  (it "returns nil for a directory that holds no .git at all"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (make-directory (expand-file-name "sub" repo) t)
       (expect (projectile--git-dir
                (file-name-as-directory (expand-file-name "sub" repo)))
               :to-be nil)))))

(describe "projectile--git-common-dir"
  (it "resolves a linked worktree's git dir to the shared one"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (worktree (projectile-test-add-git-worktree
                       repo (expand-file-name "feature") "feature")))
       (expect (file-truename
                (projectile--git-common-dir (projectile--git-dir worktree)))
               :to-equal
               (file-truename
                (projectile--git-common-dir (projectile--git-dir repo)))))))

  (it "leaves the main checkout's git dir alone"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (git-dir (projectile--git-dir repo)))
       (expect (projectile--git-common-dir git-dir) :to-equal git-dir)))))

(describe "projectile--git-config-remote-url"
  (it "prefers origin over any other remote"
    (projectile-test-with-temp-files ((config))
      (with-temp-file config
        (insert "[core]\n\trepositoryformatversion = 0\n"
                "[remote \"upstream\"]\n\turl = git@github.com:up/stream.git\n"
                "\tfetch = +refs/heads/*:refs/remotes/upstream/*\n"
                "[remote \"origin\"]\n\turl = git@github.com:me/mine.git\n"))
      (expect (projectile--git-config-remote-url config)
              :to-equal "git@github.com:me/mine.git")))

  (it "falls back to the first remote when there's no origin"
    (projectile-test-with-temp-files ((config))
      (with-temp-file config
        (insert "[remote \"upstream\"]\n\turl = git@github.com:up/stream.git\n"
                "[remote \"fork\"]\n\turl = git@github.com:me/fork.git\n"))
      (expect (projectile--git-config-remote-url config)
              :to-equal "git@github.com:up/stream.git")))

  (it "returns nil when no remote is configured"
    (projectile-test-with-temp-files ((config))
      (with-temp-file config (insert "[core]\n\tbare = false\n"))
      (expect (projectile--git-config-remote-url config) :to-be nil)))

  (it "returns nil for a file that isn't there"
    (expect (projectile--git-config-remote-url "/nope/config") :to-be nil)))

(describe "projectile--git-head-branch"
  (it "reads the branch out of HEAD"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (expect (projectile--git-head-branch
                (projectile--git-dir
                 (file-name-as-directory (expand-file-name "feature"))))
               :to-equal "feature"))))

  (it "returns nil for a detached HEAD"
    (projectile-test-with-temp-files ((git-dir :dir))
      (with-temp-file (expand-file-name "HEAD" git-dir)
        (insert "9cd1a2b0e5f3d4c6a7b8e9f0a1b2c3d4e5f6a7b8\n"))
      (expect (projectile--git-head-branch git-dir) :to-be nil))))


;;; Reading Jujutsu's own files

(describe "projectile--jj-repo-dir"
  (it "returns the repo directory of the first workspace"
    (assume (executable-find "jj") "jj is not available")
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-jj-repo "repo")))
       (expect (file-truename (projectile--jj-repo-dir repo))
               :to-equal (file-name-as-directory
                          (file-truename (expand-file-name ".jj/repo" repo)))))))

  (it "follows the file a workspace added later gets instead"
    (assume (executable-find "jj") "jj is not available")
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-jj-repo "repo"))
            (second (projectile-test-add-jj-workspace
                     repo (expand-file-name "second"))))
       ;; The added workspace's `.jj/repo' is a file, not a directory.
       (expect (file-directory-p (expand-file-name ".jj/repo" second)) :to-be nil)
       (expect (file-truename (projectile--jj-repo-dir second))
               :to-equal (file-truename (projectile--jj-repo-dir repo))))))

  (it "returns nil for a directory that is not a workspace"
    (expect (projectile--jj-repo-dir "/src/plain/") :to-be nil)))

(describe "projectile-repo-identity for Jujutsu"
  (it "agrees with the git checkout backing the same repository"
    ;; `jj git init' keeps the commits in a git directory, and the first
    ;; workspace is reported as git while a workspace added later is
    ;; reported as jj.  They have to come out as the same repository
    ;; regardless, or the known projects can't relate them.
    (assume (executable-find "jj") "jj is not available")
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-jj-repo "repo"))
            (second (projectile-test-add-jj-workspace
                     repo (expand-file-name "second"))))
       (expect (plist-get (projectile-repo-identity second) :repo)
               :to-equal (plist-get (projectile-repo-identity repo) :repo))
       (expect (projectile-same-repo-p (projectile-repo-identity repo)
                                       (projectile-repo-identity second))
               :to-be-truthy))))

  (it "agrees with a colocated checkout whose git dir is named by a file"
    ;; A submodule's `.git' is a file pointing at the real git directory
    ;; under the superproject, and that is what jj records as its backing
    ;; store.  Taking it for a directory used to give the workspace and the
    ;; checkout two different answers about which repository they are.
    (assume (executable-find "jj") "jj is not available")
    (projectile-test-with-sandbox
     (projectile-test-init-git-repo "lib")
     (let ((super (projectile-test-init-git-repo "super")))
       (let ((default-directory super))
         (projectile-test-git "-c" "protocol.file.allow=always"
                              "submodule" "add" "-q" "../lib" "vendor/lib")
         (projectile-test-git "commit" "-qm" "add submodule"))
       (let* ((sub (file-name-as-directory
                    (expand-file-name "vendor/lib" super)))
              (default-directory sub))
         (expect (file-directory-p (expand-file-name ".git" sub)) :to-be nil)
         (projectile-test-jj "git" "init" "--colocate")
         (let ((workspace (projectile-test-add-jj-workspace
                           sub (expand-file-name "lib-ws"))))
           (expect (plist-get (projectile-repo-identity workspace) :repo)
                   :to-equal
                   (plist-get (projectile-repo-identity sub) :repo)))))))

  (it "picks up the remote through the git backing store"
    (assume (executable-find "jj") "jj is not available")
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-jj-repo "repo")))
       (let ((default-directory repo))
         (projectile-test-jj "git" "remote" "add" "origin"
                             "git@github.com:bbatsov/projectile.git"))
       (expect (plist-get (projectile-repo-identity repo) :remote)
               :to-equal "github.com/bbatsov/projectile")))))


;;; Repository identity

(describe "projectile-repo-identity"
  (it "reports the common dir and the normalized remote of a git project"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo
                   "repo" "git@github.com:bbatsov/projectile.git"))
            (identity (projectile-repo-identity repo)))
       (expect (plist-get identity :repo)
               :to-equal (file-name-as-directory
                          (file-truename (expand-file-name ".git" repo))))
       (expect (plist-get identity :remote)
               :to-equal "github.com/bbatsov/projectile"))))

  (it "leaves the remote nil when the repository has none"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (identity (projectile-repo-identity repo)))
       (expect (plist-get identity :remote) :to-be nil)
       (expect (plist-get identity :repo) :to-be-truthy))))

  (it "falls back to the first remote when there's no origin"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (let ((default-directory repo))
         (projectile-test-git
          "remote" "add" "upstream" "git@github.com:bbatsov/crux.git"))
       (expect (plist-get (projectile-repo-identity repo) :remote)
               :to-equal "github.com/bbatsov/crux"))))

  (it "gives a worktree the same repo as the checkout it was linked from"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-test-add-git-worktree
        repo (expand-file-name "feature") "feature")
       (expect (plist-get (projectile-repo-identity repo) :repo)
               :to-equal
               (plist-get (projectile-repo-identity
                           (file-name-as-directory (expand-file-name "feature")))
                          :repo)))))

  (it "gives two clones of one upstream the same remote but different repos"
    (projectile-test-with-sandbox
     (let* ((remote "git@github.com:bbatsov/projectile.git")
            (one (projectile-test-init-git-repo "one" remote))
            (two (projectile-test-init-git-repo "two" remote)))
       (expect (plist-get (projectile-repo-identity one) :remote)
               :to-equal (plist-get (projectile-repo-identity two) :remote))
       (expect (plist-get (projectile-repo-identity one) :repo)
               :not :to-equal (plist-get (projectile-repo-identity two) :repo)))))

  (it "returns nil for a project that isn't under version control"
    (spy-on 'projectile-project-vcs :and-return-value 'none)
    (expect (projectile-repo-identity "/src/plain/") :to-be nil))

  (it "returns nil for a project sitting below a repository's root"
    ;; `projectile-project-vcs' answers `git' here - the enclosing repository
    ;; is found by walking up - but a directory inside a checkout is not a
    ;; checkout of its own, so it has no identity and no worktrees.
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (make-directory (expand-file-name "sub" repo) t)
       (expect (projectile-repo-identity
                (file-name-as-directory (expand-file-name "sub" repo)))
               :to-be nil))))

  (it "returns nil for a remote project rather than reaching over TRAMP"
    (spy-on 'projectile-project-vcs)
    (expect (projectile-repo-identity "/ssh:host:/src/repo/") :to-be nil)
    (expect 'projectile-project-vcs :not :to-have-been-called))

  (it "caches its answer per root"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (projectile-repo-identity repo)
       (spy-on 'projectile-project-vcs)
       (projectile-repo-identity repo)
       (expect 'projectile-project-vcs :not :to-have-been-called)))))

(describe "projectile-same-repo-p"
  (it "matches identities sharing a repo directory"
    (expect (projectile-same-repo-p '(:repo "/src/.git" :remote nil)
                                    '(:repo "/src/.git" :remote nil))
            :to-be-truthy))

  (it "matches identities sharing a remote"
    (expect (projectile-same-repo-p '(:repo "/one/.git" :remote "host/o/r")
                                    '(:repo "/two/.git" :remote "host/o/r"))
            :to-be-truthy))

  (it "does not match unrelated identities"
    (expect (projectile-same-repo-p '(:repo "/one/.git" :remote "host/o/one")
                                    '(:repo "/two/.git" :remote "host/o/two"))
            :to-be nil))

  (it "does not treat two unknown identities as the same"
    (expect (projectile-same-repo-p '(:repo nil :remote nil)
                                    '(:repo nil :remote nil))
            :to-be nil))

  (it "does not match when either identity is missing"
    (expect (projectile-same-repo-p nil '(:repo "/src/.git")) :to-be nil)
    (expect (projectile-same-repo-p '(:repo "/src/.git") nil) :to-be nil)))


(provide (quote projectile-repo-identity-test))

;;; projectile-repo-identity-test.el ends here
