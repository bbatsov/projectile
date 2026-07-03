;;; projectile-indexing-test.el --- Tests for project file indexing and listing -*- lexical-binding: t -*-

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

;; Tests for project file indexing and listing.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-get-project-directories"
  (it "gets the list of project directories"
    (spy-on 'projectile-project-root :and-return-value "/my/root/")
    (spy-on 'projectile-parse-dirconfig-file :and-return-value nil)
    (expect (projectile-get-project-directories "/my/root") :to-equal '("/my/root")))
  (it "gets the list of project directories with dirs to keep"
    (spy-on 'projectile-project-root :and-return-value "/my/root/")
    (spy-on 'projectile-parse-dirconfig-file
            :and-return-value (make-projectile-dirconfig :keep '("foo" "bar/baz")))
    (expect (projectile-get-project-directories "/my/root/") :to-equal '("/my/root/foo" "/my/root/bar/baz"))))

(describe "projectile-dir-files"
  (it "fails unless directory exists"
    (spy-on 'file-directory-p :and-call-fake
            (lambda (filename) (equal filename "/my/root/")))
    (expect (projectile-dir-files "asdf") :to-throw))
  (it "lists the files in directory and sub-directories"
    (spy-on 'file-directory-p :and-call-fake
            (lambda (filename) (equal filename "/my/root/")))
    (spy-on 'projectile-patterns-to-ignore)
    (spy-on 'projectile-index-directory :and-call-fake (lambda (dir patterns progress-reporter)
                                                         (expect dir :to-equal "/my/root/")
                                                         '("/my/root/a/b/c" "/my/root/a/d/e")))
    (spy-on 'projectile-dir-files-alien :and-return-value '("a/b/c" "a/d/e"))
    (spy-on 'cd)
    (let ((projectile-indexing-method 'native))
      (expect (projectile-dir-files "/my/root/") :to-equal '("a/b/c" "a/d/e")))
    (let ((projectile-indexing-method 'hybrid))
      (expect (projectile-dir-files "/my/root/") :to-equal '("a/b/c" "a/d/e")))))

(describe "projectile-dir-files-alien"
  (it "excludes deleted-but-unstaged files when not using fd"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/existing.txt")
      (let ((default-directory (file-truename (expand-file-name "project/")))
            (projectile-git-use-fd nil)
            (projectile-fd-executable nil))
        ;; Initialize a real git repo, commit a file, then delete it without staging
        (call-process "git" nil nil nil "init")
        (call-process "git" nil nil nil "config" "user.email" "test@test.com")
        (call-process "git" nil nil nil "config" "user.name" "Test")
        (call-process "git" nil nil nil "add" "existing.txt")
        (write-region "content" nil "deleted.txt")
        (call-process "git" nil nil nil "add" "deleted.txt")
        (call-process "git" nil nil nil "commit" "-m" "init")
        (delete-file "deleted.txt")
        (let ((files (projectile-dir-files-alien default-directory)))
          (expect files :to-contain "existing.txt")
          (expect files :not :to-contain "deleted.txt"))))))
  (it "uses the VCS argument when supplied without recomputing it"
    (spy-on 'projectile-project-vcs)
    (spy-on 'projectile-files-via-ext-command :and-return-value '("a"))
    (spy-on 'projectile-get-sub-projects-files :and-return-value nil)
    (spy-on 'projectile-git-deleted-files :and-return-value nil)
    (let ((projectile-git-use-fd nil)
          (projectile-fd-executable nil))
      (projectile-dir-files-alien "/my/root/" 'git))
    (expect 'projectile-project-vcs :not :to-have-been-called))
  (it "uses the fd-based command when fd is configured for git"
    (spy-on 'projectile-files-via-ext-command :and-return-value '("a"))
    (spy-on 'projectile-get-sub-projects-files :and-return-value nil)
    (spy-on 'projectile-git-deleted-files :and-return-value nil)
    (let ((projectile-git-use-fd t)
          (projectile-fd-executable "fd"))
      (projectile-dir-files-alien "/my/root/" 'git)
      ;; When fd is on we don't ask git for deleted files.
      (expect 'projectile-git-deleted-files :not :to-have-been-called)
      (let ((cmd (cadr (spy-calls-args-for 'projectile-files-via-ext-command 0))))
        (expect cmd :to-equal
                (concat "fd " projectile-git-fd-args)))))
  (it "falls back to the generic command for projects without a VCS"
    (spy-on 'projectile-files-via-ext-command :and-return-value '("a.txt"))
    (let ((files (projectile-dir-files-alien "/my/root/" 'none)))
      (expect files :to-equal '("a.txt"))
      (expect (cadr (spy-calls-args-for 'projectile-files-via-ext-command 0))
              :to-equal projectile-generic-command))))

(describe "hybrid indexing"
  (it "applies projectile-globally-ignored-file-suffixes on top of the alien result"
    (spy-on 'projectile-files-via-ext-command :and-return-value
            '("foo.el" "build/foo.elc" "README"))
    (spy-on 'projectile-get-sub-projects-files :and-return-value nil)
    (spy-on 'projectile-git-deleted-files :and-return-value nil)
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (spy-on 'projectile-project-root :and-return-value "/my/root/")
    (spy-on 'file-directory-p :and-call-fake
            (lambda (filename) (equal filename "/my/root/")))
    (spy-on 'projectile-parse-dirconfig-file :and-return-value nil)
    (let ((projectile-indexing-method 'hybrid)
          (projectile-enable-caching nil)
          (projectile-globally-ignored-file-suffixes '(".elc"))
          (projectile-globally-ignored-files nil)
          (projectile-globally-ignored-directories nil)
          (projectile-globally-unignored-files nil)
          (projectile-globally-unignored-directories nil)
          (projectile-git-use-fd nil)
          (projectile-fd-executable nil))
      (let ((files (projectile-dir-files "/my/root/")))
        (expect files :to-contain "foo.el")
        (expect files :to-contain "README")
        (expect files :not :to-contain "build/foo.elc"))))
  (it "honors dirconfig ignore patterns on top of the alien result"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/keep.txt"
       "project/drop.txt"
       "project/.projectile")
      (let ((root (file-truename (expand-file-name "project/"))))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-/drop.txt\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (spy-on 'projectile-project-vcs :and-return-value 'git)
        (spy-on 'projectile-files-via-ext-command :and-return-value
                '("keep.txt" "drop.txt"))
        (spy-on 'projectile-get-sub-projects-files :and-return-value nil)
        (spy-on 'projectile-git-deleted-files :and-return-value nil)
        (let ((projectile-indexing-method 'hybrid)
              (projectile-enable-caching nil)
              (projectile-git-use-fd nil)
              (projectile-fd-executable nil))
          (let ((files (projectile-dir-files root)))
            (expect files :to-contain "keep.txt")
            (expect files :not :to-contain "drop.txt")))))))
  (it "passes the resolved VCS to projectile-dir-files-alien"
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (spy-on 'projectile-dir-files-alien :and-return-value '("a"))
    (spy-on 'projectile-adjust-files :and-call-fake (lambda (_p _v files) files))
    (spy-on 'file-directory-p :and-call-fake
            (lambda (filename) (equal filename "/my/root/")))
    (let ((projectile-indexing-method 'hybrid)
          (projectile-enable-caching nil))
      (projectile-dir-files "/my/root/"))
    ;; vcs is resolved once by the dispatcher and threaded through; the
    ;; redundant call inside projectile-dir-files-alien is gone.
    (expect 'projectile-project-vcs :to-have-been-called-times 1)
    (expect 'projectile-dir-files-alien
            :to-have-been-called-with "/my/root/" 'git)))

(describe "hybrid indexing with `+' keep entries"
  (it "batches dirconfig keep dirs into a single external command"
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (spy-on 'projectile-files-via-ext-command :and-return-value
            '("src/a.el" "test/b.el"))
    (spy-on 'projectile-get-sub-projects-files :and-return-value nil)
    (spy-on 'projectile-git-deleted-files :and-return-value nil)
    (spy-on 'projectile-parse-dirconfig-file :and-return-value
            (make-projectile-dirconfig :keep '("src/" "test/")))
    (spy-on 'projectile-project-root :and-return-value "/my/root/")
    (let ((projectile-indexing-method 'hybrid)
          (projectile-enable-caching nil)
          (projectile-files-cache-expire nil)
          (projectile-git-use-fd nil)
          (projectile-fd-executable nil)
          (projectile-globally-ignored-files nil)
          (projectile-globally-ignored-directories nil)
          (projectile-globally-ignored-file-suffixes nil)
          (projectile-globally-unignored-files nil)
          (projectile-globally-unignored-directories nil))
      (projectile-project-files "/my/root/")
      ;; The external command is invoked exactly once - not once per
      ;; kept subdirectory - and receives the kept paths as pathspecs.
      (expect 'projectile-files-via-ext-command :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'projectile-files-via-ext-command 0)
              :to-equal (list "/my/root/" projectile-git-command '("src/" "test/")))))
  (it "leaves the single-keep-dir case on the per-directory path"
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (spy-on 'projectile-dir-files-alien :and-return-value '("a.el"))
    (spy-on 'projectile-adjust-files :and-call-fake (lambda (_p _v files) files))
    (spy-on 'projectile-parse-dirconfig-file :and-return-value
            (make-projectile-dirconfig :keep '("src/")))
    (spy-on 'projectile-project-root :and-return-value "/my/root/")
    (spy-on 'file-directory-p :and-call-fake
            (lambda (filename)
              (member filename '("/my/root/" "/my/root/src/"))))
    (let ((projectile-indexing-method 'hybrid)
          (projectile-enable-caching nil)
          (projectile-files-cache-expire nil))
      (projectile-project-files "/my/root/"))
    ;; With one keep entry there are no extra shell calls to save, so
    ;; we keep going through projectile-dir-files (which threads vcs
    ;; through to projectile-dir-files-alien).
    (expect 'projectile-dir-files-alien
            :to-have-been-called-with "/my/root/src/" 'git)))

(describe "projectile-project-files single-directory relativization"
  (it "returns the directory listing as-is when the only dir is the project root"
    ;; Native (and hybrid without keep entries) walk a single directory
    ;; that *is* the project root, so the paths come back already relative
    ;; to it - re-relativising them against the root must be a no-op.
    (spy-on 'projectile-dir-files :and-return-value '("src/a.el" "b.el"))
    (spy-on 'file-relative-name)
    (let ((projectile-indexing-method 'native)
          (projectile-enable-caching nil)
          (projectile-files-cache-expire nil))
      (expect (projectile-project-files "/my/root/")
              :to-equal '("src/a.el" "b.el"))
      ;; The redundant per-file re-relativisation is skipped entirely.
      (expect 'file-relative-name :not :to-have-been-called)))
  (it "relativises against the root when the dir differs from it"
    (spy-on 'projectile-get-project-directories
            :and-return-value '("/my/root/src/"))
    (spy-on 'projectile-dir-files :and-return-value '("a.el" "nested/b.el"))
    (let ((projectile-indexing-method 'native)
          (projectile-enable-caching nil)
          (projectile-files-cache-expire nil))
      (expect (projectile-project-files "/my/root/")
              :to-equal '("src/a.el" "src/nested/b.el")))))

(describe "projectile--restricted-sub-projects-files"
  (it "returns all submodule files when no subdirs are supplied"
    (spy-on 'projectile-get-sub-projects-files :and-return-value
            '("vendor/foo/x.txt" "src/sub/y.txt"))
    (expect (projectile--restricted-sub-projects-files "/r/" 'git nil)
            :to-equal '("vendor/foo/x.txt" "src/sub/y.txt")))
  (it "drops submodule files outside the supplied subdirs"
    (spy-on 'projectile-get-sub-projects-files :and-return-value
            '("vendor/foo/x.txt" "src/sub/y.txt"))
    (expect (projectile--restricted-sub-projects-files "/r/" 'git '("src/"))
            :to-equal '("src/sub/y.txt")))
  (it "normalises subdirs without a trailing slash"
    (spy-on 'projectile-get-sub-projects-files :and-return-value
            '("src/sub/y.txt"))
    (expect (projectile--restricted-sub-projects-files "/r/" 'git '("src"))
            :to-equal '("src/sub/y.txt"))))

(describe "projectile-project-dirs"
  (it "includes intermediate directories that contain only subdirectories"
    (spy-on 'projectile-project-files
            :and-return-value '("src/ComponentA/a.cc"
                                "src/ComponentB/b.cc"
                                "config/config_file"))
    (let ((dirs (projectile-project-dirs "/project/")))
      ;; Leaf directories
      (expect dirs :to-contain "src/ComponentA/")
      (expect dirs :to-contain "src/ComponentB/")
      (expect dirs :to-contain "config/")
      ;; Intermediate directory (only has subdirectories, no direct files)
      (expect dirs :to-contain "src/"))))

(describe "projectile-index-directory"
  (it "skips unreadable directories"
    (unless (eq system-type 'windows-nt)
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/"
         "project/.projectile"
         "project/readable-file.el"
         "project/unreadable-dir/")
        (let* ((project-dir (file-name-as-directory (expand-file-name "project")))
               (unreadable-dir (expand-file-name "unreadable-dir" project-dir))
               (progress-reporter (make-progress-reporter "Indexing...")))
          (set-file-modes unreadable-dir #o000)
          (unwind-protect
              (let ((files (projectile-index-directory project-dir nil progress-reporter)))
                (expect (cl-some (lambda (f) (string-match-p "readable-file" f)) files) :to-be-truthy)
                (expect (cl-some (lambda (f) (string-match-p "unreadable-dir" f)) files) :not :to-be-truthy))
            (set-file-modes unreadable-dir #o755)))))))
  (it "honors dirconfig glob ignore patterns at every level"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile"
       "project/keep.el"
       "project/skip.elc"
       "project/src/"
       "project/src/keep.el"
       "project/src/skip.elc")
      (let* ((project-dir (file-name-as-directory (expand-file-name "project")))
             (progress-reporter (make-progress-reporter "Indexing...")))
        (with-temp-file (expand-file-name ".projectile" project-dir)
          (insert "-*.elc\n"))
        (spy-on 'projectile-project-root :and-return-value project-dir)
        (let ((files (projectile-index-directory project-dir
                                                 (projectile-filtering-patterns)
                                                 progress-reporter)))
          (expect (cl-some (lambda (f) (string-match-p "/keep.el\\'" f)) files)
                  :to-be-truthy)
          (expect (cl-some (lambda (f) (string-match-p "/src/keep.el\\'" f)) files)
                  :to-be-truthy)
          (expect (cl-some (lambda (f) (string-match-p "skip\\.elc\\'" f)) files)
                  :not :to-be-truthy))))))
  (it "honors dirconfig ensure entries that override an ignore pattern"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/.projectile"
       "project/keep.elc"
       "project/skip.elc")
      (let* ((project-dir (file-name-as-directory (expand-file-name "project")))
             (progress-reporter (make-progress-reporter "Indexing...")))
        (with-temp-file (expand-file-name ".projectile" project-dir)
          (insert "-*.elc\n!keep.elc\n"))
        (spy-on 'projectile-project-root :and-return-value project-dir)
        (let ((files (projectile-index-directory project-dir
                                                 (projectile-filtering-patterns)
                                                 progress-reporter)))
          (expect (cl-some (lambda (f) (string-match-p "/keep\\.elc\\'" f)) files)
                  :to-be-truthy)
          (expect (cl-some (lambda (f) (string-match-p "skip\\.elc\\'" f)) files)
                  :not :to-be-truthy))))))
  (it "follows a symlink that points at a directory"
    (unless (eq system-type 'windows-nt)
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/"
         "project/.projectile"
         "project/real/"
         "project/real/inside.el")
        (let* ((project-dir (file-name-as-directory (expand-file-name "project")))
               (progress-reporter (make-progress-reporter "Indexing...")))
          (make-symbolic-link "real" (expand-file-name "link" project-dir))
          (let ((files (projectile-index-directory project-dir nil progress-reporter)))
            ;; The real directory is walked...
            (expect (cl-some (lambda (f) (string-match-p "/real/inside\\.el\\'" f)) files)
                    :to-be-truthy)
            ;; ...and so is the symlink pointing at it, matching the previous
            ;; `file-directory-p' follow-symlink behaviour.
            (expect (cl-some (lambda (f) (string-match-p "/link/inside\\.el\\'" f)) files)
                    :to-be-truthy)))))))
  (it "treats a symlink to a regular file as a file, not a directory"
    (unless (eq system-type 'windows-nt)
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/"
         "project/.projectile"
         "project/target.el")
        (let* ((project-dir (file-name-as-directory (expand-file-name "project")))
               (progress-reporter (make-progress-reporter "Indexing...")))
          (make-symbolic-link "target.el" (expand-file-name "alias.el" project-dir))
          (let ((files (projectile-index-directory project-dir nil progress-reporter)))
            (expect (cl-some (lambda (f) (string-match-p "/alias\\.el\\'" f)) files)
                    :to-be-truthy))))))))

(describe "projectile--list->set"
  (it "puts all elements as keys with value t and tests with equal"
    (let ((set (projectile--list->set '("a" "b/" "c"))))
      (expect (gethash "a" set) :to-be t)
      (expect (gethash "b/" set) :to-be t)
      (expect (gethash "missing" set) :to-be nil)))
  (it "handles the empty list without error"
    (let ((set (projectile--list->set nil)))
      (expect (hash-table-count set) :to-equal 0))))

(describe "projectile-get-sub-projects-command"
  (it "gets sub projects command for git"
    (expect (string-prefix-p "git" (projectile-get-sub-projects-command 'git)) :to-be-truthy))
  (it "returns nil when vcs is not supported"
    (expect (projectile-get-sub-projects-command 'none) :to-be nil)))

(describe "projectile-get-ext-command"
  (it "returns the git command for git"
    (let ((projectile-git-use-fd nil)
          (projectile-fd-executable nil))
      (expect (projectile-get-ext-command 'git) :to-equal projectile-git-command)))
  (it "uses fd for git when fd is configured"
    (let ((projectile-git-use-fd t)
          (projectile-fd-executable "fd")
          (projectile-git-fd-args "-H -0"))
      (expect (projectile-get-ext-command 'git) :to-equal "fd -H -0")))
  (it "uses the remote-detected fd over TRAMP, not the local one"
    ;; With a remote DIRECTORY argument we should consult
    ;; `projectile-fd-executable-for' / the per-host cache, not just the
    ;; local `projectile-fd-executable'.
    (let ((projectile-git-use-fd t)
          (projectile-fd-executable "fd")
          (projectile-git-fd-args "-H -0")
          (projectile--remote-fd-executable-cache (make-hash-table :test 'equal)))
      ;; Cache says the remote has fdfind (not fd) - we must prefer it.
      (puthash "/ssh:host:" "fdfind" projectile--remote-fd-executable-cache)
      (expect (projectile-get-ext-command 'git "/ssh:host:/proj/")
              :to-equal "fdfind -H -0")
      ;; Cache says the remote has no fd at all - we must fall back to git.
      (puthash "/ssh:nofd:" nil projectile--remote-fd-executable-cache)
      (expect (projectile-get-ext-command 'git "/ssh:nofd:/proj/")
              :to-equal projectile-git-command)))
  (it "returns the matching command for each non-git VCS"
    (expect (projectile-get-ext-command 'hg) :to-equal projectile-hg-command)
    (expect (projectile-get-ext-command 'svn) :to-equal projectile-svn-command)
    (expect (projectile-get-ext-command 'bzr) :to-equal projectile-bzr-command)
    (expect (projectile-get-ext-command 'darcs) :to-equal projectile-darcs-command)
    (expect (projectile-get-ext-command 'fossil) :to-equal projectile-fossil-command)
    (expect (projectile-get-ext-command 'pijul) :to-equal projectile-pijul-command)
    (expect (projectile-get-ext-command 'sapling) :to-equal projectile-sapling-command)
    (expect (projectile-get-ext-command 'jj) :to-equal projectile-jj-command))
  (it "falls back to the generic command for unknown / no VCS"
    (expect (projectile-get-ext-command 'none) :to-equal projectile-generic-command)
    (expect (projectile-get-ext-command nil) :to-equal projectile-generic-command)))

(describe "projectile-fd-executable-for"
  (it "returns the local fd executable for a local directory"
    (let ((projectile-fd-executable "fd"))
      (expect (projectile-fd-executable-for "/tmp/") :to-equal "fd"))
    (let ((projectile-fd-executable nil))
      (expect (projectile-fd-executable-for "/tmp/") :to-be nil)))
  (it "consults the per-host cache for remote directories"
    (let ((projectile-fd-executable "fd")
          (projectile--remote-fd-executable-cache (make-hash-table :test 'equal)))
      (puthash "/ssh:host:" "fdfind" projectile--remote-fd-executable-cache)
      (expect (projectile-fd-executable-for "/ssh:host:/proj/") :to-equal "fdfind")
      (puthash "/ssh:nofd:" nil projectile--remote-fd-executable-cache)
      (expect (projectile-fd-executable-for "/ssh:nofd:/proj/") :to-be nil)))
  (it "performs the remote lookup once per host and caches the result"
    (let ((projectile--remote-fd-executable-cache (make-hash-table :test 'equal))
          (call-count 0))
      (spy-on 'executable-find :and-call-fake
              (lambda (name &optional _remote)
                (cl-incf call-count)
                (when (equal name "fdfind") "/usr/bin/fdfind")))
      (expect (projectile-fd-executable-for "/ssh:host:/proj/") :to-equal "fdfind")
      ;; Second call hits the cache, no new lookup.
      (expect (projectile-fd-executable-for "/ssh:host:/other/") :to-equal "fdfind")
      (expect call-count :to-equal 1))))

(describe "projectile-project-vcs"
  (it "detects each marker via a single directory listing"
    (projectile-test-with-sandbox
      (projectile-test-with-files
       ("project/.git/" "project/file")
       (let ((dir (file-name-as-directory (expand-file-name "project")))
             (listing-calls 0))
         (spy-on 'directory-files :and-call-through)
         (expect (projectile-project-vcs dir) :to-equal 'git)
         (setq listing-calls
               (length (spy-calls-all 'directory-files)))
         ;; The listing-calls count is bounded - we shouldn't be issuing
         ;; one per VCS marker (10 was the historic worst case).
         (expect listing-calls :to-be-less-than 3)))))

  (it "recognizes each VCS marker"
    ;; `projectile-test-with-files' is a macro that needs literal filenames,
    ;; so the markers are listed inline rather than via a `dolist'.
    (projectile-test-with-sandbox
      (projectile-test-with-files
       ("hg/.hg/" "bzr/.bzr/" "fslckout/.fslckout"
        "fossil/_FOSSIL_" "darcs/_darcs/" "pijul/.pijul/"
        "svn/.svn/" "sapling/.sl/" "jj/.jj/")
       (dolist (case '(("hg" . hg) ("bzr" . bzr) ("fslckout" . fossil)
                       ("fossil" . fossil) ("darcs" . darcs) ("pijul" . pijul)
                       ("svn" . svn) ("sapling" . sapling) ("jj" . jj)))
         (let ((dir (file-name-as-directory (expand-file-name (car case)))))
           (expect (projectile-project-vcs dir) :to-equal (cdr case)))))))

  (it "walks up to find a marker on an ancestor"
    (projectile-test-with-sandbox
      (projectile-test-with-files
       ("repo/.git/" "repo/sub/file")
       (let ((dir (file-name-as-directory (expand-file-name "repo/sub"))))
         (expect (projectile-project-vcs dir) :to-equal 'git)))))

  (it "caches the result and skips the directory listing on hit"
    (projectile-test-with-sandbox
      (projectile-test-with-files
       ("project/.git/")
       (let ((dir (file-name-as-directory (expand-file-name "project"))))
         (expect (projectile-project-vcs dir) :to-equal 'git)
         (spy-on 'directory-files :and-call-through)
         (expect (projectile-project-vcs dir) :to-equal 'git)
         (expect 'directory-files :not :to-have-been-called)))))

  (it "recognizes osc checkouts"
    (projectile-test-with-sandbox
      (projectile-test-with-files
       ("pkg/.osc/" "pkg/pkg.spec")
       (let ((dir (file-name-as-directory (expand-file-name "pkg"))))
         (expect (projectile-project-vcs dir) :to-equal 'osc)))))

  (it "breaks colocated-marker ties by projectile-vcs-markers order"
    (projectile-test-with-sandbox
      (projectile-test-with-files
       ("colocated/.git/" "colocated/.jj/")
       (let ((dir (file-name-as-directory (expand-file-name "colocated"))))
         ;; Default order prefers git...
         (expect (projectile-project-vcs dir) :to-equal 'git)
         ;; ... and moving .jj to the front prefers jj.
         (let ((projectile-vcs-markers
                (cons '(".jj" . jj)
                      (assoc-delete-all ".jj" (copy-alist projectile-vcs-markers))))
               (projectile-project-vcs-cache (make-hash-table :test 'equal)))
           (expect (projectile-project-vcs dir) :to-equal 'jj))))))

  (it "returns none when no marker exists anywhere up the tree"
    ;; The test sandbox lives inside Projectile's own git checkout, so
    ;; the upward walk would find its .git; use a temp dir instead.
    (let* ((dir (file-name-as-directory
                 (make-temp-file "projectile-no-vcs" t)))
           (projectile-project-vcs-cache (make-hash-table :test 'equal)))
      (unwind-protect
          (expect (projectile-project-vcs dir) :to-equal 'none)
        (delete-directory dir t)))))

(describe "projectile--ext-command-line"
  (it "returns the command unchanged when there are no pathspecs"
    (expect (projectile--ext-command-line "git ls-files -zco" nil)
            :to-equal "git ls-files -zco"))
  (it "appends shell-quoted pathspecs for tools that take trailing paths"
    (expect (projectile--ext-command-line "git ls-files -zco --exclude-standard"
                                          '("src/" "test dir/"))
            :to-equal "git ls-files -zco --exclude-standard src/ test\\ dir/"))
  (it "passes fd dirs via --search-path and drops --strip-cwd-prefix (#2005)"
    ;; fd rejects --strip-cwd-prefix alongside explicit paths, and its
    ;; `[pattern] [path...]' grammar would misread a trailing path as the
    ;; search pattern; --search-path sidesteps both.
    (expect (projectile--ext-command-line
             "fd -H -0 -E .git -tf --strip-cwd-prefix -c never" '("src/" "tests/"))
            :to-equal
            "fd -H -0 -E .git -tf -c never --search-path src/ --search-path tests/"))
  (it "keeps an existing fd search pattern while rewriting the paths"
    (expect (projectile--ext-command-line
             "fd . -0 --type f --color=never --strip-cwd-prefix" '("src/"))
            :to-equal "fd . -0 --type f --color=never --search-path src/"))
  (it "handles fd's --strip-cwd-prefix=<when> value form"
    (expect (projectile--ext-command-line "fd . --strip-cwd-prefix=always" '("src/"))
            :to-equal "fd . --search-path src/")))

(describe "projectile-files-via-ext-command"
          (it "returns nil when command is nil or empty"
              (expect (projectile-files-via-ext-command "/" "") :not :to-be-truthy)
              (expect (projectile-files-via-ext-command "/" nil) :not :to-be-truthy)
              (expect (projectile-files-via-ext-command temporary-file-directory "echo filename")
                      :to-equal '("filename")))

          (it "signals a user-error when the command exits non-zero with no output"
              ;; `false' is a portable way to force a non-zero exit; the previous
              ;; behavior was to silently return nil, which made fd-on-remote
              ;; failures look like empty projects.
              (expect (projectile-files-via-ext-command temporary-file-directory "false")
                      :to-throw 'user-error))

          (it "uses the output when the command exits non-zero but still produced some"
              ;; External listers such as `fd' routinely exit non-zero on benign
              ;; conditions (e.g. an unreadable directory) while having listed
              ;; everything else, so a non-zero exit with output must not abort
              ;; (see #2042).
              (expect (projectile-files-via-ext-command
                       temporary-file-directory "printf 'foo\\0bar\\0'; exit 1")
                      :to-equal '("foo" "bar")))

          (it "supports magic file handlers"
              (expect (projectile-files-via-ext-command "#magic#" "echo filename") :to-equal '("magic")))

          (it "strips ./ prefix from results"
              (expect (projectile-files-via-ext-command
                       temporary-file-directory "printf './foo\\0./bar/baz\\0quux'")
                      :to-equal '("foo" "bar/baz" "quux")))

          (it "appends shell-quoted pathspecs to the command when supplied"
              (expect (projectile-files-via-ext-command
                       temporary-file-directory "printf 'args: %s\\0' --"
                       '("src dir" "test"))
                      :to-equal
                      '("args: --" "args: src dir" "args: test"))))

(describe "projectile-get-all-sub-projects"
  (it "excludes out-of-project submodules"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      (;; VCS root is here
       "project/"
       "project/.git/"
       "project/.gitmodules"
       ;; Current project root is here:
       "project/web-ui/"
       "project/web-ui/.projectile"
       ;; VCS git submodule will return the following submodules,
       ;; relative to current project root, 'project/web-ui/':
       "project/web-ui/vendor/client-submodule/"
       "project/server/vendor/server-submodule/")
      (let ((project (file-truename (expand-file-name "project/web-ui"))))
        (spy-on 'projectile--git-submodules :and-call-fake
                (lambda (path)
                  (when (string= path project)
                    '("vendor/client-submodule"
                      "../server/vendor/server-submodule"))))
        (spy-on 'projectile-project-root :and-return-value project)
        ;; assert that it only returns the submodule 'project/web-ui/vendor/client-submodule/'
        (expect (projectile-get-all-sub-projects project) :to-equal
                (list (expand-file-name "vendor/client-submodule/" project))))))))

(describe "projectile--git-submodules"
  (it "computes the listing only once for repeated calls"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/.gitmodules")
      (let ((root (file-truename (expand-file-name "project/"))))
        (spy-on 'projectile--git-submodule-paths
                :and-return-value '("vendor/sub"))
        (expect (projectile--git-submodules root) :to-equal '("vendor/sub"))
        (expect (projectile--git-submodules root) :to-equal '("vendor/sub"))
        (expect 'projectile--git-submodule-paths :to-have-been-called-times 1)))))
  (it "recomputes the listing when .gitmodules changes"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/.gitmodules")
      (let ((root (file-truename (expand-file-name "project/"))))
        (spy-on 'projectile--git-submodule-paths
                :and-return-value '("vendor/sub"))
        (projectile--git-submodules root)
        ;; Force a distinct mtime — file-attribute-modification-time has
        ;; second-level resolution on some filesystems.
        (set-file-times (expand-file-name ".gitmodules" root)
                        (time-add (current-time) 5))
        (projectile--git-submodules root)
        (expect 'projectile--git-submodule-paths :to-have-been-called-times 2)))))
  (it "is cleared for the project by projectile-invalidate-cache"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/.gitmodules")
      (let ((root (file-truename (expand-file-name "project/"))))
        (spy-on 'projectile--git-submodule-paths
                :and-return-value '("vendor/sub"))
        (spy-on 'projectile-project-root :and-return-value root)
        ;; Avoid touching the on-disk cache file or recentf during the test.
        (spy-on 'projectile-persistent-cache-p :and-return-value nil)
        (spy-on 'recentf-cleanup)
        (projectile--git-submodules root)
        (expect (gethash root projectile--git-submodules-cache) :not :to-be nil)
        (projectile-invalidate-cache nil)
        (expect (gethash root projectile--git-submodules-cache) :to-be nil)
        (projectile--git-submodules root)
        (expect 'projectile--git-submodule-paths :to-have-been-called-times 2)))))
  (it "skips the listing entirely when there is no .gitmodules"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/")
      (let ((root (file-truename (expand-file-name "project/"))))
        (spy-on 'projectile--git-submodule-paths)
        (spy-on 'projectile-files-via-ext-command)
        (expect (projectile--git-submodules root) :to-be nil)
        (expect (gethash root projectile--git-submodules-cache) :to-be nil)
        (expect 'projectile--git-submodule-paths :not :to-have-been-called)
        (expect 'projectile-files-via-ext-command :not :to-have-been-called)))))
  (it "lists submodules with the default command without shelling out"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/vendor/sub/.git")
      (let ((root (file-truename (expand-file-name "project/"))))
        (with-temp-file (expand-file-name ".gitmodules" root)
          (insert "[submodule \"sub\"]\n"
                  "\tpath = vendor/sub\n"
                  "\turl = https://example.com/sub.git\n"))
        (spy-on 'projectile-files-via-ext-command)
        (expect (projectile--git-submodules root) :to-equal '("vendor/sub"))
        (expect 'projectile-files-via-ext-command :not :to-have-been-called)))))
  (it "rebases the listing when PATH is below the .gitmodules dir"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/web-ui/"
       "project/web-ui/vendor/sub/.git"
       "project/server/vendor/other/.git")
      (let ((root (file-truename (expand-file-name "project/"))))
        (with-temp-file (expand-file-name ".gitmodules" root)
          (insert "[submodule \"sub\"]\n"
                  "\tpath = web-ui/vendor/sub\n"
                  "\turl = https://example.com/sub.git\n"
                  "[submodule \"other\"]\n"
                  "\tpath = server/vendor/other\n"
                  "\turl = https://example.com/other.git\n"))
        (expect (projectile--git-submodules (expand-file-name "web-ui" root))
                :to-equal '("vendor/sub" "../server/vendor/other"))))))
  (it "honors a customized command as a shell command"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/.gitmodules")
      (let ((root (file-truename (expand-file-name "project/")))
            (projectile-git-submodule-command "git my-submodule-lister"))
        (spy-on 'projectile-files-via-ext-command
                :and-return-value '("vendor/sub"))
        (spy-on 'projectile--git-submodule-paths)
        (expect (projectile--git-submodules root) :to-equal '("vendor/sub"))
        (expect 'projectile-files-via-ext-command
                :to-have-been-called-with root "git my-submodule-lister")
        (expect 'projectile--git-submodule-paths :not :to-have-been-called)))))
  (it "returns nil when projectile-git-submodule-command is nil"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/.gitmodules")
      (let ((root (file-truename (expand-file-name "project/")))
            (projectile-git-submodule-command nil))
        (spy-on 'projectile--git-submodule-paths)
        (spy-on 'projectile-files-via-ext-command)
        (expect (projectile--git-submodules root) :to-be nil)
        (expect 'projectile--git-submodule-paths :not :to-have-been-called)
        (expect 'projectile-files-via-ext-command :not :to-have-been-called))))))

(describe "projectile-define-project-cache"
  ;; Shadow the registry so the caches defined here don't leak into
  ;; the global lists after the specs.
  :var (projectile--project-cache-vars projectile--project-cache-cleanups)
  (before-each
    (setq projectile--project-cache-vars
          (default-value 'projectile--project-cache-vars))
    (setq projectile--project-cache-cleanups
          (copy-alist (default-value 'projectile--project-cache-cleanups))))

  (it "wires a new cache into invalidation automatically"
    (eval '(projectile-define-project-cache projectile-test--registry-cache
             "A throwaway cache for the registry spec.")
          t)
    (puthash "/proj/" 'value projectile-test--registry-cache)
    (puthash "/other/" 'value projectile-test--registry-cache)
    (projectile--invalidate-project-cache "/proj/")
    (expect (gethash "/proj/" projectile-test--registry-cache) :to-be nil)
    (expect (gethash "/other/" projectile-test--registry-cache) :to-be 'value)
    (expect (memq 'projectile-test--registry-cache
                  projectile--project-cache-vars)
            :to-be-truthy))

  (it "drops all entries under the root for prefix-keyed caches"
    (eval '(projectile-define-project-cache projectile-test--prefix-cache
             "A throwaway prefix-keyed cache for the registry spec."
             :prefix-keyed t)
          t)
    (puthash "/proj/sub/" 'value projectile-test--prefix-cache)
    (puthash "/proj/other/" 'value projectile-test--prefix-cache)
    (puthash "/elsewhere/" 'value projectile-test--prefix-cache)
    (projectile--invalidate-project-cache "/proj/")
    (expect (hash-table-count projectile-test--prefix-cache) :to-equal 1)
    (expect (gethash "/elsewhere/" projectile-test--prefix-cache)
            :to-be 'value))

  (it "cancels a pending cache flush on invalidation"
    (let ((projectile--pending-cache-flush-timers
           (make-hash-table :test 'equal))
          (timer (timer-create)))
      (puthash "/proj/" timer projectile--pending-cache-flush-timers)
      (spy-on 'cancel-timer)
      (spy-on 'projectile-persistent-cache-p :and-return-value nil)
      (projectile--invalidate-project-cache "/proj/")
      (expect 'cancel-timer :to-have-been-called-with timer)
      (expect (gethash "/proj/" projectile--pending-cache-flush-timers)
              :to-be nil))))

(describe "projectile-invalidate-cache-all"
  (it "clears the per-project caches of every known project"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.git/"
       "projectB/.git/")
      (let* ((root-a (file-truename (expand-file-name "projectA/")))
             (root-b (file-truename (expand-file-name "projectB/")))
             (roots (list root-a root-b))
             (projectile-enable-caching 'transient)
             (projectile-projects-cache (make-hash-table :test 'equal))
             (projectile-projects-cache-time (make-hash-table :test 'equal))
             (projectile-project-type-cache (make-hash-table :test 'equal))
             (projectile--dirconfig-cache (make-hash-table :test 'equal))
             (projectile-file-exists-cache (make-hash-table :test 'equal)))
        (spy-on 'projectile-known-projects :and-return-value roots)
        (spy-on 'recentf-cleanup)
        (dolist (root roots)
          (puthash root '("a.el") projectile-projects-cache)
          (puthash root 0 projectile-projects-cache-time)
          (puthash root 'generic projectile-project-type-cache)
          (puthash root 'git projectile-project-vcs-cache)
          (puthash root 'dirconfig projectile--dirconfig-cache)
          ;; submodule entries are keyed per directory under the root
          (puthash (expand-file-name "sub/" root) '("vendor/sub")
                   projectile--git-submodules-cache))
        (puthash "/some/file" (cons 'found (current-time))
                 projectile-file-exists-cache)
        (projectile-invalidate-cache-all)
        (dolist (root roots)
          (expect (gethash root projectile-projects-cache) :to-be nil)
          (expect (gethash root projectile-projects-cache-time) :to-be nil)
          (expect (gethash root projectile-project-type-cache) :to-be nil)
          (expect (gethash root projectile-project-vcs-cache) :to-be nil)
          (expect (gethash root projectile--dirconfig-cache) :to-be nil))
        (expect (hash-table-count projectile--git-submodules-cache) :to-equal 0)
        (expect (hash-table-count projectile-file-exists-cache) :to-equal 0)
        (expect (hash-table-count projectile-project-root-cache) :to-equal 0)))))

  (it "deletes existing on-disk cache files but doesn't create missing ones"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/.git/"
       "projectB/.git/")
      (let* ((root-a (file-truename (expand-file-name "projectA/")))
             (root-b (file-truename (expand-file-name "projectB/")))
             (projectile-enable-caching 'persistent)
             (projectile-projects-cache (make-hash-table :test 'equal))
             (projectile-projects-cache-time (make-hash-table :test 'equal))
             (cache-a (projectile-project-cache-file root-a))
             (cache-b (projectile-project-cache-file root-b)))
        (spy-on 'projectile-known-projects
                :and-return-value (list root-a root-b))
        (spy-on 'recentf-cleanup)
        (with-temp-file cache-a (insert "(\"a.el\")"))
        (projectile-invalidate-cache-all)
        (expect (file-exists-p cache-a) :to-be nil)
        (expect (file-exists-p cache-b) :to-be nil)))))

  (it "skips remote projects"
    (let ((projectile-projects-cache (make-hash-table :test 'equal))
          (projectile-enable-caching 'persistent))
      (spy-on 'projectile-known-projects
              :and-return-value '("/ssh:example.com:/proj/" "/local/proj/"))
      (spy-on 'projectile--invalidate-project-cache)
      (spy-on 'recentf-cleanup)
      (projectile-invalidate-cache-all)
      (expect 'projectile--invalidate-project-cache :to-have-been-called-times 1)
      (expect 'projectile--invalidate-project-cache
              :to-have-been-called-with "/local/proj/"))))

(describe "projectile--git-submodule-paths"
  (it "parses .gitmodules with git and keeps only populated submodules"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       ;; Populated submodules have a .git entry (a gitfile in practice).
       "project/vendor/first/.git"
       "project/dir with spaces/.git"
       ;; Registered but never initialized: no .git inside.
       "project/vendor/uninitialized/")
      (let ((root (file-truename (expand-file-name "project/"))))
        (with-temp-file (expand-file-name ".gitmodules" root)
          (insert "# top-level comment\n"
                  "[submodule \"first\"]\n"
                  "\tpath = vendor/first\n"
                  "\turl = https://example.com/first.git\n"
                  "[submodule \"spaced\"]\n"
                  "\tpath = \"dir with spaces\"\n"
                  "\turl = https://example.com/spaced.git\n"
                  "[submodule \"uninitialized\"]\n"
                  "\tpath = vendor/uninitialized\n"
                  "\turl = https://example.com/uninit.git\n"))
        (expect (projectile--git-submodule-paths root)
                :to-equal '("vendor/first" "dir with spaces"))))))
  (it "returns nil for a .gitmodules without path entries"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.git/"
       "project/.gitmodules")
      (let ((root (file-truename (expand-file-name "project/"))))
        (expect (projectile--git-submodule-paths root) :to-be nil))))))

(describe "projectile-get-all-sub-projects-files"
  (it "returns relative paths to submodule files"
    (spy-on 'projectile-get-all-sub-projects :and-return-value '("/a/b/x/"))
    (spy-on 'projectile-files-via-ext-command :and-return-value '("1.txt" "2.txt"))
    (expect (projectile-get-sub-projects-files "/a/b" 'git) :to-equal
      (list "x/1.txt" "x/2.txt"))))

(describe "projectile--directory-p"
  (it "tests which directory exists"
    (expect (projectile--directory-p nil) :to-be nil)
    (expect (projectile--directory-p "asdf") :to-be nil)
    (expect (projectile--directory-p user-emacs-directory) :to-be-truthy)))

(describe "projectile-find-file-in-directory"
  (it "fails when called in a non-existing directory"
    (expect (projectile-find-file-in-directory "asdf") :to-throw)))

(describe "projectile-dir-files-native"
  (it "calculates ignored files and directories only once during recursion"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/"
       "projectA/.svn/"
       "projectA/src/.svn/"
       "projectA/src/html/.svn/"
       "projectA/.git/"
       "projectA/src/html/"
       "projectA/src/framework/lib/"
       "projectA/src/framework.conf"
       "projectA/src/html/index.html"
       "projectA/.projectile")

      ;; verify that indexing only invokes these funcs once during recursion
      (spy-on 'projectile-ignored-files :and-call-through)
      (spy-on 'projectile-ignored-directories :and-call-through)
      (spy-on 'projectile-globally-ignored-directory-names :and-call-through)

      (projectile-dir-files-native "projectA/")
      (expect 'projectile-ignored-files :to-have-been-called-times 1)
      (expect 'projectile-globally-ignored-directory-names :to-have-been-called-times 1)
      (expect 'projectile-ignored-directories :to-have-been-called-times 1))))
  (it "ignores globally ignored directories when using native indexing"
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/"
         "project/.ignoreme/"
         "project/.ignoreme/should_ignore"
         "project/src/"
         "project/src/.ignoreme/"
         "project/src/.ignoreme/should_ignore"
         "project/config.conf")

        (setq projectile-globally-ignored-directories '(".ignoreme"))
        (expect (projectile-dir-files-native "project") :to-equal '("config.conf"))))))

(describe "projectile--vcs-from-directory-listing"
  (it "detects the VCS from a marker directly inside the directory"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("repo/.git/" "repo/src/")
      (expect (projectile--vcs-from-directory-listing (expand-file-name "repo/"))
              :to-equal 'git))))
  (it "returns nil when no VCS marker is present"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("plain/src/" "plain/README")
      (expect (projectile--vcs-from-directory-listing (expand-file-name "plain/"))
              :to-equal nil))))
  (it "returns nil for a directory that does not exist"
    (projectile-test-with-sandbox
     (expect (projectile--vcs-from-directory-listing
              (expand-file-name "does-not-exist/"))
             :to-equal nil))))

(describe "native/hybrid dirconfig parity"
  ;; The acceptance test for the unified dirconfig matcher (#1941): the
  ;; same .projectile must produce the same file set under native and
  ;; hybrid indexing.
  (it "produces the same file set for the same .projectile"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/"
       "project/src/"
       "project/src/main.c"
       "project/src/gen/"
       "project/src/gen/out.c"
       "project/vendor/"
       "project/vendor/lib.js"
       "project/docs/"
       "project/docs/a.text"
       "project/keep.text"
       "project/README.md")
      (let ((root (file-truename (expand-file-name "project/"))))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-*.text\n!keep.text\n-vendor/\n-src/gen/\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (spy-on 'projectile-project-vcs :and-return-value 'git)
        ;; The external tool lists everything; hybrid's post-filtering
        ;; must reduce it to what the native walker produces.
        (spy-on 'projectile-files-via-ext-command :and-return-value
                '(".projectile" "src/main.c" "src/gen/out.c" "vendor/lib.js"
                  "docs/a.text" "keep.text" "README.md"))
        (spy-on 'projectile-get-sub-projects-files :and-return-value nil)
        (spy-on 'projectile-git-deleted-files :and-return-value nil)
        ;; The sandbox is not a real git repo, so the VCS-ignored file
        ;; listings behind projectile-add-unignored must be stubbed too.
        (spy-on 'projectile-get-repo-ignored-files :and-return-value nil)
        (spy-on 'projectile-get-repo-ignored-directory :and-return-value nil)
        (let* ((projectile-enable-caching nil)
               (projectile-git-use-fd nil)
               (projectile-fd-executable nil)
               (native (let ((projectile-indexing-method 'native))
                         (projectile-dir-files root)))
               (hybrid (let ((projectile-indexing-method 'hybrid))
                         (projectile-dir-files root))))
          (expect (sort native #'string<) :to-equal (sort hybrid #'string<))
          (expect native :to-contain "src/main.c")
          (expect native :to-contain "README.md")
          (expect native :to-contain "keep.text")
          (expect native :not :to-contain "docs/a.text")
          (expect native :not :to-contain "vendor/lib.js")
          (expect native :not :to-contain "src/gen/out.c")))))))

;;; projectile-indexing-test.el ends here
