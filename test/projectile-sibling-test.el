;;; projectile-sibling-test.el --- Tests for sibling project switching -*- lexical-binding: t -*-

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

;; Tests for `projectile-switch-sibling-project' and the signals it uses
;; to decide which projects are related to one another.

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-sibling-test--names (paths)
  "Return the directory names of PATHS, for readable expectations."
  (mapcar (lambda (path)
            (file-name-nondirectory (directory-file-name path)))
          paths))


;;; Deriving an owner from a remote

(describe "projectile--repo-url-owner"
  (it "returns the account a forge repository hangs off"
    (expect (projectile--repo-url-owner "github.com/bbatsov/projectile")
            :to-equal "github.com/bbatsov"))

  (it "keeps subgroups with the owner"
    (expect (projectile--repo-url-owner "gitlab.com/group/sub/proj")
            :to-equal "gitlab.com/group/sub"))

  (it "returns the containing directory of a local repository"
    (expect (projectile--repo-url-owner "/srv/git/repo")
            :to-equal "/srv/git"))

  (it "returns nil when there's no owner, only a host and a repo"
    ;; Grouping on the host would relate every repository on it.
    (expect (projectile--repo-url-owner "github.com/projectile") :to-be nil))

  (it "returns nil for no remote at all"
    (expect (projectile--repo-url-owner nil) :to-be nil)))

(describe "projectile-repo-identity"
  (it "carries the owner of the remote"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo
                  "repo" "git@github.com:bbatsov/projectile.git")))
       (expect (plist-get (projectile-repo-identity repo) :owner)
               :to-equal "github.com/bbatsov"))))

  (it "leaves the owner nil when there's no remote"
    (projectile-test-with-sandbox
     (let ((repo (projectile-test-init-git-repo "repo")))
       (expect (plist-get (projectile-repo-identity repo) :owner) :to-be nil)))))


;;; Tokenizing a project's name

(describe "projectile--project-leading-token"
  (it "takes the first word, splitting on the usual separators"
    (expect (projectile--project-leading-token "/src/rubocop-ast/")
            :to-equal "rubocop")
    (expect (projectile--project-leading-token "/src/docs.cider.mx/")
            :to-equal "docs")
    (expect (projectile--project-leading-token "/src/my_cool_thing/")
            :to-equal "my"))

  (it "downcases the word"
    (expect (projectile--project-leading-token "/src/FsUnit/")
            :to-equal "fsunit"))

  (it "skips single characters, which group nothing usefully"
    (expect (projectile--project-leading-token "/src/a-project/")
            :to-equal "project"))

  (it "copes with a path that has no trailing slash"
    (expect (projectile--project-leading-token "/src/rubocop-ast")
            :to-equal "rubocop")))


;;; The signals

(describe "projectile-siblings-from-groups"
  (it "returns the members of a group the project belongs to"
    (let ((projectile-project-groups
           '(("editor" . ("/src/editor/" "/src/editor-docs/"))
             ("infra" . ("/src/deploy/" "/src/tf/")))))
      (expect (projectile-siblings-from-groups "/src/editor/")
              :to-equal '("/src/editor/" "/src/editor-docs/"))))

  (it "combines every group the project belongs to"
    (let ((projectile-project-groups
           '(("one" . ("/src/a/" "/src/b/"))
             ("two" . ("/src/a/" "/src/c/")))))
      (expect (projectile-siblings-from-groups "/src/a/")
              :to-have-same-items-as '("/src/a/" "/src/b/" "/src/a/" "/src/c/"))))

  (it "matches a member however its path is spelled"
    (let ((projectile-project-groups '(("g" . ("/src/a" "/src/b/")))))
      (expect (projectile-siblings-from-groups "/src/a/")
              :to-equal '("/src/a" "/src/b/"))))

  (it "returns nothing for a project in no group"
    (let ((projectile-project-groups '(("g" . ("/src/a/" "/src/b/")))))
      (expect (projectile-siblings-from-groups "/src/z/") :to-be nil)))

  (it "includes what the project's own dir-locals variable names"
    (let ((projectile-project-groups nil)
          (projectile-project-siblings '("/src/partner/")))
      (spy-on 'projectile-project-root :and-return-value "/src/here/")
      (expect (projectile-siblings-from-groups "/src/here/")
              :to-equal '("/src/partner/"))))

  (it "does not attribute the current project's siblings to another root"
    ;; `projectile-project-siblings' is buffer-local and describes the
    ;; project you're in, so asking about a different one must not pick
    ;; it up.
    (let ((projectile-project-groups nil)
          (projectile-project-siblings '("/src/partner/")))
      (spy-on 'projectile-project-root :and-return-value "/src/here/")
      (expect (projectile-siblings-from-groups "/src/elsewhere/") :to-be nil))))

(describe "projectile-siblings-from-owner"
  (it "relates projects whose remotes share an owner"
    (projectile-test-with-sandbox
     (let* ((one (projectile-test-init-git-repo
                  "haystack" "git@github.com:clojure-emacs/haystack.git"))
            (two (projectile-test-init-git-repo
                  "orchard" "git@github.com:clojure-emacs/orchard.git"))
            (other (projectile-test-init-git-repo
                    "crux" "git@github.com:bbatsov/crux.git"))
            (projectile-known-projects (list one two other)))
       ;; The names have nothing in common - only the owner relates them.
       (expect (projectile-sibling-test--names
                (projectile-siblings-from-owner one))
               :to-have-same-items-as '("haystack" "orchard")))))

  (it "returns nothing for a project with no remote"
    (projectile-test-with-sandbox
     (let* ((repo (projectile-test-init-git-repo "repo"))
            (projectile-known-projects (list repo)))
       (expect (projectile-siblings-from-owner repo) :to-be nil)))))

(describe "projectile-siblings-from-name"
  (it "relates projects whose names start with the same word"
    (let ((projectile-known-projects
           '("/src/rubocop/" "/src/rubocop-ast/" "/src/rubocop-rails/"
             "/src/crux/")))
      (expect (projectile-sibling-test--names
               (projectile-siblings-from-name "/src/rubocop/"))
              :to-have-same-items-as '("rubocop" "rubocop-ast" "rubocop-rails"))))

  (it "only looks at the leading word"
    ;; `helm-projectile' shares a word with `projectile' but not the first
    ;; one; matching on any shared word relates far too much.
    (let ((projectile-known-projects
           '("/src/projectile/" "/src/helm-projectile/" "/src/crux/")))
      (expect (projectile-sibling-test--names
               (projectile-siblings-from-name "/src/projectile/"))
              :to-equal '("projectile"))))

  (it "does not relate everything ending in a common word"
    (let ((projectile-known-projects
           '("/src/clojure-mode/" "/src/fish-mode/" "/src/adoc-mode/")))
      (expect (projectile-sibling-test--names
               (projectile-siblings-from-name "/src/clojure-mode/"))
              :to-equal '("clojure-mode")))))


;;; The share cap

(describe "projectile--siblings-by-key"
  (defun projectile-sibling-test--numbered (count)
    (mapcar (lambda (i) (format "/src/p%d/" i)) (number-sequence 1 count)))

  (defun projectile-sibling-test--bucket (members)
    "Return a key function putting MEMBERS in one bucket and the rest apart."
    (lambda (project) (if (member project members) "shared" project)))

  (it "drops a group covering too much of the known projects"
    (let ((projectile-known-projects (projectile-sibling-test--numbered 20))
          (projectile-sibling-max-group-share 0.25))
      ;; 10 of 20 is well past a quarter, so the group says nothing.
      (expect (projectile--siblings-by-key
               "/src/p1/"
               (projectile-sibling-test--bucket
                (projectile-sibling-test--numbered 10)))
              :to-be nil)))

  (it "keeps a group that stays under the share"
    (let ((projectile-known-projects (projectile-sibling-test--numbered 20))
          (projectile-sibling-max-group-share 0.25))
      (expect (projectile--siblings-by-key
               "/src/p1/"
               (projectile-sibling-test--bucket
                '("/src/p1/" "/src/p2/" "/src/p3/")))
              :to-equal '("/src/p1/" "/src/p2/" "/src/p3/"))))

  (it "does not apply the cap when there are too few projects to take a share of"
    ;; Three of your four projects being related is a fine answer; the cap
    ;; is there for the machine with a hundred of them.
    (let ((projectile-known-projects '("/src/a/" "/src/b/" "/src/c/" "/src/d/"))
          (projectile-sibling-max-group-share 0.25))
      (expect (projectile--siblings-by-key
               "/src/a/"
               (projectile-sibling-test--bucket '("/src/a/" "/src/b/" "/src/c/")))
              :to-equal '("/src/a/" "/src/b/" "/src/c/"))))

  (it "always allows a pair, however small the configured share"
    (let ((projectile-known-projects (projectile-sibling-test--numbered 20))
          (projectile-sibling-max-group-share 0.01))
      (expect (projectile--siblings-by-key
               "/src/p1/"
               (projectile-sibling-test--bucket '("/src/p1/" "/src/p2/")))
              :to-equal '("/src/p1/" "/src/p2/"))))

  (it "keeps every group when the cap is disabled"
    (let ((projectile-known-projects (projectile-sibling-test--numbered 20))
          (projectile-sibling-max-group-share nil))
      (expect (length (projectile--siblings-by-key "/src/p1/" (lambda (_) "all")))
              :to-equal 20)))

  (it "returns nothing when the project itself has no key"
    (let ((projectile-known-projects '("/src/a/" "/src/b/")))
      (expect (projectile--siblings-by-key "/src/a/" #'ignore) :to-be nil)))

  (it "leaves out ignored projects"
    (let ((projectile-known-projects '("/src/a/" "/src/b/"))
          (projectile-ignored-projects '("/src/b/"))
          (projectile-sibling-max-group-share nil))
      (spy-on 'projectile-ignored-project-p :and-call-fake
              (lambda (p) (equal p "/src/b/")))
      (expect (projectile--siblings-by-key "/src/a/" (lambda (_) "all"))
              :to-equal '("/src/a/")))))


;;; Putting the signals together

(describe "projectile-sibling-projects"
  (it "offers configured groups before anything inferred"
    (let ((projectile-project-groups '(("g" . ("/src/aaa/" "/src/zzz/"))))
          (projectile-known-projects '("/src/aaa/" "/src/aaa-two/"))
          (projectile-sibling-project-functions
           '(projectile-siblings-from-groups projectile-siblings-from-name)))
      ;; `zzz' is unrelated by name but configured, so it comes before the
      ;; project only the name signal found.
      (expect (projectile-sibling-test--names
               (projectile-sibling-projects "/src/aaa/"))
              :to-equal '("aaa" "zzz" "aaa-two"))))

  (it "offers a project found by two signals only once"
    (let ((projectile-project-groups '(("g" . ("/src/aaa/" "/src/aaa-two/"))))
          (projectile-known-projects '("/src/aaa/" "/src/aaa-two/"))
          (projectile-sibling-project-functions
           '(projectile-siblings-from-groups projectile-siblings-from-name)))
      (expect (projectile-sibling-test--names
               (projectile-sibling-projects "/src/aaa/"))
              :to-equal '("aaa" "aaa-two"))))

  (it "survives a sibling function that throws"
    (let ((projectile-sibling-project-functions
           (list (lambda (_root) (error "boom"))
                 (lambda (_root) '("/src/other/")))))
      (expect (projectile-sibling-projects "/src/here/")
              :to-equal '("/src/other/")))))


;;; Switching

(describe "projectile-switch-sibling-project"
  (it "errors when nothing is related to the current project"
    (let ((projectile-sibling-project-functions nil))
      (spy-on 'projectile-acquire-root :and-return-value "/src/lonely/")
      (expect (projectile-switch-sibling-project) :to-throw 'user-error)))

  (it "does not offer the project we're already in"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("aaa/" "aaa-two/")
      (let* ((root (file-name-as-directory (expand-file-name "aaa")))
             (projectile-known-projects
              (list root (file-name-as-directory (expand-file-name "aaa-two"))))
             (projectile-sibling-project-functions
              '(projectile-siblings-from-name))
             offered)
        (spy-on 'projectile-acquire-root :and-return-value root)
        (spy-on 'projectile-completing-read :and-call-fake
                (lambda (_prompt choices &rest _args) (setq offered choices) nil))
        (projectile-switch-sibling-project)
        (expect (projectile-sibling-test--names offered) :to-equal '("aaa-two"))))))

  (it "does not offer a configured project that isn't there any more"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("here/")
      (let* ((root (file-name-as-directory (expand-file-name "here")))
             (projectile-project-groups
              (list (cons "g" (list root "/src/moved-away/"))))
             (projectile-sibling-project-functions
              '(projectile-siblings-from-groups)))
        (spy-on 'projectile-acquire-root :and-return-value root)
        (expect (projectile-switch-sibling-project) :to-throw 'user-error)))))

  (it "switches to the chosen project"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("aaa/" "aaa-two/")
      (let* ((root (file-name-as-directory (expand-file-name "aaa")))
             (sibling (file-name-as-directory (expand-file-name "aaa-two")))
             (projectile-known-projects (list root sibling))
             (projectile-sibling-project-functions
              '(projectile-siblings-from-name)))
        (spy-on 'projectile-acquire-root :and-return-value root)
        (spy-on 'projectile-switch-project-by-name)
        (spy-on 'projectile-completing-read :and-call-fake
                (lambda (_prompt choices &rest args)
                  (funcall (plist-get args :action) (car choices))))
        (projectile-switch-sibling-project)
        (expect (file-truename
                 (car (spy-calls-args-for 'projectile-switch-project-by-name 0)))
                :to-equal (file-truename sibling)))))))

(provide 'projectile-sibling-test)

;;; projectile-sibling-test.el ends here
