;;; projectile-file-kinds-test.el --- Tests for file kinds -*- lexical-binding: t -*-

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

;; Tests for the declarative "file kinds" feature and the
;; `projectile-find-file-of-kind' / `projectile-toggle-related-file'
;; commands built on top of it.

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-file-kinds-test--spec (kind)
  "Return the SPEC of the Rails file KIND."
  (cdr (assq kind projectile--rails-file-kinds)))

(describe "projectile--singularize"
  (it "handles the regular English cases"
    (expect (projectile--singularize "users") :to-equal "user")
    (expect (projectile--singularize "companies") :to-equal "company")
    (expect (projectile--singularize "boxes") :to-equal "box")
    (expect (projectile--singularize "buses") :to-equal "bus"))
  (it "leaves already-singular and -ss nouns unchanged"
    (expect (projectile--singularize "user") :to-equal "user")
    (expect (projectile--singularize "address") :to-equal "address")))

(describe "projectile--pluralize"
  (it "handles the regular English cases"
    (expect (projectile--pluralize "user") :to-equal "users")
    (expect (projectile--pluralize "company") :to-equal "companies")
    (expect (projectile--pluralize "box") :to-equal "boxes")
    (expect (projectile--pluralize "bus") :to-equal "buses")))

(describe "projectile--file-kind-member-p"
  (it "matches on a path prefix"
    (expect (projectile--file-kind-member-p "app/models/user.rb" '(:path "app/models/"))
            :to-be-truthy)
    (expect (projectile--file-kind-member-p "app/controllers/x.rb" '(:path "app/models/"))
            :not :to-be-truthy))
  (it "matches on a basename suffix"
    (expect (projectile--file-kind-member-p "a/users_controller.rb" '(:suffix "_controller.rb"))
            :to-be-truthy)
    (expect (projectile--file-kind-member-p "a/user.rb" '(:suffix "_controller.rb"))
            :not :to-be-truthy))
  (it "requires both path and suffix when both are given"
    (let ((spec '(:path "app/controllers/" :suffix "_controller.rb")))
      (expect (projectile--file-kind-member-p "app/controllers/users_controller.rb" spec)
              :to-be-truthy)
      (expect (projectile--file-kind-member-p "app/models/users_controller.rb" spec)
              :not :to-be-truthy)
      (expect (projectile--file-kind-member-p "app/controllers/users.rb" spec)
              :not :to-be-truthy))))

(describe "projectile--file-kind-default-key"
  (it "strips the extension when no suffix is given"
    (expect (projectile--file-kind-default-key "app/models/user.rb" '(:path "app/models/"))
            :to-equal "user"))
  (it "strips the suffix when one is given"
    (expect (projectile--file-kind-default-key "app/controllers/users_controller.rb"
                                               '(:path "app/controllers/"
                                                 :suffix "_controller.rb"))
            :to-equal "users"))
  (it "strips a prefix too"
    (expect (projectile--file-kind-default-key "tests/test_views.py"
                                               '(:path "tests/" :prefix "test_"))
            :to-equal "views"))
  (it "keeps namespace directories under the kind's path"
    ;; Files that differ only by subdirectory must get distinct keys, or
    ;; a top-level and a namespaced resource would wrongly relate.
    (expect (projectile--file-kind-default-key "app/models/admin/user.rb"
                                               '(:path "app/models/"))
            :to-equal "admin/user"))
  (it "keeps the full directory when the kind has no path anchor"
    (expect (projectile--file-kind-default-key "a/foo_controller.rb"
                                               '(:suffix "_controller.rb"))
            :to-equal "a/foo"))
  (it "returns nil for an empty key"
    (expect (projectile--file-kind-default-key "app/models/user.rb"
                                               '(:path "app/models/" :suffix "user.rb"))
            :to-equal nil)))

(describe "projectile--file-kind-match (Rails)"
  (it "derives a shared singular key across kinds"
    (expect (projectile--file-kind-match "app/models/user.rb"
                                         (projectile-file-kinds-test--spec :model))
            :to-equal "user")
    (expect (projectile--file-kind-match "app/controllers/users_controller.rb"
                                         (projectile-file-kinds-test--spec :controller))
            :to-equal "user")
    (expect (projectile--file-kind-match "app/views/users/index.html.erb"
                                         (projectile-file-kinds-test--spec :view))
            :to-equal "user")
    (expect (projectile--file-kind-match "app/helpers/users_helper.rb"
                                         (projectile-file-kinds-test--spec :helper))
            :to-equal "user"))
  (it "returns nil for files not of the kind"
    (expect (projectile--file-kind-match "app/models/user.rb"
                                         (projectile-file-kinds-test--spec :controller))
            :to-equal nil)
    ;; a view file directly under app/views has no resource directory
    (expect (projectile--file-kind-match "app/views/layout.html.erb"
                                         (projectile-file-kinds-test--spec :view))
            :to-equal nil)))

(describe "projectile--file-kind-match (Django)"
  (it "uses the app directory as the shared key"
    (expect (projectile--file-kind-match "polls/models.py"
                                         (cdr (assq :model projectile--django-file-kinds)))
            :to-equal "polls")
    (expect (projectile--file-kind-match "polls/views.py"
                                         (cdr (assq :view projectile--django-file-kinds)))
            :to-equal "polls")
    ;; A nested app keeps its full directory, so two apps both named
    ;; "blog" under different parents don't collide.
    (expect (projectile--file-kind-match "project/blog/urls.py"
                                         (cdr (assq :urls projectile--django-file-kinds)))
            :to-equal "project/blog")))

(describe "projectile--file-kinds-related-files-fn"
  (it "emits cross-kind predicates for the other Rails kinds"
    (let* ((fn (projectile--file-kinds-related-files-fn projectile--rails-file-kinds))
           (plist (funcall fn "app/controllers/users_controller.rb")))
      ;; the file's own kind is not emitted
      (expect (plist-member plist :controller) :to-equal nil)
      (expect (funcall (plist-get plist :model) "app/models/user.rb") :to-be-truthy)
      (expect (funcall (plist-get plist :model) "app/models/product.rb") :not :to-be-truthy)
      (expect (funcall (plist-get plist :view) "app/views/users/index.html.erb") :to-be-truthy)
      (expect (funcall (plist-get plist :helper) "app/helpers/users_helper.rb") :to-be-truthy)
      (expect (funcall (plist-get plist :helper) "app/helpers/products_helper.rb") :not :to-be-truthy)))
  (it "relates Django app files by directory"
    (let* ((fn (projectile--file-kinds-related-files-fn projectile--django-file-kinds))
           (plist (funcall fn "polls/models.py")))
      (expect (plist-member plist :model) :to-equal nil)
      (expect (funcall (plist-get plist :view) "polls/views.py") :to-be-truthy)
      (expect (funcall (plist-get plist :view) "blog/views.py") :not :to-be-truthy)
      (expect (funcall (plist-get plist :urls) "polls/urls.py") :to-be-truthy)))
  (it "returns nil for a file of no known kind"
    (let ((fn (projectile--file-kinds-related-files-fn projectile--rails-file-kinds)))
      (expect (funcall fn "config/routes.rb") :to-equal nil))))

(describe "projectile--related-file-candidates"
  (let ((files '("app/models/user.rb"
                 "app/controllers/users_controller.rb"
                 "app/views/users/index.html.erb"
                 "app/helpers/users_helper.rb"
                 "app/models/product.rb"
                 "app/controllers/products_controller.rb")))
    (it "returns one counterpart per related kind in table order"
      (expect (projectile--related-file-candidates
               "app/controllers/users_controller.rb"
               projectile--rails-file-kinds files)
              :to-equal '((:model . "app/models/user.rb")
                          (:view . "app/views/users/index.html.erb")
                          (:helper . "app/helpers/users_helper.rb"))))
    (it "returns a single counterpart when only one related kind exists"
      (expect (projectile--related-file-candidates
               "app/controllers/products_controller.rb"
               projectile--rails-file-kinds files)
              :to-equal '((:model . "app/models/product.rb"))))
    (it "returns nil for a file of no known kind"
      (expect (projectile--related-file-candidates
               "config/routes.rb" projectile--rails-file-kinds files)
              :to-equal nil)))

  (it "keeps namespaced resources distinct from top-level ones"
    ;; The wrong-relation case: a top-level UsersController must relate to
    ;; the top-level User model, not to Admin::User, and vice versa.
    (let ((files '("app/models/user.rb"
                   "app/models/admin/user.rb"
                   "app/controllers/users_controller.rb"
                   "app/controllers/admin/users_controller.rb"
                   "app/views/users/index.html.erb"
                   "app/views/admin/users/index.html.erb")))
      (expect (projectile--related-file-candidates
               "app/controllers/users_controller.rb"
               projectile--rails-file-kinds files)
              :to-equal '((:model . "app/models/user.rb")
                          (:view . "app/views/users/index.html.erb")))
      (expect (projectile--related-file-candidates
               "app/controllers/admin/users_controller.rb"
               projectile--rails-file-kinds files)
              :to-equal '((:model . "app/models/admin/user.rb")
                          (:view . "app/views/admin/users/index.html.erb")))))

  (it "does not crash on a key-fn that errors, and treats it as no match"
    (let ((kinds '((:a :suffix ".rb"
                    :key-fn (lambda (_) (error "boom")))
                   (:b :suffix ".txt")))
          (files '("foo.rb" "foo.txt")))
      (expect (projectile--related-file-candidates "foo.txt" kinds files)
              :to-equal nil)))

  (it "ignores a key-fn that returns a non-string"
    (let ((kinds '((:a :suffix ".rb" :key-fn (lambda (_) 42))
                   (:b :suffix ".txt" :key-fn (lambda (_) 42))))
          (files '("foo.rb" "foo.txt")))
      ;; both return 42, but non-string keys must not relate them
      (expect (projectile--related-file-candidates "foo.txt" kinds files)
              :to-equal nil))))

(describe "projectile--related-file-ring"
  (it "lists one file per matching kind in table order, stably"
    (let ((files '("app/models/user.rb"
                   "app/controllers/users_controller.rb"
                   "app/views/users/index.html.erb")))
      ;; the ring is the same regardless of which file we start from, so
      ;; advancing from any position cycles the kinds deterministically
      (expect (projectile--related-file-ring
               "app/controllers/users_controller.rb"
               projectile--rails-file-kinds files)
              :to-equal '("app/models/user.rb"
                          "app/controllers/users_controller.rb"
                          "app/views/users/index.html.erb"))
      (expect (projectile--related-file-ring
               "app/models/user.rb"
               projectile--rails-file-kinds files)
              :to-equal '("app/models/user.rb"
                          "app/controllers/users_controller.rb"
                          "app/views/users/index.html.erb")))))

(describe "projectile-find-file-of-kind"
  (it "completes only over files of the chosen kind"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("app/models/user.rb"
           "app/controllers/users_controller.rb"
           "app/controllers/products_controller.rb"
           "app/views/users/index.html.erb"
           "config/routes.rb")
          (:file-kinds projectile--rails-file-kinds)
        (let (captured)
          (spy-on 'projectile-completing-read
                  :and-call-fake (lambda (_prompt choices &rest _)
                                   (setq captured choices)
                                   nil))
          (projectile--find-file-of-kind (assq :controller projectile--rails-file-kinds))
          (expect captured
                  :to-have-same-items-as '("app/controllers/products_controller.rb"
                                           "app/controllers/users_controller.rb")))))))

(describe "projectile-toggle-related-file"
  (it "opens the single counterpart immediately"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("app/controllers/users_controller.rb"
           "app/models/user.rb")
          (:file-kinds projectile--rails-file-kinds)
        (let ((root (file-truename (expand-file-name "project/"))))
          (spy-on 'find-file)
          (cl-letf (((symbol-function 'buffer-file-name)
                     (lambda (&optional _)
                       (expand-file-name "app/controllers/users_controller.rb" root))))
            (setq last-command nil this-command 'projectile-toggle-related-file)
            (projectile-toggle-related-file)
            (expect 'find-file :to-have-been-called-with
                    (expand-file-name "app/models/user.rb" root)))))))
  (it "resolves the visited file's symlinks against the project root"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("app/controllers/users_controller.rb"
           "app/models/user.rb")
          (:file-kinds projectile--rails-file-kinds)
        (let* ((root (file-truename (expand-file-name "project/")))
               ;; the current file reached through a symlinked project root, so
               ;; `buffer-file-name' is un-resolved while the root is resolved
               (link-file (expand-file-name
                           "app/controllers/users_controller.rb"
                           (file-name-as-directory (expand-file-name "linkproj")))))
          (make-symbolic-link "project" "linkproj")
          (spy-on 'find-file)
          (cl-letf (((symbol-function 'buffer-file-name)
                     (lambda (&optional _) link-file)))
            (setq last-command nil this-command 'projectile-toggle-related-file)
            ;; without truename'ing the file this raises "No related files"
            (projectile-toggle-related-file)
            (expect 'find-file :to-have-been-called-with
                    (expand-file-name "app/models/user.rb" root)))))))
  (it "cycles through several related kinds on repeated invocation"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("app/controllers/users_controller.rb"
           "app/models/user.rb"
           "app/views/users/index.html.erb")
          (:file-kinds projectile--rails-file-kinds)
        (let* ((root (file-truename (expand-file-name "project/")))
               (current "app/controllers/users_controller.rb"))
          (spy-on 'find-file :and-call-fake
                  (lambda (path)
                    (setq current (file-relative-name path root))))
          (cl-letf (((symbol-function 'buffer-file-name)
                     (lambda (&optional _) (expand-file-name current root))))
            ;; simulate repeated presses of the command
            ;; ring is (model controller view) in table order; starting on
            ;; the controller, repeated presses advance through it and wrap
            (setq this-command 'projectile-toggle-related-file
                  last-command 'projectile-toggle-related-file)
            (projectile-toggle-related-file)
            (expect current :to-equal "app/views/users/index.html.erb")
            (projectile-toggle-related-file)
            (expect current :to-equal "app/models/user.rb")
            (projectile-toggle-related-file)
            (expect current :to-equal "app/controllers/users_controller.rb")))))))

(describe "projectile-related-files-fn with :file-kinds"
  (it "merges a hand-written related-files-fn with the compiled kinds fn"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("app/models/user.rb"
           "app/controllers/users_controller.rb"
           "doc/users.md")
          (:file-kinds projectile--rails-file-kinds
           :related-files-fn (lambda (f)
                               (when (string-prefix-p "app/controllers/" f)
                                 (list :doc "doc/users.md"))))
        ;; both fns are present, so a list is returned
        (expect (listp (projectile-related-files-fn 'sample-project)) :to-be-truthy)
        (let ((kinds (projectile--related-files-kinds
                      "app/controllers/users_controller.rb")))
          ;; declarative (:model) and hand-written (:doc) relations coexist
          (expect kinds :to-contain :model)
          (expect kinds :to-contain :doc)))))
  (it "leaves a :related-files-fn-only type unaffected"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/foo.c" "doc/foo.txt")
          (:related-files-fn (lambda (f)
                               (when (equal f "src/foo.c")
                                 (list :doc "doc/foo.txt"))))
        ;; no :file-kinds, so the fn is returned verbatim (not wrapped)
        (expect (functionp (projectile-related-files-fn 'sample-project)) :to-be-truthy)
        (expect (projectile--related-files-kinds "src/foo.c") :to-equal '(:doc))))))

;;; projectile-file-kinds-test.el ends here
