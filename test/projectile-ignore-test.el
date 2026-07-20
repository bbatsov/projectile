;;; projectile-ignore-test.el --- Tests for ignored files/directories and .projectile parsing -*- lexical-binding: t -*-

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

;; Tests for ignored files/directories and .projectile parsing.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-ignored-directory-p"
  (it "checks if directory should be ignored"
    (spy-on 'projectile-ignored-directories :and-return-value '("/path/to/project/tmp" "/path/to/project/t.ignore"))
    (expect (projectile-ignored-directory-p "/path/to/project/tmp") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/t.ignore") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/log") :not :to-be-truthy)))

(describe "projectile-ignored-file-p"
  (it "checks if file should be ignored"
    (spy-on 'projectile-ignored-files :and-return-value '("/path/to/project/TAGS" "/path/to/project/T.*"))
    (expect (projectile-ignored-file-p "/path/to/project/TAGS") :to-be-truthy)
    (expect (projectile-ignored-file-p "/path/to/project/foo.el") :not :to-be-truthy)))

(describe "projectile-globally-ignored-files"
  (it "includes TAGS file by default"
    (expect (member projectile-tags-file-name projectile-globally-ignored-files) :to-be-truthy))
  (it "includes cache file by default"
    (expect (member projectile-cache-file projectile-globally-ignored-files) :to-be-truthy))
  (it "causes cache file to be ignored via projectile-ignored-file-p"
    (spy-on 'projectile-ignored-files :and-return-value
            (list (concat "/path/to/project/" projectile-cache-file)
                  (concat "/path/to/project/" projectile-tags-file-name)))
    (expect (projectile-ignored-file-p (concat "/path/to/project/" projectile-cache-file)) :to-be-truthy)
    (expect (projectile-ignored-file-p "/path/to/project/source.el") :not :to-be-truthy)))

(describe "projectile-globally-ignored-files :safe predicate"
  (it "accepts list of strings as safe"
    (let ((pred (get 'projectile-globally-ignored-files 'safe-local-variable)))
      (expect (funcall pred '("file1" "file2")) :to-be-truthy)))
  (it "rejects list containing non-strings as unsafe"
    (let ((pred (get 'projectile-globally-ignored-files 'safe-local-variable)))
      (expect (funcall pred '("file1" 123)) :not :to-be-truthy)))
  (it "accepts empty list as safe"
    (let ((pred (get 'projectile-globally-ignored-files 'safe-local-variable)))
      (expect (funcall pred '()) :to-be-truthy)))
  (it "rejects non-list as unsafe"
    (let ((pred (get 'projectile-globally-ignored-files 'safe-local-variable)))
      (expect (funcall pred "not-a-list") :not :to-be-truthy))))

(describe "safe-local-variable predicates for project settings"
  (it "accepts strings for string-valued project settings"
    (dolist (var '(projectile-project-test-suffix
                   projectile-project-test-prefix
                   projectile-project-src-dir
                   projectile-project-test-dir
                   projectile-project-configure-cmd
                   projectile-project-compilation-cmd
                   projectile-project-compilation-dir
                   projectile-project-test-cmd
                   projectile-project-install-cmd
                   projectile-project-package-cmd
                   projectile-project-run-cmd))
      (expect (funcall (get var 'safe-local-variable) "some-value") :to-be-truthy)))
  (it "rejects non-strings for string-valued project settings"
    (dolist (var '(projectile-project-test-suffix
                   projectile-project-compilation-cmd
                   projectile-project-run-cmd))
      (expect (funcall (get var 'safe-local-variable) 123) :not :to-be-truthy)
      (expect (funcall (get var 'safe-local-variable) '("list")) :not :to-be-truthy)))
  (it "accepts booleans for projectile-project-enable-cmd-caching"
    (let ((pred (get 'projectile-project-enable-cmd-caching 'safe-local-variable)))
      (expect (funcall pred t) :to-be-truthy)
      (expect (funcall pred nil) :to-be-truthy)))
  (it "rejects non-booleans for projectile-project-enable-cmd-caching"
    (let ((pred (get 'projectile-project-enable-cmd-caching 'safe-local-variable)))
      (expect (funcall pred "yes") :not :to-be-truthy)
      (expect (funcall pred 1) :not :to-be-truthy)))
  (it "accepts symbols for projectile-project-type"
    (let ((pred (get 'projectile-project-type 'safe-local-variable)))
      (expect (funcall pred 'maven) :to-be-truthy)
      (expect (funcall pred 'generic) :to-be-truthy)))
  (it "rejects non-symbols for projectile-project-type"
    (let ((pred (get 'projectile-project-type 'safe-local-variable)))
      (expect (funcall pred "maven") :not :to-be-truthy))))

(describe "projectile-ignored-files"
  (it "returns list of ignored files"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-project-name :and-return-value "project")
    (spy-on 'projectile-project-ignored-files :and-return-value '("foo.js" "bar.rb"))
    (let ((files '("/path/to/project/TAGS"
                   "/path/to/project/foo.js"
                   "/path/to/project/bar.rb"
                   "/path/to/project/file1.log"
                   "/path/to/project/file2.log"))
          (projectile-ignored-files '("TAGS" "file\d+\\.log")))
      (expect (projectile-ignored-files) :not :to-equal files)
      (expect (projectile-ignored-files) :to-equal '("/path/to/project/TAGS"
                                                     "/path/to/project/.projectile-cache.eld"
                                                     "/path/to/project/foo.js"
                                                     "/path/to/project/bar.rb")))))

(describe "projectile-ignored-directories"
  (it "returns list of ignored directories"
    (spy-on 'projectile-project-ignored-directories :and-return-value '("tmp" "log"))
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (let ((paths '("/path/to/project/compiled/"
                   "/path/to/project/ignoreme"
                   "/path/to/project/ignoremetoo"
                   "/path/to/project/tmp"
                   "/path/to/project/log"))
          (projectile-globally-ignored-directories '("compiled" "ignoreme")))
      (expect (projectile-ignored-directories) :not :to-equal paths)
      (expect (projectile-ignored-directories) :to-equal '("/path/to/project/compiled/"
                                                           "/path/to/project/ignoreme/"
                                                           "/path/to/project/tmp/"
                                                           "/path/to/project/log/")))))

(describe "projectile-project-ignored-files"
  (it "returns list of project ignored files"
    (let ((files '("/path/to/project/foo.el" "/path/to/project/foo.elc")))
      (spy-on 'projectile-project-ignored :and-return-value files)
      (spy-on 'file-directory-p :and-return-value nil)
      (expect (projectile-project-ignored-files) :to-equal files)
      (spy-on 'file-directory-p :and-return-value t)
      (expect (projectile-project-ignored-files) :not :to-be-truthy))))

(describe "projectile-project-ignored-directories"
  (it "returns list of project ignored directories"
    (let ((directories '("/path/to/project/tmp" "/path/to/project/log")))
      (spy-on 'projectile-project-ignored :and-return-value directories)
      (spy-on 'file-directory-p :and-return-value t)
      (expect (projectile-project-ignored-directories) :to-equal directories)
      (spy-on 'file-directory-p :and-return-value nil)
      (expect (projectile-project-ignored-directories) :not :to-be-truthy))))

(describe "projectile-project-ignored"
  (it "returns list of ignored files/directories"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-project-name :and-return-value "project")
    (spy-on 'projectile-paths-to-ignore :and-return-value (list "log" "tmp" "compiled"))
    (spy-on 'file-expand-wildcards :and-call-fake
            (lambda (pattern ignored)
              (cond
               ((string-equal pattern "log") "/path/to/project/log")
               ((string-equal pattern "tmp") "/path/to/project/tmp")
               ((string-equal pattern "compiled") "/path/to/project/compiled"))))
    (let* ((file-names '("log" "tmp" "compiled"))
           (files (mapcar 'projectile-expand-root file-names)))
      (expect (projectile-project-ignored) :to-equal files))))

(describe "projectile-add-unignored"
  (it "requires explicitly unignoring files inside ignored paths"
    (spy-on 'projectile-get-repo-ignored-files :and-return-value '("unignored-file" "path/unignored-file2"))
    (let ((projectile-globally-unignored-files '("unignored-file")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "unignored-file")))
    (let ((projectile-globally-unignored-files '("unignored-file" "path/unignored-file2")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "unignored-file" "path/unignored-file2"))))
  (it "returns the list of globally unignored files on an unsupported VCS"
    (spy-on 'projectile-project-vcs :and-return-value 'none)
    (let ((projectile-globally-unignored-files '("unignored-file")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file"))))
  (it "requires explicitly unignoring ignored files inside unignored paths"
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (spy-on 'projectile-get-repo-ignored-files :and-return-value '("path/unignored-file"))
    (spy-on 'projectile-get-repo-ignored-directory :and-call-fake
            (lambda (project dir vcs)
              (list (concat dir "unignored-file"))))
    (let ((projectile-globally-unignored-directories '("path")))
      (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "path/unignored-file"))
      (let ((projectile-globally-ignored-files '("unignored-file")))
        (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file"))
        (let ((projectile-globally-unignored-files '("path/unignored-file")))
          (expect (projectile-add-unignored nil nil '("file")) :to-equal '("file" "path/unignored-file")))))))

(describe "projectile--dirconfig-classify-line"
  (it "returns nil for blank or whitespace-only lines"
    (expect (projectile--dirconfig-classify-line "") :to-be nil)
    (expect (projectile--dirconfig-classify-line "   ") :to-be nil)
    (expect (projectile--dirconfig-classify-line "\t") :to-be nil))
  (it "classifies prefix dispatches"
    (expect (projectile--dirconfig-classify-line "+/src")
            :to-equal '(:keep . "/src"))
    (expect (projectile--dirconfig-classify-line "-/build")
            :to-equal '(:ignore . "/build"))
    (expect (projectile--dirconfig-classify-line "!/build/keepme")
            :to-equal '(:ensure . "/build/keepme")))
  (it "tags prefix-less lines as legacy-ignore for backward compatibility"
    (expect (projectile--dirconfig-classify-line "stale-pattern")
            :to-equal '(:legacy-ignore . "stale-pattern")))
  (it "skips leading whitespace before dispatch"
    (expect (projectile--dirconfig-classify-line "  -indented")
            :to-equal '(:ignore . "indented"))
    (expect (projectile--dirconfig-classify-line "\t+keep")
            :to-equal '(:keep . "keep")))
  (it "honors the comment prefix when configured"
    (let ((projectile-dirconfig-comment-prefix ?#))
      (expect (projectile--dirconfig-classify-line "# a comment")
              :to-equal '(:comment))
      (expect (projectile--dirconfig-classify-line "  # indented comment")
              :to-equal '(:comment)))
    ;; Without a comment prefix, # is just a regular character.
    (let ((projectile-dirconfig-comment-prefix nil))
      (expect (projectile--dirconfig-classify-line "#may-be-a-comment")
              :to-equal '(:legacy-ignore . "#may-be-a-comment")))))

(describe "projectile-parse-dirconfig-file"
  (before-each
    (clrhash projectile--dirconfig-cache)
    (clrhash projectile--glob-keep-warned-projects)
    (clrhash projectile--prefixless-dirconfig-warned-projects))
  (it "parses dirconfig and returns directories to ignore and keep"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'file-truename :and-call-fake (lambda (filename) filename))
    (spy-on 'insert-file-contents :and-call-fake
            (lambda (filename)
              (save-excursion (insert "\n-exclude\n+include\n#may-be-a-comment\nno-prefix\n left-wspace\nright-wspace\t\n"))))
    (expect (projectile-parse-dirconfig-file)
            :to-equal (make-projectile-dirconfig
                       :keep '("include/")
                       :ignore '("exclude"
                                 "#may-be-a-comment"
                                 "no-prefix"
                                 "left-wspace"
                                 "right-wspace")
                       :prefixless-ignore '("#may-be-a-comment"
                                            "no-prefix"
                                            "left-wspace"
                                            "right-wspace")))
    ;; same test - but with comment lines enabled using prefix '#'
    (let ((projectile-dirconfig-comment-prefix ?#))
      (expect (projectile-parse-dirconfig-file)
              :to-equal (make-projectile-dirconfig
                         :keep '("include/")
                         :ignore '("exclude"
                                   "no-prefix"
                                   "left-wspace"
                                   "right-wspace")
                         :prefixless-ignore '("no-prefix"
                                              "left-wspace"
                                              "right-wspace")))))
  (it "skips leading whitespace before dispatching on the prefix"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'insert-file-contents :and-call-fake
            (lambda (_filename)
              (save-excursion
                (insert "  -indented-exclude\n"
                        "\t+indented-include\n"
                        " !indented-ensure\n"
                        "  no-prefix-indented\n"))))
    (expect (projectile-parse-dirconfig-file)
            :to-equal (make-projectile-dirconfig
                       :keep '("indented-include/")
                       :ignore '("indented-exclude" "no-prefix-indented")
                       :ensure '("indented-ensure")
                       :prefixless-ignore '("no-prefix-indented"))))
  (it "treats indented comment-prefix lines as comments"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'insert-file-contents :and-call-fake
            (lambda (_filename)
              (save-excursion
                (insert "  # indented comment\n"
                        "-keep-this\n"))))
    (let ((projectile-dirconfig-comment-prefix ?#))
      (expect (projectile-parse-dirconfig-file)
              :to-equal (make-projectile-dirconfig :ignore '("keep-this")))))
  (it "warns once per project even when multiple + entries contain globs"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'insert-file-contents :and-call-fake
            (lambda (_filename)
              (save-excursion (insert "+/*.json\n+/src\n+/[abc]/lib\n"))))
    (spy-on 'display-warning)
    (projectile-parse-dirconfig-file)
    (projectile-parse-dirconfig-file)
    (expect 'display-warning :to-have-been-called-times 1))
  (it "does not warn for plain + subdirectory entries"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'insert-file-contents :and-call-fake
            (lambda (_filename)
              (save-excursion (insert "+/src\n+/tests/foo\n"))))
    (spy-on 'display-warning)
    (projectile-parse-dirconfig-file)
    (expect 'display-warning :not :to-have-been-called))
  (it "does not warn for - ignore entries that contain globs"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'insert-file-contents :and-call-fake
            (lambda (_filename)
              (save-excursion (insert "-*.json\n-build/*.tmp\n"))))
    (spy-on 'display-warning)
    (projectile-parse-dirconfig-file)
    (expect 'display-warning :not :to-have-been-called)))

(describe "projectile-parse-dirconfig-file with a real file"
  (before-each
    (clrhash projectile--dirconfig-cache))
  (it "parses a mix of keep, ignore, ensure and unprefixed entries from disk"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "+/src\n"
                  "-/build\n"
                  "!/build/keepme\n"
                  "stale-pattern\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (expect (projectile-parse-dirconfig-file)
                :to-equal (make-projectile-dirconfig
                           :keep '("/src/")
                           :ignore '("/build" "stale-pattern")
                           :ensure '("/build/keepme")
                           :prefixless-ignore '("stale-pattern")))))))
  (it "round-trips non-ASCII paths through the parser"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root))
            (coding-system-for-write 'utf-8-unix))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-héllo/wörld\n"
                  "+/プロジェクト\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (expect (projectile-parse-dirconfig-file)
                :to-equal (make-projectile-dirconfig
                           :keep '("/プロジェクト/")
                           :ignore '("héllo/wörld")))))))
  (it "tolerates a trailing line without a final newline"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-foo\n-bar"))
        (spy-on 'projectile-project-root :and-return-value root)
        (expect (projectile-dirconfig-ignore (projectile-parse-dirconfig-file))
                :to-equal '("foo" "bar")))))))

(describe "dirconfig cache"
  (before-each
    (clrhash projectile--dirconfig-cache))
  (it "memoizes the parsed result while the file is unchanged"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-foo\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (spy-on 'projectile--parse-dirconfig-file-uncached
                :and-call-through)
        (projectile-parse-dirconfig-file)
        (projectile-parse-dirconfig-file)
        (projectile-parse-dirconfig-file)
        (expect 'projectile--parse-dirconfig-file-uncached
                :to-have-been-called-times 1)))))
  (it "re-parses when the dirconfig file's mtime changes"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let* ((root (projectile-test-project-root))
             (dirconfig (expand-file-name ".projectile" root)))
        (with-temp-file dirconfig (insert "-foo\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (expect (projectile-dirconfig-ignore (projectile-parse-dirconfig-file))
                :to-equal '("foo"))
        ;; Force a distinct mtime — file-attribute-modification-time has
        ;; second-level resolution on some filesystems.
        (set-file-times dirconfig (time-add (current-time) 5))
        (with-temp-file dirconfig (insert "-bar\n"))
        (set-file-times dirconfig (time-add (current-time) 5))
        (expect (projectile-dirconfig-ignore (projectile-parse-dirconfig-file))
                :to-equal '("bar"))))))
  (it "returns nil and does not cache when the dirconfig file is absent"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/")
      (let ((root (projectile-test-project-root)))
        (spy-on 'projectile-project-root :and-return-value root)
        (expect (projectile-parse-dirconfig-file) :to-be nil)
        (expect (gethash root projectile--dirconfig-cache) :to-be nil)))))
  (it "is cleared for the project by projectile-invalidate-cache"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-foo\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        ;; Avoid touching the on-disk cache file or recentf during the test.
        (spy-on 'projectile-persistent-cache-p :and-return-value nil)
        (spy-on 'recentf-cleanup)
        (projectile-parse-dirconfig-file)
        (expect (gethash root projectile--dirconfig-cache) :not :to-be nil)
        (projectile-invalidate-cache nil)
        (expect (gethash root projectile--dirconfig-cache) :to-be nil)))))
  (it "re-parses when projectile-dirconfig-file points to a different file"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-foo\n"))
        (with-temp-file (expand-file-name ".projectile-alt" root)
          (insert "-bar\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (let ((projectile-dirconfig-file ".projectile"))
          (expect (projectile-dirconfig-ignore (projectile-parse-dirconfig-file))
                  :to-equal '("foo")))
        (let ((projectile-dirconfig-file ".projectile-alt"))
          (expect (projectile-dirconfig-ignore (projectile-parse-dirconfig-file))
                  :to-equal '("bar"))))))))

(describe "prefix-less dirconfig warning"
  (before-each
    (clrhash projectile--dirconfig-cache)
    (clrhash projectile--prefixless-dirconfig-warned-projects))
  (it "warns once when a dirconfig contains prefix-less ignore lines"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-foo\nstale-pattern\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (spy-on 'display-warning)
        (let ((projectile-warn-on-prefixless-dirconfig-lines t))
          (projectile-parse-dirconfig-file)
          (projectile-parse-dirconfig-file))
        (expect 'display-warning :to-have-been-called-times 1)))))
  (it "does not warn for a fully-prefixed dirconfig"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "-foo\n+/src\n!/build/keep\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (spy-on 'display-warning)
        (let ((projectile-warn-on-prefixless-dirconfig-lines t))
          (projectile-parse-dirconfig-file))
        (expect 'display-warning :not :to-have-been-called)))))
  (it "does not warn when the option is disabled"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/.projectile")
      (let ((root (projectile-test-project-root)))
        (with-temp-file (expand-file-name ".projectile" root)
          (insert "stale-pattern\n"))
        (spy-on 'projectile-project-root :and-return-value root)
        (spy-on 'display-warning)
        (let ((projectile-warn-on-prefixless-dirconfig-lines nil))
          (projectile-parse-dirconfig-file))
        (expect 'display-warning :not :to-have-been-called))))))

(describe "projectile--ignored-file-fast-p"
  (it "matches global ignore patterns and suffixes case-sensitively"
    ;; `string-match-p' and `string-suffix-p' both fold case by default
    (let ((rules (projectile--make-walk-rules nil nil nil))
          (projectile-global-ignore-file-patterns '("build"))
          (projectile-globally-ignored-file-suffixes '(".elc")))
      (expect (projectile--ignored-file-fast-p "/r/build/a.o" rules) :to-be-truthy)
      (expect (projectile--ignored-file-fast-p "/r/BUILD/a.o" rules) :not :to-be-truthy)
      (expect (projectile--ignored-file-fast-p "/r/a.elc" rules) :to-be-truthy)
      (expect (projectile--ignored-file-fast-p "/r/a.ELC" rules) :not :to-be-truthy)))

  (it "returns t for files in the pre-computed ignored-files-set"
    (let ((rules (projectile--make-walk-rules
                  '("/r/TAGS") nil nil)))
      (expect (projectile--ignored-file-fast-p "/r/TAGS" rules) :to-be-truthy)
      (expect (projectile--ignored-file-fast-p "/r/keep.el" rules)
              :not :to-be-truthy)))
  (it "honors projectile-globally-ignored-file-suffixes"
    (let ((rules (projectile--make-walk-rules nil nil nil))
          (projectile-globally-ignored-file-suffixes '(".elc")))
      (expect (projectile--ignored-file-fast-p "/r/foo.elc" rules) :to-be-truthy)
      (expect (projectile--ignored-file-fast-p "/r/foo.el" rules)
              :not :to-be-truthy)))
  (it "honors projectile-global-ignore-file-patterns"
    (let ((rules (projectile--make-walk-rules nil nil nil))
          (projectile-global-ignore-file-patterns '("\\.min\\.js\\'")))
      (expect (projectile--ignored-file-fast-p "/r/foo.min.js" rules) :to-be-truthy)
      (expect (projectile--ignored-file-fast-p "/r/foo.js" rules)
              :not :to-be-truthy))))

(describe "projectile--ignored-directory-fast-p"
  (it "matches absolute ignored-dirs entries"
    (let ((rules (projectile--make-walk-rules
                  nil '("/r/build/") nil)))
      (expect (projectile--ignored-directory-fast-p "/r/build/" "build" rules)
              :to-be-truthy)))
  (it "matches globally ignored directory basenames"
    (let ((rules (projectile--make-walk-rules nil nil '(".git"))))
      (expect (projectile--ignored-directory-fast-p "/r/.git/" ".git" rules)
              :to-be-truthy)
      (expect (projectile--ignored-directory-fast-p "/r/src/" "src" rules)
              :not :to-be-truthy))))

(describe "projectile-remove-ignored"
  (it "removes files whose suffix matches projectile-globally-ignored-file-suffixes"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-project-name :and-return-value "project")
    (spy-on 'projectile-ignored-files-rel)
    (spy-on 'projectile-ignored-directories-rel)
    (let* ((file-names '("foo.c" "foo.o" "foo.so" "foo.o.gz" "foo.tar.gz" "foo.tar.GZ"))
           (files (mapcar 'projectile-expand-root file-names)))
      (let ((projectile-globally-ignored-file-suffixes '(".o" ".so" ".tar.gz")))
        ;; matching is case-sensitive, so `foo.tar.GZ' survives `.tar.gz'
        (expect (projectile-remove-ignored files)
                :to-equal (mapcar 'projectile-expand-root
                                  '("foo.c" "foo.o.gz" "foo.tar.GZ"))))))

  (it "matches ignored suffixes case-sensitively"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-ignored-files-rel)
    (spy-on 'projectile-ignored-directories-rel)
    (let ((projectile-globally-ignored-file-suffixes '(".elc")))
      (expect (projectile-remove-ignored '("a.elc" "B.ELC" "c.Elc"))
              :to-equal '("B.ELC" "c.Elc"))))

  (it "matches dirconfig ignore patterns case-sensitively"
    (spy-on 'projectile-project-root :and-return-value "/path/to/project")
    (spy-on 'projectile-ignored-files-rel)
    (spy-on 'projectile-ignored-directories-rel)
    (spy-on 'projectile-filtering-patterns :and-return-value '(("*.log") . nil))
    (expect (projectile-remove-ignored '("a.log" "B.LOG"))
            :to-equal '("B.LOG")))
  (it "drops files whose basename matches an ignored entry"
    (spy-on 'projectile-ignored-files-rel :and-return-value '("TAGS"))
    (spy-on 'projectile-ignored-directories-rel :and-return-value nil)
    (expect (projectile-remove-ignored '("a/TAGS" "src/foo.el" "TAGS"))
            :to-equal '("src/foo.el")))
  (it "treats `*'-prefixed entries as any-segment matches"
    (spy-on 'projectile-ignored-files-rel :and-return-value nil)
    (spy-on 'projectile-ignored-directories-rel :and-return-value '("*node_modules/"))
    (expect (projectile-remove-ignored
             '("src/foo.js"
               "node_modules/lib/index.js"
               "vendor/node_modules/lib/index.js"))
            :to-equal '("src/foo.js")))
  (it "treats plain entries as path-prefix matches"
    (spy-on 'projectile-ignored-files-rel :and-return-value nil)
    (spy-on 'projectile-ignored-directories-rel :and-return-value '("build/"))
    (expect (projectile-remove-ignored '("build/foo.o" "src/foo.c" "buildbot/x"))
            :to-equal '("src/foo.c" "buildbot/x"))))

(describe "projectile-ignored-project-p"
  (it "matches abbreviated paths against truename-resolved ignored list"
    (let ((projectile-ignored-projects (list default-directory)))
      (spy-on 'file-truename :and-call-fake
              (lambda (f) (expand-file-name f)))
      (expect (projectile-ignored-project-p
               (abbreviate-file-name default-directory))
              :to-be-truthy)))
  (it "returns nil for non-ignored projects"
    (let ((projectile-ignored-projects '("/some/other/project/")))
      (expect (projectile-ignored-project-p "/my/project/")
              :not :to-be-truthy)))
  (it "skips file-truename for remote project paths"
    ;; `file-truename' on a TRAMP path is a remote stat; the
    ;; find-file-hook's known-projects tracker calls
    ;; `projectile-ignored-project-p' for every visit, so it must
    ;; not round-trip just to canonicalize symlinks.
    (spy-on 'file-truename :and-call-fake (lambda (f) f))
    (let ((projectile-ignored-projects '("/ssh:host:/proj/")))
      (expect (projectile-ignored-project-p "/ssh:host:/proj/")
              :to-be-truthy))
    (expect 'file-truename :not :to-have-been-called)))

(describe "projectile-keep-project-p"
  (it "keeps remote projects without stat'ing them"
    ;; Both connected and disconnected remote paths must be kept
    ;; without a `file-readable-p' round-trip.  Previously the
    ;; connected branch issued one stat per remote project, which
    ;; turned `projectile--cleanup-known-projects' into a serial
    ;; remote walk on every project switch.
    (spy-on 'file-readable-p :and-return-value nil)
    (expect (projectile-keep-project-p "/ssh:host:/proj/") :to-be-truthy)
    (expect 'file-readable-p :not :to-have-been-called))
  (it "stat's local projects to detect deletion"
    (let ((real-readable (symbol-function 'file-readable-p)))
      (spy-on 'file-readable-p :and-call-fake real-readable))
    (expect (projectile-keep-project-p temporary-file-directory) :to-be-truthy)
    (expect (projectile-keep-project-p "/this/path/should/not/exist/")
            :not :to-be-truthy)))

(describe "projectile-test-ignored-directory-p"
  (it "ignores specified directory values"
    (spy-on 'projectile-ignored-directories :and-return-value '("/path/to/project/tmp"))
    (expect (projectile-ignored-directory-p "/path/to/project/tmp") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/log") :not :to-be-truthy))
  (it "ignores specified directory values with characters that need to be escaped"
    (spy-on 'projectile-ignored-directories :and-return-value '("/path/to/project/.dir"))
    (expect (projectile-ignored-directory-p "/path/to/project/.dir") :to-be-truthy)
    (expect (projectile-ignored-directory-p "/path/to/project/log") :not :to-be-truthy)))

(describe "project-ignores"
  (it "returns ignore globs in the format expected by project.el"
    (let ((root (file-truename (expand-file-name "/my/root/")))
          (projectile-globally-ignored-files '("TAGS" ".#foo"))
          (projectile-globally-ignored-file-suffixes '(".elc" ".o")))
      (spy-on 'projectile-globally-ignored-directory-names
              :and-return-value '(".git" ".svn/"))
      (spy-on 'projectile-patterns-to-ignore :and-return-value '("*.log"))
      (spy-on 'projectile-project-ignored-directories
              :and-return-value (list (concat root "build/")
                                      (concat root "dist/")))
      (spy-on 'projectile-project-ignored-files
              :and-return-value (list (concat root "TODO")))
      (let ((ignores (project-ignores (cons 'projectile root) root)))
        ;; globally ignored directory names match at any depth
        (expect (member ".git/" ignores) :to-be-truthy)
        (expect (member ".svn/" ignores) :to-be-truthy)
        ;; globally ignored files match at any depth
        (expect (member "TAGS" ignores) :to-be-truthy)
        ;; suffixes become globs
        (expect (member "*.elc" ignores) :to-be-truthy)
        ;; dirconfig patterns are passed through
        (expect (member "*.log" ignores) :to-be-truthy)
        ;; dirconfig ignored directories are rooted and end with a slash
        (expect (member "./build/" ignores) :to-be-truthy)
        (expect (member "./dist/" ignores) :to-be-truthy)
        ;; dirconfig ignored files are rooted
        (expect (member "./TODO" ignores) :to-be-truthy)))))

(describe "projectile-normalise-patterns"
  (it "drops absolute (path) patterns and keeps relative globs"
    (expect (projectile-normalise-patterns '("/abs/path" "rel" "/x" "glob*"))
            :to-equal '("rel" "glob*")))
  (it "returns an empty list when every pattern is a path"
    (expect (projectile-normalise-patterns '("/a" "/b")) :to-equal nil)))

(describe "projectile--dirconfig-pattern-to-regexp"
  (cl-flet ((matches (pattern path)
              (string-match-p (projectile--compile-dirconfig-patterns
                               (list pattern))
                              path)))
    (it "matches a slashless glob against the file name anywhere"
      (expect (matches "*.text" "foo.text") :to-be-truthy)
      (expect (matches "*.text" "mysubdir/y.text") :to-be-truthy)
      (expect (matches "*.text" "foo.text.bak") :not :to-be-truthy))
    (it "matches a slashless name against any directory segment"
      (expect (matches "vendor" "vendor/foo.js") :to-be-truthy)
      (expect (matches "vendor" "a/vendor/foo.js") :to-be-truthy)
      (expect (matches "vendor" "vendor") :to-be-truthy)
      (expect (matches "vendor" "myvendor/foo.js") :not :to-be-truthy)
      (expect (matches "vendor" "vendored/foo.js") :not :to-be-truthy))
    (it "restricts trailing-slash patterns to directories"
      (expect (matches "vendor/" "vendor/foo.js") :to-be-truthy)
      (expect (matches "vendor/" "a/vendor/foo.js") :to-be-truthy)
      (expect (matches "vendor/" "vendor") :not :to-be-truthy)
      (expect (matches "lib/" "lib/foo.c") :to-be-truthy))
    (it "anchors patterns containing a slash at the project root"
      (expect (matches "src/*.c" "src/a.c") :to-be-truthy)
      (expect (matches "src/*.c" "src/a/b.c") :not :to-be-truthy)
      (expect (matches "src/*.c" "x/src/a.c") :not :to-be-truthy)
      (expect (matches "build/output/" "build/output/x.o") :to-be-truthy)
      (expect (matches "build/output/" "x/build/output/x.o") :not :to-be-truthy))
    (it "treats a matched anchored name as a subtree"
      (expect (matches "src/gen" "src/gen") :to-be-truthy)
      (expect (matches "src/gen" "src/gen/deep/file.c") :to-be-truthy)
      (expect (matches "src/gen" "src/gen.c") :not :to-be-truthy))
    (it "stops `*' at slashes and lets `**' span them"
      (expect (matches "src/*" "src/a.c") :to-be-truthy)
      ;; `src/*' matches the directory src/a, and a matched directory
      ;; covers its subtree - same as .gitignore.
      (expect (matches "src/*" "src/a/b.c") :to-be-truthy)
      (expect (matches "src/*.c" "src/a/b.c") :not :to-be-truthy)
      (expect (matches "src/**" "src/a/b.c") :to-be-truthy)
      (expect (matches "**/gen/*.c" "a/b/gen/x.c") :to-be-truthy)
      (expect (matches "**/gen/*.c" "gen/x.c") :to-be-truthy))
    (it "supports `?' and character classes"
      (expect (matches "file?.c" "file1.c") :to-be-truthy)
      (expect (matches "file?.c" "file10.c") :not :to-be-truthy)
      (expect (matches "file[0-9].c" "file7.c") :to-be-truthy)
      (expect (matches "file[!0-9].c" "filex.c") :to-be-truthy)
      (expect (matches "file[!0-9].c" "file7.c") :not :to-be-truthy))
    (it "quotes regexp metacharacters in literal parts"
      (expect (matches "a+b.c" "a+b.c") :to-be-truthy)
      (expect (matches "a+b.c" "aab.c") :not :to-be-truthy))))

(describe "projectile--compile-dirconfig-patterns"
  (it "returns nil for no patterns"
    (expect (projectile--compile-dirconfig-patterns nil) :to-be nil))
  (it "matches when any of several patterns matches"
    (let ((re (projectile--compile-dirconfig-patterns '("*.o" "vendor/"))))
      (expect (string-match-p re "x/y.o") :to-be-truthy)
      (expect (string-match-p re "vendor/z.js") :to-be-truthy)
      (expect (string-match-p re "src/main.c") :not :to-be-truthy))))

;;; projectile-ignore-test.el ends here
