;;; projectile-grep-test.el --- Tests for grep/ripgrep search -*- lexical-binding: t -*-

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

;; Tests for grep/ripgrep search.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-find-references"
  (it "greps the project for the (regexp-quoted) symbol, scoped and honouring ignores"
    (spy-on 'projectile-acquire-root :and-return-value "/my/root/")
    (spy-on 'projectile--project-ignore-globs
            :and-return-value '("*.elc" "/build/"))
    (spy-on 'xref-matches-in-directory :and-return-value nil)
    ;; `xref-show-xrefs' would try to display; run the fetcher and stop.
    (spy-on 'xref-show-xrefs :and-call-fake
            (lambda (fetcher &rest _) (funcall fetcher)))
    (projectile-find-references "foo.bar")
    (expect 'xref-matches-in-directory :to-have-been-called-with
            ;; the gitignore patterns are respelled in project.el's format
            "foo\\.bar" "*" "/my/root/" '("*.elc" "./build/"))))

(describe "projectile--ripgrep-ignore-globs"
  (it "builds unquoted --glob=! exclusions that also work on Windows (#1946)"
    (spy-on 'projectile--dirconfig-ignore :and-return-value '("*.log"))
    (let ((projectile-globally-ignored-files '("TAGS" "GTAGS"))
          (projectile-globally-unignored-files nil)
          (projectile-globally-ignored-file-suffixes nil)
          (projectile-globally-unignored-directories nil)
          (projectile-globally-ignored-directories '(".git" ".svn")))
      (expect (projectile--ripgrep-ignore-globs)
              :to-equal '("--glob=!.git/" "--glob=!.svn/"
                          "--glob=!TAGS" "--glob=!GTAGS"
                          "--glob=!*.log")))))

(describe "projectile--ag-ignore-patterns"
  (it "strips the gitignore markers ag can't make sense of"
    (spy-on 'projectile--dirconfig-ignore
            :and-return-value '("*.log" "/build/" "**/gen" "src/out"))
    (let ((projectile-globally-ignored-directories '("node_modules"))
          (projectile-globally-unignored-directories nil)
          (projectile-globally-ignored-files '("TAGS"))
          (projectile-globally-unignored-files nil)
          (projectile-globally-ignored-file-suffixes '(".elc")))
      (expect (projectile--ag-ignore-patterns)
              :to-equal '("node_modules" "TAGS" "*.elc"
                          "*.log" "build" "gen" "src/out")))))

(describe "the grep and indexing ignore lists"
  ;; The whole point of deriving grep's exclusions from
  ;; `projectile--ignore-patterns' is that the two agree, so assert it
  ;; directly: every pattern indexing filters by has to show up in the
  ;; find expression, on the right side of the anchored/floating split.
  (before-each
    (spy-on 'projectile--dirconfig-ignore
            :and-return-value '("*.log" "/build/" "vendor/" "src/gen" "**/tmp"))
    (spy-on 'projectile--dirconfig-ensure :and-return-value '("keep.log")))
  (it "cover the same patterns"
    (let* ((projectile-globally-ignored-directories '("node_modules"))
           (projectile-globally-unignored-directories nil)
           (projectile-globally-ignored-files '("TAGS"))
           (projectile-globally-unignored-files nil)
           (projectile-globally-ignored-file-suffixes '(".elc"))
           (patterns (projectile--ignore-patterns "/r/"))
           (specs (projectile--grep-find-specs patterns)))
      (expect (car specs) :to-equal '("build" "src/gen"))
      (expect (cdr specs) :to-equal '("node_modules" "TAGS" "*.elc"
                                      "*.log" "vendor" "tmp"))
      ;; nothing indexing knows about is dropped on the way to grep
      (expect (+ (length (car specs)) (length (cdr specs)))
              :to-equal (length patterns))))
  (it "agree on which files a search sees"
    (let* ((projectile-globally-ignored-directories '("node_modules"))
           (projectile-globally-unignored-directories nil)
           (projectile-globally-ignored-files '("TAGS"))
           (projectile-globally-unignored-files nil)
           (projectile-globally-ignored-file-suffixes '(".elc"))
           (files '("a.el" "a.elc" "TAGS" "keep.log" "src/a.log"
                    "build/out" "vendor/dep/a.el" "src/gen/a.el"
                    "node_modules/dep/a.js" "deep/tmp/a.el"))
           (specs (projectile--grep-find-specs (projectile--ignore-patterns "/r/")))
           (ensure (projectile--grep-find-specs (projectile--ensure-patterns "/r/"))))
      (spy-on 'projectile-project-root :and-return-value "/r/")
      (cl-flet* ((path-prefixes (file)
                   ;; `find' prunes a directory, which takes its whole
                   ;; subtree with it, so a test hitting any ancestor of
                   ;; FILE removes FILE too
                   (let (prefixes current)
                     (dolist (segment (split-string file "/") (nreverse prefixes))
                       (setq current (if current (concat current "/" segment) segment))
                       (push current prefixes))))
                 (tested-p (specs file)
                   (let ((globs (append
                                 (mapcar (lambda (p) (concat "./" p)) (car specs))
                                 (mapcar (lambda (p) (concat "*/" p)) (cdr specs)))))
                     (seq-some (lambda (path)
                                 (seq-some (lambda (glob)
                                             (string-match-p (wildcard-to-regexp glob)
                                                             (concat "./" path)))
                                           globs))
                               (path-prefixes file))))
                 (pruned-p (file)
                   (and (tested-p specs file)
                        (not (tested-p ensure file)))))
        (expect (seq-remove #'pruned-p files)
                :to-equal (projectile-remove-ignored files))))))

(describe "projectile-grep"
  (describe "multi-root grep"
    (after-each
      (cl-flet ((grep-buffer-p (b) (string-prefix-p "*grep" (buffer-name b))))
        (let ((grep-buffers (cl-remove-if-not #'grep-buffer-p (buffer-list))))
          (dolist (grep-buffer grep-buffers)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer grep-buffer))))))
    (it "grep multi-root projects"
      (projectile-test-with-sandbox
        (projectile-test-with-files
            ("project/bar/"
             "project/baz/")
          (cd "project")
          (with-temp-file ".projectile" (insert (concat "+/baz\n"
                                                        "+/bar\n")))
          (with-temp-file "foo.txt" (insert "hi"))
          (with-temp-file "bar/bar.txt" (insert "hi"))
          (with-temp-file "baz/baz.txt" (insert "hi"))
          (with-current-buffer (find-file-noselect ".projectile" t)
            (let ((grep-find-template "<X>")
                  grep-find-ignored-directories grep-find-ignored-files
                  projectile-globally-ignored-files
                  projectile-globally-ignored-file-suffixes
                  projectile-globally-ignored-directories)
              (projectile-grep "hi")))))))

  (describe "rgrep"
    (before-each
      (spy-on 'compilation-start))
    (it "excludes global ignores"
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/"
         "project/.projectile")
        (cd "project")
        (with-current-buffer (find-file-noselect ".projectile" t)
          (let ((grep-find-template "<X>")
                (grep-find-ignored-directories '("IG_DIR"))
                (grep-find-ignored-files '("IG_FILE"))
                (projectile-globally-ignored-files '("GLOB_IG_FILE"))
                (projectile-globally-ignored-file-suffixes '("IG_SUF"))
                (projectile-globally-ignored-directories '("GLOB_IG_DIR")))
            (projectile-grep "hi")))
        ;; the projectile clause carries the gitignore patterns: the
        ;; globally ignored directory and file names match at any depth,
        ;; and so do the ignored suffixes
        (expect 'compilation-start :to-have-been-called-with
                (concat "-type d \\( -path \\*/IG_DIR \\) -prune -o "
                        "\\! -type d \\( -name IG_FILE \\) -prune -o "
                        "\\( -path \\*/GLOB_IG_DIR -o -path \\*/GLOB_IG_FILE "
                        "-o -path \\*IG_SUF \\) -prune -o ")
                'grep-mode))))
    (it "excludes project ignores"
      (projectile-test-with-sandbox
       (projectile-test-with-files
        ("project/bar/"
         "project/baz/")
        (cd "project")
        (with-temp-file ".projectile" (insert (concat "-/*.txt\n"
                                                      "-/bar/*.txt\n"
                                                      "-/baz\n"
                                                      "-*.txt\n"
                                                      "-*.text\n"
                                                      "!/abc.txt\n"
                                                      "!/bar/abc.txt\n"
                                                      "!def.txt\n")))
        (with-temp-file "foo.txt")
        (with-temp-file "abc.txt")
        (with-temp-file "bar/foo.txt")
        (with-temp-file "bar/abc.txt")
        (with-current-buffer (find-file-noselect ".projectile" t)
          (let ((grep-find-template "<X>")
                grep-find-ignored-directories grep-find-ignored-files
                projectile-globally-ignored-files
                projectile-globally-ignored-file-suffixes
                projectile-globally-ignored-directories)
            (projectile-grep "hi")))
        ;; the ignore patterns go in as patterns, not as the paths they
        ;; happened to expand to on disk, and the `!' entries are
        ;; subtracted from the whole ignore expression
        (expect 'compilation-start :to-have-been-called-with
                (concat "\\( "
                        "\\( -path ./\\*.txt -o -path ./bar/\\*.txt -o -path ./baz "
                        "-o -path \\*.txt -o -path \\*.text \\) "
                        "-a \\! \\( -path ./abc.txt -o -path ./bar/abc.txt -o -path \\*/def.txt \\) "
                        "\\) -prune -o ")
                'grep-mode)))))
  (it "grep a git project using default files"
    (require 'vc-git)
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("project/c/src/"
       "project/c/include/"
       "project/go/src/package1/"
       "project/.projectile")
      (cd "project")
      (with-temp-file "go/src/package1/x.go" (insert "foo(bar)"))
      (with-temp-file "c/include/x.h" (insert "typedef struct bar_t"))
      (with-temp-file "c/src/x.c" (insert "struct bar_t *x"))
      (dolist (test '(("go/src/package1/x.go" "foo" "*.go")
                      ("c/src/x.c" "bar_t" "*.[ch]")
                      ("c/include/x.h" "bar_t" "*.[ch]")))
        (let ((projectile-use-git-grep t)
              (current-prefix-arg '-)
              (sym (cadr test)))
          (spy-on 'projectile-project-vcs :and-return-value 'git)
          (spy-on 'read-string :and-call-fake
                  (lambda (prompt initial-input history default-value &rest args)
                    (if (should (equal sym default-value)) default-value)))
          (spy-on 'vc-git-grep :and-call-fake
                  (lambda (regexp files dir)
                    (progn (expect regexp :to-equal sym)
                           (expect files :to-equal (car (last test)))
                           (expect (projectile-project-root) :to-equal dir))))
          (with-current-buffer (find-file-noselect (car test) t)
            (save-excursion
              (re-search-forward sym)
              (projectile-grep nil ?-)))))))))

;;; projectile-grep-test.el ends here
