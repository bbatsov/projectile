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

(describe "projectile-find-references prompt"
  (it "asks with the shared search prompt when no symbol is given"
    (spy-on 'projectile-acquire-root :and-return-value "/my/root/")
    (spy-on 'projectile--project-ignore-globs :and-return-value nil)
    (spy-on 'xref-matches-in-directory :and-return-value nil)
    (spy-on 'xref-show-xrefs :and-call-fake
            (lambda (fetcher &rest _) (funcall fetcher)))
    (spy-on 'projectile--read-search-string-with-default :and-return-value "foo")
    (projectile-find-references)
    (expect 'projectile--read-search-string-with-default
            :to-have-been-called-with "Find references to")
    (expect (car (spy-calls-args-for 'xref-matches-in-directory 0))
            :to-equal "foo")))

(describe "projectile--file-extensions"
  (it "lists the extensions in use, commonest first, ties alphabetically"
    (expect (projectile--file-extensions
             '("a.el" "lib/b.el" "README.md" "doc/x.adoc" "c.el" "Makefile"
               "doc/y.adoc" ".gitignore" "weird."))
            :to-equal '("el" "adoc" "md"))))

(describe "projectile--read-grep-files"
  (before-each
    (spy-on 'projectile-prepend-project-name :and-call-fake #'identity)
    (spy-on 'projectile-dir-files :and-return-value '("a.el" "b.el" "c.md")))

  (it "completes over a glob per extension in the project, in order of use"
    (let (table)
      (spy-on 'projectile-grep-default-files :and-return-value "*.el")
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _) (setq table collection) "*.md"))
      (expect (projectile--read-grep-files "/project/") :to-equal "*.md")
      (expect (all-completions "" table) :to-equal '("*.el" "*.md"))
      (expect (completion-metadata-get (completion-metadata "" table nil)
                                       'display-sort-function)
              :to-be #'identity)))

  (it "does not list the project's files unless completion is asked for"
    (spy-on 'projectile-grep-default-files :and-return-value "*.el")
    (spy-on 'completing-read :and-return-value "*.el")
    (projectile--read-grep-files "/project/")
    (expect 'projectile-dir-files :not :to-have-been-called))

  (it "does not list a remote directory at all"
    (expect (projectile--directory-extensions "/ssh:host:/project/") :to-be nil)
    (expect 'projectile-dir-files :not :to-have-been-called))

  (it "shows the pattern of the current file as the default"
    (spy-on 'projectile-grep-default-files :and-return-value "*.el")
    (spy-on 'completing-read :and-return-value "*.el")
    (projectile--read-grep-files "/project/")
    (let ((args (spy-calls-args-for 'completing-read 0)))
      (expect (car args) :to-match "Grep in (default \\*\\.el): ")
      (expect (nth 6 args) :to-equal "*.el"))))

(describe "projectile--completing-read-free-text"
  (it "lets SPC and ? be typed, for several patterns and for globs"
    (let (keys)
      (spy-on 'completing-read :and-call-fake
              (lambda (&rest _)
                ;; what the prompt's setup hook does in the minibuffer
                (with-temp-buffer
                  (use-local-map (make-sparse-keymap))
                  (run-hooks 'minibuffer-setup-hook)
                  (setq keys (list (local-key-binding (kbd "SPC"))
                                   (local-key-binding (kbd "?")))))
                "*.el *.c"))
      (expect (projectile--completing-read-free-text "Files: " nil)
              :to-equal "*.el *.c")
      (expect keys :to-equal '(self-insert-command self-insert-command)))))

(describe "projectile--read-file-extension-glob"
  (before-each
    (spy-on 'projectile-dir-files :and-return-value '("a.el" "b.el" "c.md")))

  (it "offers the extensions in the project"
    (let (table)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt collection &rest _) (setq table collection) "el"))
      (projectile--read-file-extension-glob "/project/")
      (expect (all-completions "" table) :to-equal '("el" "md"))))

  (it "turns an extension into the glob the search tools need"
    (spy-on 'completing-read :and-return-value "el")
    (expect (projectile--read-file-extension-glob "/project/") :to-equal "*.el")
    (spy-on 'completing-read :and-return-value ".rs ")
    (expect (projectile--read-file-extension-glob "/project/") :to-equal "*.rs"))

  (it "leaves a glob or a file name as typed"
    (spy-on 'completing-read :and-return-value "*.[ch]")
    (expect (projectile--read-file-extension-glob "/project/") :to-equal "*.[ch]")
    (spy-on 'completing-read :and-return-value "Makefile")
    (expect (projectile--read-file-extension-glob "/project/") :to-equal "Makefile"))

  (it "returns nil for empty input, meaning every file"
    (spy-on 'completing-read :and-return-value "")
    (expect (projectile--read-file-extension-glob "/project/") :to-be nil)))

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
                          "--glob=!*.log"))))

  (it "quotes the exclusions for the shell on request (#2176)"
    (spy-on 'projectile--dirconfig-ignore :and-return-value nil)
    (let ((projectile-globally-ignored-files nil)
          (projectile-globally-unignored-files nil)
          (projectile-globally-ignored-file-suffixes nil)
          (projectile-globally-unignored-directories nil)
          (projectile-globally-ignored-directories '("*.egg-info")))
      (expect (projectile--ripgrep-ignore-globs t)
              :to-equal (list (shell-quote-argument "--glob=!*.egg-info/")))))

  (it "quotes so that the shell hands ripgrep back the original pattern"
    ;; The point of the quoting isn't its spelling, it's that the glob
    ;; survives the shell intact.  Run it through one and see.
    (assume (not (memq system-type '(windows-nt ms-dos))) "POSIX shell")
    (spy-on 'projectile--dirconfig-ignore :and-return-value nil)
    (let ((projectile-globally-ignored-files nil)
          (projectile-globally-unignored-files nil)
          (projectile-globally-ignored-file-suffixes nil)
          (projectile-globally-unignored-directories nil)
          (projectile-globally-ignored-directories '("*.egg-info")))
      (let ((arg (car (projectile--ripgrep-ignore-globs t))))
        (expect (shell-command-to-string (concat "printf %s " arg))
                :to-equal "--glob=!*.egg-info/")))))

(describe "projectile--ripgrep"
  (before-each
    (spy-on 'projectile-acquire-root :and-return-value "/my/root/")
    (spy-on 'projectile--ripgrep-ignore-globs :and-return-value '("--glob=!x"))
    (spy-on 'display-warning)
    (setq projectile--deprecations-warned nil))

  (it "warns once that the `ripgrep' package is on its way out"
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (eq feature 'ripgrep)))
              ((symbol-function 'ripgrep-regexp) #'ignore))
      (projectile--ripgrep "foo")
      (projectile--ripgrep "bar"))
    (expect 'display-warning :to-have-been-called-times 1)
    (expect (cadr (spy-calls-args-for 'display-warning 0))
            :to-match "`ripgrep' package is deprecated"))

  (it "has nothing to warn about on the `rg' path"
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (eq feature 'rg)))
              ((symbol-function 'rg-run) #'ignore))
      (projectile--ripgrep "foo"))
    (expect 'display-warning :not :to-have-been-called))

  (it "prefers the `rg' package when both are installed"
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (memq feature '(rg ripgrep))))
              ((symbol-function 'rg-run) #'ignore)
              ((symbol-function 'ripgrep-regexp) #'ignore))
      (spy-on 'rg-run)
      (spy-on 'ripgrep-regexp)
      (projectile--ripgrep "foo")
      (expect 'rg-run :to-have-been-called)
      (expect 'ripgrep-regexp :not :to-have-been-called)))

  (it "asks for shell-quoted globs on the `ripgrep' path (#2176)"
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (eq feature 'ripgrep)))
              ((symbol-function 'ripgrep-regexp) #'ignore))
      (projectile--ripgrep "foo" t))
    (expect 'projectile--ripgrep-ignore-globs :to-have-been-called-with t))

  (it "searches hidden files on the `ripgrep' path, regexp or literal (#1708)"
    (let (seen)
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest _) (eq feature 'ripgrep)))
                ((symbol-function 'ripgrep-regexp)
                 (lambda (_term _dir args) (push args seen))))
        (projectile--ripgrep "foo" t)
        (projectile--ripgrep "foo"))
      (expect (nreverse seen)
              :to-equal '(("--hidden" "--glob=!x")
                          ("--fixed-strings --hidden" "--glob=!x")))))

  (it "asks for unquoted globs on the `rg' path, which quotes them itself"
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (eq feature 'rg)))
              ((symbol-function 'rg-run) #'ignore))
      (projectile--ripgrep "foo" t))
    (expect 'projectile--ripgrep-ignore-globs :to-have-been-called-with)))

;; ag.el's own variable, unbound here because the package is not loaded
(defvar ag-ignore-list)

(describe "the deprecated ag backend"
  (before-each
    (spy-on 'projectile-acquire-root :and-return-value "/my/root/")
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (spy-on 'display-warning)
    (setq projectile--deprecations-warned nil))

  (it "still searches, and warns once that it is going away"
    (let (searched ag-ignore-list)
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest _) (eq feature 'ag)))
                ((symbol-function 'ag)
                 (lambda (term dir) (push (list term dir) searched))))
        (projectile--ag "foo")
        (projectile--ag "bar"))
      (expect (nreverse searched)
              :to-equal '(("foo" "/my/root/") ("bar" "/my/root/"))))
    (expect 'display-warning :to-have-been-called-times 1)
    (expect (cadr (spy-calls-args-for 'display-warning 0))
            :to-match "ag search backend is deprecated"))

  (it "marks projectile-ag obsolete in favour of projectile-ripgrep"
    (expect (car (get 'projectile-ag 'byte-obsolete-info))
            :to-be 'projectile-ripgrep)))

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
          (spy-on 'read-from-minibuffer :and-call-fake
                  (lambda (_prompt _initial _keymap _read _history defaults &rest _)
                    (if (should (equal sym (car defaults))) (car defaults))))
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
