;;; projectile-search-review-test.el --- Tests for the reviewable search UI -*- lexical-binding: t -*-

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

;; Tests for `projectile-search-review' and
;; `projectile-search-regexp-review', the read-only search reviewer.  Like
;; the replace-review tests, these exercise real files on disk and real live
;; buffers, since faithful matching against real content is the whole point.

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-search-review-test--run (term &optional regexp-p)
  "Drive the search review command for TERM and return the results buffer.
REGEXP-P selects `projectile-search-regexp-review'."
  (let ((inputs (list term)))
    (spy-on 'read-string :and-call-fake (lambda (&rest _) (pop inputs)))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (call-interactively (if regexp-p
                              #'projectile-search-regexp-review
                            #'projectile-search-review)))
    (get-buffer projectile-search-buffer-name)))

(defun projectile-search-review-test--files (buf)
  "Return the sorted set of project-relative files with matches in BUF."
  (with-current-buffer buf
    (let ((root projectile-replace--root)
          (files nil))
      (dolist (m projectile-replace--matches)
        (cl-pushnew (file-relative-name (projectile-replace--match-file m) root)
                    files :test #'equal))
      (sort files #'string<))))

(describe "projectile-search-review (literal)"
  (it "finds every literal match grouped by file, read-only, no apply keys"
    (projectile-test-with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect major-mode :to-equal 'projectile-search-mode)
          (expect (length projectile-replace--matches) :to-equal 3)
          (expect (projectile-search-review-test--files buf)
                  :to-equal '("a.txt" "lib/b.txt"))
          ;; the buffer is genuinely read-only
          (expect buffer-read-only :to-be-truthy)
          ;; and exposes no write-back / enable-toggle commands
          (expect (lookup-key projectile-search-mode-map (kbd "!")) :to-be nil)
          (expect (lookup-key projectile-search-mode-map (kbd "t")) :to-be nil)
          (expect (lookup-key projectile-search-mode-map (kbd "C-c C-c"))
                  :not :to-equal 'projectile-replace--apply)
          ;; the rendered body carries no [X]/[ ] enable indicators
          (expect (buffer-string) :not :to-match "\\[[ X]\\]")))))

  (it "renders each match as LINE:COL: with the matched span present"
    (projectile-test-with-project
        (("a.txt" . "alpha foo beta\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect (buffer-string) :to-match "1:7: alpha foo beta")))))

  (it "reports no matches without popping a buffer"
    (projectile-test-with-project
        (("a.txt" . "nothing here\n"))
      (projectile-test-use-plain-grep)
      (spy-on 'message)
      (let ((buf (projectile-search-review-test--run "absent")))
        (expect buf :to-be nil)))))

(describe "projectile-search-review prompt tool tag"
  (before-each
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-replace--candidates :and-return-value nil)
    (spy-on 'projectile-replace--open))

  (it "advertises [ripgrep] when the literal fast-path will run"
    (spy-on 'projectile-search--rg-fastpath-p :and-return-value t)
    (let (label)
      (spy-on 'projectile--read-search-string-with-default :and-call-fake
              (lambda (l) (setq label l) "foo"))
      (projectile-search--review t)
      (expect label :to-match (regexp-quote "[ripgrep]"))))

  (it "advertises [elisp] for a regexp search that never takes the fast-path"
    (spy-on 'projectile-search--rg-fastpath-p :and-return-value nil)
    (let (label)
      (spy-on 'projectile--read-search-string-with-default :and-call-fake
              (lambda (l) (setq label l) "foo"))
      (projectile-search--review nil)
      (expect label :to-match (regexp-quote "[elisp]")))))

(describe "projectile-search-review whole-word"
  (it "fences the pattern in shy-grouped word boundaries"
    (expect (projectile-replace--word-boundary-regexp "foo\\|bar")
            :to-equal "\\<\\(?:foo\\|bar\\)\\>"))

  (it "matches only whole-word occurrences when whole-word mode is on"
    (projectile-test-with-project
        (("a.txt" . "foo foobar barfoo foo\n"))
      (projectile-test-use-plain-grep)
      (let* ((projectile-search-whole-word t)
             (buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect projectile-replace--word :to-be t)
          ;; the two standalone `foo's match, foobar/barfoo do not
          (expect (length projectile-replace--matches) :to-equal 2)
          (expect (buffer-string) :to-match "\\[word\\]")))))

  (it "toggling whole-word off re-scans and matches substrings again"
    (projectile-test-with-project
        (("a.txt" . "foo foobar barfoo foo\n"))
      (projectile-test-use-plain-grep)
      (let* ((projectile-search-whole-word t)
             (buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 2)
          ;; batch scans synchronously, so the re-gather is done on return
          (projectile-replace--toggle-word)
          (expect projectile-replace--word :to-be nil)
          (expect (length projectile-replace--matches) :to-equal 4))))))

(describe "projectile-search-regexp-review"
  (it "honors Emacs-only regexp constructs a shell regexp couldn't express"
    (projectile-test-with-project
        (("s.txt" . "foo foobar barfoo foo\n"))
      (let ((buf (projectile-search-review-test--run "\\_<foo\\_>" 'regexp)))
        (with-current-buffer buf
          ;; only the two standalone `foo' symbols match
          (expect (length projectile-replace--matches) :to-equal 2)
          (expect projectile-replace--literal :to-be nil))))))

(describe "projectile-search-review visit"
  (it "RET resolves the right file and line/column"
    (projectile-test-with-project
        (("a.txt" . "line one\nsecond foo here\n")
         ("lib/b.txt" . "foo at top\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          ;; jump to the match in lib/b.txt
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (let ((m (projectile-replace--match-at-point)))
                (when (and m (string-suffix-p
                              "lib/b.txt"
                              (projectile-replace--match-file m)))
                  (setq found t)))
              (unless found (forward-line 1)))
            (expect found :to-be-truthy))
          (let (visited)
            (cl-letf (((symbol-function 'switch-to-buffer-other-window)
                       (lambda (b &rest _) (set-buffer b) (setq visited b))))
              (projectile-replace--visit))
            ;; point now sits in lib/b.txt on the match
            (with-current-buffer visited
              (expect (string-suffix-p "lib/b.txt" (buffer-file-name))
                      :to-be-truthy)
              (expect (line-number-at-pos) :to-equal 1)
              (expect (current-column) :to-equal 0))))))))

(describe "projectile-search-review filtering"
  (it "prunes matches by line and marks the list filtered"
    (projectile-test-with-project
        (("f.txt" . "keep foo here\ndrop foo there\nkeep foo again\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 3)
          (projectile-replace--keep-matches "keep")
          (expect (length projectile-replace--matches) :to-equal 2)
          (expect projectile-replace--filtered :to-be-truthy)
          ;; re-search brings the pruned match back
          (projectile-replace--refresh)
          (expect (length projectile-replace--matches) :to-equal 3)
          (expect projectile-replace--filtered :to-be nil)))))

  (it "prunes matches by file"
    (projectile-test-with-project
        (("keep.txt" . "foo\n")
         ("lib/skip.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 2)
          (projectile-replace--flush-files "lib/")
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect (file-name-nondirectory
                   (projectile-replace--match-file
                    (car projectile-replace--matches)))
                  :to-equal "keep.txt"))))))

(describe "projectile-search-review case and regexp toggles"
  (it "re-scans when case sensitivity flips"
    (projectile-test-with-project
        (("case.txt" . "Foo foo FOO\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect projectile-replace--case-fold :to-be-truthy)
          (expect (length projectile-replace--matches) :to-equal 3)
          (projectile-replace--toggle-case)
          (expect projectile-replace--case-fold :to-be nil)
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect (projectile-replace--match-string
                   (car projectile-replace--matches))
                  :to-equal "foo")))))

  (it "re-scans when literal/regexp flips"
    (projectile-test-with-project
        (("meta.txt" . "a.b axb a.b\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "a.b")))
        (with-current-buffer buf
          (expect projectile-replace--literal :to-be-truthy)
          (expect (length projectile-replace--matches) :to-equal 2)
          (projectile-replace--toggle-regexp)
          (expect projectile-replace--literal :to-be nil)
          ;; regexp: the `.' now also matches the `x' in `axb'
          (expect (length projectile-replace--matches) :to-equal 3))))))

(describe "projectile-search-review status header"
  (it "shows the term, counts and mode flags"
    (projectile-test-with-project
        (("h.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (let ((header (buffer-substring-no-properties
                         (point-min)
                         (save-excursion (goto-char (point-min))
                                         (line-end-position 2)))))
            (expect header :to-match "Search \"foo\"")
            (expect header :to-match "\\[literal\\]")
            (expect header :to-match "\\[ignore-case\\]")
            ;; a search header has no replacement segment
            (expect header :not :to-match "with")))))))

(defun projectile-search-review-test--export (buf)
  "Export search results buffer BUF to a grep buffer and return it."
  (with-current-buffer buf
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (projectile-replace--export))))

(describe "projectile-search-review export to grep-mode"
  (it "renders the shown matches as a navigable grep-mode buffer"
    (projectile-test-with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-test-use-plain-grep)
      (let* ((buf (projectile-search-review-test--run "foo"))
             (gbuf (projectile-search-review-test--export buf)))
        (unwind-protect
            (with-current-buffer gbuf
              (expect major-mode :to-equal 'grep-mode)
              (let ((text (buffer-string)))
                ;; the export is labelled for the reviewer it came from
                (expect text :to-match "Projectile search")
                (expect text :not :to-match "Projectile replace")
                (expect text :to-match "^a\\.txt:1:foo one foo$")
                (expect text :to-match "^lib/b\\.txt:1:start foo end$")))
          (kill-buffer gbuf))))))

(describe "projectile-search-review to-replace bridge"
  (it "opens the replace reviewer preloaded with the same term"
    (projectile-test-with-project
        (("a.txt" . "foo one foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          ;; the bridge prompts only for the replacement
          (spy-on 'read-string :and-return-value "bar")
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (projectile-search--to-replace)))
        (let ((rbuf (get-buffer projectile-replace-buffer-name)))
          (expect rbuf :not :to-be nil)
          (with-current-buffer rbuf
            (expect major-mode :to-equal 'projectile-replace-mode)
            (expect projectile-replace--term :to-equal "foo")
            (expect projectile-replace--literal :to-be-truthy)
            (expect projectile-replace--replacement :to-equal "bar")
            (expect (length projectile-replace--matches) :to-equal 2)
            ;; the replace buffer really is applyable (has the enable indicator)
            (expect (buffer-string) :to-match "\\[X\\]"))))))

  (it "carries literal-ness through from a regexp search"
    (projectile-test-with-project
        (("s.txt" . "foo foobar foo\n"))
      (let ((buf (projectile-search-review-test--run "\\_<foo\\_>" 'regexp)))
        (with-current-buffer buf
          (spy-on 'read-string :and-return-value "X")
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (projectile-search--to-replace)))
        (with-current-buffer (get-buffer projectile-replace-buffer-name)
          (expect projectile-replace--literal :to-be nil)
          (expect projectile-replace--search :to-equal "\\_<foo\\_>")
          (expect (length projectile-replace--matches) :to-equal 2))))))

;;; Ripgrep fast-path

(defun projectile-search-review-test--wait (buf)
  "Pump events until BUF's ripgrep scan finishes (or a timeout elapses)."
  (projectile-test-wait-for
   (lambda () (not (buffer-local-value 'projectile-replace--scanning buf)))))

(describe "projectile-search--rg byte->char column conversion"
  (it "counts a multibyte prefix as characters, not bytes"
    ;; "café " is 5 characters but 6 UTF-8 bytes (é is 2 bytes); ripgrep
    ;; reports the match at byte 6, which must map to character column 5.
    (expect (projectile-search--rg-byte->char-column "café foo" 6) :to-equal 5)
    (expect (projectile-search--rg-byte->char-column "abc" 2) :to-equal 2)
    (expect (projectile-search--rg-byte->char-column "x" 0) :to-equal 0)
    (expect (projectile-search--rg-byte->char-column "x" nil) :to-equal 0)))

(describe "projectile-search--rg-parse-line"
  (it "builds a struct with a character column from a multibyte match line"
    (let* ((json (concat "{\"type\":\"match\",\"data\":{"
                         "\"path\":{\"text\":\"lib/mb.txt\"},"
                         "\"lines\":{\"text\":\"café foo bar\\n\"},"
                         "\"line_number\":3,\"absolute_offset\":0,"
                         "\"submatches\":[{\"match\":{\"text\":\"foo\"},"
                         "\"start\":6,\"end\":9}]}}"))
           (ms (projectile-search--rg-parse-line json "/proj/"))
           (m (car ms)))
      (expect (length ms) :to-equal 1)
      ;; byte offset 6 -> character column 5; the trailing newline is stripped
      ;; from the context line
      (expect m :to-be-a-match-with '(:file "/proj/lib/mb.txt" :line 3 :column 5
                                      :string "foo" :context "café foo bar"))
      ;; write-back-only fields stay nil (search never uses them)
      (expect (projectile-replace--match-beg m) :to-be nil)
      (expect (projectile-replace--match-end m) :to-be nil)
      (expect (projectile-replace--match-match-data m) :to-be nil)))

  (it "yields one struct per submatch, in order"
    (let* ((json (concat "{\"type\":\"match\",\"data\":{"
                         "\"path\":{\"text\":\"a.txt\"},"
                         "\"lines\":{\"text\":\"foo baz foo\\n\"},"
                         "\"line_number\":1,"
                         "\"submatches\":[{\"match\":{\"text\":\"foo\"},"
                         "\"start\":0,\"end\":3},"
                         "{\"match\":{\"text\":\"foo\"},\"start\":8,\"end\":11}]}}"))
           (ms (projectile-search--rg-parse-line json "/proj/")))
      (expect (length ms) :to-equal 2)
      (expect (mapcar #'projectile-replace--match-column ms) :to-equal '(0 8))))

  (it "returns nil for non-match records"
    (expect (projectile-search--rg-parse-line
             "{\"type\":\"begin\",\"data\":{\"path\":{\"text\":\"a.txt\"}}}" "/p/")
            :to-be nil)
    (expect (projectile-search--rg-parse-line
             "{\"type\":\"summary\",\"data\":{}}" "/p/")
            :to-be nil))

  (it "swallows a malformed JSON line rather than erroring"
    (expect (projectile-search--rg-parse-line "not json at all" "/p/")
            :to-be nil))

  (it "skips a match whose line or path is non-UTF-8 (bytes, not text)"
    ;; rg emits {\"bytes\": <base64>} instead of {\"text\": ...} for content
    ;; that isn't valid UTF-8; the fast-path drops those cleanly (they surface
    ;; via the portable elisp path instead)
    (expect (projectile-search--rg-parse-line
             (concat "{\"type\":\"match\",\"data\":{"
                     "\"path\":{\"text\":\"a.txt\"},"
                     "\"lines\":{\"bytes\":\"Zm9vCg==\"},"
                     "\"line_number\":1,"
                     "\"submatches\":[{\"match\":{\"text\":\"foo\"},"
                     "\"start\":0,\"end\":3}]}}")
             "/p/")
            :to-be nil)
    (expect (projectile-search--rg-parse-line
             (concat "{\"type\":\"match\",\"data\":{"
                     "\"path\":{\"bytes\":\"Zm9v\"},"
                     "\"lines\":{\"text\":\"foo\\n\"},"
                     "\"line_number\":1,"
                     "\"submatches\":[{\"match\":{\"text\":\"foo\"},"
                     "\"start\":0,\"end\":3}]}}")
             "/p/")
            :to-be nil)))

(describe "projectile-search--rg-command"
  (it "builds a fixed-strings, ignore-aware command with the term after --"
    (let ((cmd (projectile-search--rg-command
                "foo-bar" t nil '("node_modules/" "*.elc" "/vendor/"))))
      (expect (member "--fixed-strings" cmd) :to-be-truthy)
      (expect (member "--ignore-case" cmd) :to-be-truthy)
      (expect (member "--case-sensitive" cmd) :to-be nil)
      ;; no whole-word restriction unless requested
      (expect (member "--word-regexp" cmd) :to-be nil)
      ;; a basename glob becomes a negated --glob verbatim
      (expect (member "!node_modules/" cmd) :to-be-truthy)
      (expect (member "!*.elc" cmd) :to-be-truthy)
      ;; ripgrep speaks gitignore, so a root-anchored glob is passed as is
      (expect (member "!/vendor/" cmd) :to-be-truthy)
      ;; the term is right after the -- terminator, then the relative `./' root
      (let ((tail (cdr (member "--" cmd))))
        (expect (car tail) :to-equal "foo-bar")
        (expect (cadr tail) :to-equal "./"))))

  (it "uses --case-sensitive when case-fold is nil"
    (let ((cmd (projectile-search--rg-command "foo" nil nil nil)))
      (expect (member "--case-sensitive" cmd) :to-be-truthy)
      (expect (member "--ignore-case" cmd) :to-be nil)))

  (it "adds --word-regexp when whole-word matching is requested"
    (let ((cmd (projectile-search--rg-command "foo" nil t nil)))
      (expect (member "--word-regexp" cmd) :to-be-truthy))))

(describe "projectile-search--rg-ingest streaming and cap"
  (it "honors projectile-replace-max-matches and marks the list truncated"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (get-buffer-create projectile-search-buffer-name))
            (line (lambda (n)
                    (format (concat "{\"type\":\"match\",\"data\":{"
                                    "\"path\":{\"text\":\"a.txt\"},"
                                    "\"lines\":{\"text\":\"foo\\n\"},"
                                    "\"line_number\":%d,"
                                    "\"submatches\":[{\"match\":{\"text\":\"foo\"},"
                                    "\"start\":0,\"end\":3}]}}")
                            n))))
        (projectile-replace--seed buf #'projectile-search-mode
                                  default-directory "foo" "foo" nil t t)
        (with-current-buffer buf (setq projectile-replace--scanning t))
        (let ((projectile-replace-max-matches 2))
          (let ((capped (projectile-search--rg-ingest
                         buf (list (funcall line 1)
                                   (funcall line 2)
                                   (funcall line 3))
                         default-directory)))
            (expect capped :to-be-truthy))
          (with-current-buffer buf
            (expect (length projectile-replace--matches) :to-equal 2)
            (expect projectile-replace--truncated :to-be-truthy)))))))

(describe "projectile-search-review ripgrep process lifecycle"
  (it "cancel-scan kills the running process, leaving no orphan"
    (assume (executable-find "sleep") "needs a sleep executable")
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      ;; stub the command so we get a controllable long-running process
      ;; without depending on ripgrep being installed
      (cl-letf (((symbol-function 'projectile-search--rg-command)
                 (lambda (&rest _) (list "sleep" "30"))))
        (let ((buf (get-buffer-create projectile-search-buffer-name)))
          (projectile-replace--seed buf #'projectile-search-mode
                                    default-directory "foo" "foo" nil t t)
          (projectile-search--gather-rg buf "foo" nil)
          (let ((proc (buffer-local-value 'projectile-replace--scan-process buf)))
            (expect (process-live-p proc) :to-be-truthy)
            (expect (memq proc (process-list)) :to-be-truthy)
            (with-current-buffer buf (projectile-replace--cancel-scan))
            (expect (process-live-p proc) :to-be nil)
            (expect (memq proc (process-list)) :to-be nil)
            (expect (buffer-local-value 'projectile-replace--scan-process buf)
                    :to-be nil))))))

  (it "killing the results buffer kills the running process"
    (assume (executable-find "sleep") "needs a sleep executable")
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (cl-letf (((symbol-function 'projectile-search--rg-command)
                 (lambda (&rest _) (list "sleep" "30"))))
        (let ((buf (get-buffer-create projectile-search-buffer-name)))
          (projectile-replace--seed buf #'projectile-search-mode
                                    default-directory "foo" "foo" nil t t)
          (projectile-search--gather-rg buf "foo" nil)
          (let ((proc (buffer-local-value 'projectile-replace--scan-process buf)))
            (expect (process-live-p proc) :to-be-truthy)
            (kill-buffer buf)
            (expect (process-live-p proc) :to-be nil)
            (expect (memq proc (process-list)) :to-be nil)))))))

(describe "projectile-search-review ripgrep dispatch"
  (before-each
    ;; make the fast-path see ripgrep as available without depending on a
    ;; real rg in CI; the gathers are spied so nothing actually runs
    (spy-on 'executable-find :and-call-fake
            (lambda (command &rest _) (and (equal command "rg") "rg"))))

  (it "fastpath-p holds only for literal, enabled, interactive, rg present"
    (let ((noninteractive nil) (projectile-search-use-ripgrep t))
      (expect (projectile-search--rg-fastpath-p t) :to-be-truthy)
      ;; a regexp search never takes the fast-path
      (expect (projectile-search--rg-fastpath-p nil) :to-be nil))
    (let ((noninteractive nil) (projectile-search-use-ripgrep nil))
      (expect (projectile-search--rg-fastpath-p t) :to-be nil))
    ;; batch keeps the deterministic elisp scan
    (let ((noninteractive t) (projectile-search-use-ripgrep t))
      (expect (projectile-search--rg-fastpath-p t) :to-be nil)))

  (it "routes a literal search-mode buffer to the ripgrep gather"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (get-buffer-create projectile-search-buffer-name)))
        (projectile-replace--seed buf #'projectile-search-mode
                                  default-directory "foo" "foo" nil t t)
        (spy-on 'projectile-search--gather-rg)
        (spy-on 'projectile-replace--gather-async)
        (let ((noninteractive nil) (projectile-search-use-ripgrep t))
          (projectile-replace--start buf nil "foo" nil))
        (expect 'projectile-search--gather-rg :to-have-been-called)
        (expect 'projectile-replace--gather-async :not :to-have-been-called))))

  (it "routes a regexp search-mode buffer to the elisp path (no rg)"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (get-buffer-create projectile-search-buffer-name)))
        (projectile-replace--seed buf #'projectile-search-mode
                                  default-directory "foo" "foo" nil nil t)
        (spy-on 'projectile-search--gather-rg)
        (spy-on 'projectile-replace--gather-async)
        (let ((noninteractive nil) (projectile-search-use-ripgrep t))
          (projectile-replace--start buf nil "foo" nil))
        (expect 'projectile-search--gather-rg :not :to-have-been-called)
        (expect 'projectile-replace--gather-async :to-have-been-called))))

  (it "never routes a replace-mode buffer to ripgrep, even for a literal"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (get-buffer-create projectile-replace-buffer-name)))
        (projectile-replace--seed buf #'projectile-replace-mode
                                  default-directory "foo" "foo" "" t t)
        (spy-on 'projectile-search--gather-rg)
        (spy-on 'projectile-replace--gather-async)
        (let ((noninteractive nil) (projectile-search-use-ripgrep t))
          (projectile-replace--start buf nil "foo" nil))
        (expect 'projectile-search--gather-rg :not :to-have-been-called)
        (expect 'projectile-replace--gather-async :to-have-been-called))))

  (it "projectile-search-use-ripgrep nil forces the elisp path"
    (projectile-test-with-project
        (("a.txt" . "foo\n"))
      (let ((buf (get-buffer-create projectile-search-buffer-name)))
        (projectile-replace--seed buf #'projectile-search-mode
                                  default-directory "foo" "foo" nil t t)
        (spy-on 'projectile-search--gather-rg)
        (spy-on 'projectile-replace--gather-async)
        (let ((noninteractive nil) (projectile-search-use-ripgrep nil))
          (projectile-replace--start buf nil "foo" nil))
        (expect 'projectile-search--gather-rg :not :to-have-been-called)
        (expect 'projectile-replace--gather-async :to-have-been-called)))))

(describe "projectile-search-review ripgrep end-to-end"
  (it "finds matches with a correct character column on a multibyte line"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("mb.txt" . "café foo bar\n")
         ("plain.txt" . "foo\n"))
      (let ((buf (get-buffer-create projectile-search-buffer-name))
            (root default-directory))
        (projectile-replace--seed buf #'projectile-search-mode
                                  root "foo" "foo" nil t t)
        (projectile-search--gather-rg buf "foo" nil)
        (projectile-search-review-test--wait buf)
        (with-current-buffer buf
          (expect projectile-replace--scanning :to-be nil)
          (expect (projectile-search-review-test--files buf)
                  :to-equal '("mb.txt" "plain.txt"))
          ;; character column 5 ("café " is 5 chars), NOT the byte offset 6
          (expect (projectile-test-find-match projectile-replace--matches "mb.txt")
                  :to-be-a-match-with '(:line 1 :column 5 :string "foo"
                                        :context "café foo bar"))))))

  (it "honors projectile-replace-max-matches over a real ripgrep stream"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("a.txt" . "foo\nfoo\nfoo\n"))
      (let ((buf (get-buffer-create projectile-search-buffer-name))
            (root default-directory)
            (projectile-replace-max-matches 1))
        (projectile-replace--seed buf #'projectile-search-mode
                                  root "foo" "foo" nil t t)
        (projectile-search--gather-rg buf "foo" nil)
        (projectile-search-review-test--wait buf)
        (with-current-buffer buf
          (expect projectile-replace--scanning :to-be nil)
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect projectile-replace--truncated :to-be-truthy)))))

  (it "excludes a path-rooted .projectile ignore from the ripgrep results"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        ((".projectile" . "-/vendor\n")
         ("keep.txt" . "foo\n")
         ("vendor/hidden.txt" . "foo\n"))
      (let ((buf (get-buffer-create projectile-search-buffer-name))
            (root default-directory))
        (projectile-replace--seed buf #'projectile-search-mode
                                  root "foo" "foo" nil t t)
        (projectile-search--gather-rg buf "foo" nil)
        (projectile-search-review-test--wait buf)
        (with-current-buffer buf
          ;; the `-/vendor' dirconfig ignore must be honored by the rg path,
          ;; just as it is by the elisp path
          (expect (projectile-search-review-test--files buf)
                  :to-equal '("keep.txt"))
          (expect (cl-some (lambda (m)
                             (string-match-p
                              "vendor"
                              (projectile-replace--match-file m)))
                           projectile-replace--matches)
                  :to-be nil))))))

(describe "projectile-search-review ripgrep vs elisp parity"
  (it "the ripgrep fast-path finds the same matches as the elisp scan"
    (assume (executable-find "rg") "needs a real ripgrep")
    (projectile-test-with-project
        (("a.txt" . "foo one\nno match here\nfoo two\n")
         ("sub/b.txt" . "prefix foo suffix\n")
         ("c.txt" . "nothing to see\n"))
      (let* ((root default-directory)
             (term "foo")
             ;; elisp reference scan (the portable, deterministic path)
             (candidates (projectile-replace--candidates term t t root))
             (elisp (plist-get (projectile-replace--gather candidates term)
                               :matches))
             ;; ripgrep fast-path
             (buf (get-buffer-create projectile-search-buffer-name)))
        (projectile-replace--seed buf #'projectile-search-mode
                                  root term term nil t t)
        (projectile-search--gather-rg buf term nil)
        (projectile-search-review-test--wait buf)
        (expect (projectile-test-match-sig
                 (buffer-local-value 'projectile-replace--matches buf) root)
                :to-equal
                (projectile-test-match-sig elisp root))))))

;;; projectile-search-review-test.el ends here
