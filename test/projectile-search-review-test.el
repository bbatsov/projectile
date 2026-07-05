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

(defun projectile-search-review-test--kill-project-buffers (root)
  "Kill all buffers visiting files under ROOT, discarding modifications."
  (dolist (buffer (buffer-list))
    (when-let* ((file (buffer-file-name buffer)))
      (when (string-prefix-p root (file-truename file))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (let (kill-buffer-query-functions)
          (kill-buffer buffer)))))
  (dolist (name (list projectile-search-buffer-name
                      projectile-replace-buffer-name))
    (when-let* ((buf (get-buffer name)))
      (kill-buffer buf))))

(defmacro projectile-search-review-test--with-project (files &rest body)
  "Evaluate BODY in a sandbox project containing FILES.

FILES is a literal alist of (NAME . CONTENT) file specs created relative
to the project root.  A `.projectile' marker is created; include one in
FILES to override its contents (e.g. to add ignore rules).  BODY runs
with `default-directory' at the root and `projectile-project-root'
stubbed accordingly.  Project buffers and the results buffers are killed
afterwards."
  (declare (indent 1) (debug (sexp &rest form)))
  `(projectile-test-with-sandbox
     (make-directory "project" t)
     (with-temp-file "project/.projectile")
     ,@(mapcar (lambda (spec)
                 `(progn
                    (when-let* ((dir (file-name-directory ,(car spec))))
                      (make-directory (expand-file-name dir "project") t))
                    (with-temp-file (expand-file-name ,(car spec) "project")
                      (insert ,(cdr spec)))))
               files)
     (let ((default-directory (file-name-as-directory
                               (file-truename (expand-file-name "project/"))))
           (projectile-indexing-method 'native)
           (projectile-projects-cache (make-hash-table :test 'equal))
           (projectile-projects-cache-time (make-hash-table :test 'equal))
           (projectile-enable-caching nil)
           (case-fold-search t))
       (spy-on 'projectile-project-root :and-return-value default-directory)
       (unwind-protect
           (progn ,@body)
         (projectile-search-review-test--kill-project-buffers default-directory)))))

(defun projectile-search-review-test--use-plain-grep ()
  "Force `projectile-files-with-string' to shell out to plain grep."
  (assume (projectile-unixy-system-p) "needs unixy text utilities")
  (spy-on 'executable-find :and-call-fake
          (lambda (command &rest _)
            (member command '("grep" "cut" "uniq")))))

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
    (projectile-search-review-test--with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("a.txt" . "alpha foo beta\n"))
      (projectile-search-review-test--use-plain-grep)
      (let ((buf (projectile-search-review-test--run "foo")))
        (with-current-buffer buf
          (expect (buffer-string) :to-match "1:7: alpha foo beta")))))

  (it "reports no matches without popping a buffer"
    (projectile-search-review-test--with-project
        (("a.txt" . "nothing here\n"))
      (projectile-search-review-test--use-plain-grep)
      (spy-on 'message)
      (let ((buf (projectile-search-review-test--run "absent")))
        (expect buf :to-be nil)))))

(describe "projectile-search-regexp-review"
  (it "honors Emacs-only regexp constructs a shell regexp couldn't express"
    (projectile-search-review-test--with-project
        (("s.txt" . "foo foobar barfoo foo\n"))
      (let ((buf (projectile-search-review-test--run "\\_<foo\\_>" 'regexp)))
        (with-current-buffer buf
          ;; only the two standalone `foo' symbols match
          (expect (length projectile-replace--matches) :to-equal 2)
          (expect projectile-replace--literal :to-be nil))))))

(describe "projectile-search-review visit"
  (it "RET resolves the right file and line/column"
    (projectile-search-review-test--with-project
        (("a.txt" . "line one\nsecond foo here\n")
         ("lib/b.txt" . "foo at top\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("f.txt" . "keep foo here\ndrop foo there\nkeep foo again\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("keep.txt" . "foo\n")
         ("lib/skip.txt" . "foo\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("case.txt" . "Foo foo FOO\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("meta.txt" . "a.b axb a.b\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("h.txt" . "foo\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
        (("a.txt" . "foo one foo\n"))
      (projectile-search-review-test--use-plain-grep)
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
    (projectile-search-review-test--with-project
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

;;; projectile-search-review-test.el ends here
