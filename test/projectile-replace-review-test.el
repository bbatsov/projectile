;;; projectile-replace-review-test.el --- Tests for the reviewable replace UI -*- lexical-binding: t -*-

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

;; Tests for `projectile-replace-review' and
;; `projectile-replace-regexp-review' and their gather/apply machinery.
;; These exercise real files on disk (and real live buffers), not mocks,
;; since the whole point of the feature is faithful preview and write-back.

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-replace-review-test--run (term replacement &optional regexp-p)
  "Drive the review command for TERM/REPLACEMENT and return the results buffer.
REGEXP-P selects `projectile-replace-regexp-review'."
  (let ((inputs (list term replacement)))
    (spy-on 'read-string :and-call-fake (lambda (&rest _) (pop inputs)))
    ;; keep window juggling out of the batch run without tripping
    ;; buttercup's interactive-form check on a spied command
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (call-interactively (if regexp-p
                              #'projectile-replace-regexp-review
                            #'projectile-replace-review)))
    (get-buffer projectile-replace-buffer-name)))

(defun projectile-replace-review-test--apply (buf)
  "Apply the enabled matches in results buffer BUF."
  (with-current-buffer buf
    (projectile-replace--apply)))

(defun projectile-replace-review-test--header (buf)
  "Return the two-line status header of results buffer BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min)
                                    (save-excursion
                                      (goto-char (point-min))
                                      (line-end-position 2)))))

(describe "projectile-replace-review (literal)"
  (it "finds every literal match and applies them to a file on disk"
    (projectile-test-with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 3))
        (projectile-replace-review-test--apply buf)
        ;; the files were never opened, so this proves the disk write-back
        (expect (get-file-buffer (expand-file-name "a.txt")) :to-be nil)
        (expect (projectile-test-disk "a.txt")
                :to-equal "bar one bar\n")
        (expect (projectile-test-disk "lib/b.txt")
                :to-equal "start bar end\n"))))

  (it "applies multiple matches on one line in descending order"
    (projectile-test-with-project
        (("m.txt" . "xx xx xx\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "xx" "yyy")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 3))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "m.txt")
                :to-equal "yyy yyy yyy\n"))))

  (it "edits an already-open buffer in place rather than the file on disk"
    (projectile-test-with-project
        (("open.txt" . "first foo\nlast foo\n"))
      (projectile-test-use-plain-grep)
      (find-file-noselect (expand-file-name "open.txt"))
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          ;; the scan tagged the matches with the live buffer
          (expect (projectile-replace--match-buffer
                   (car projectile-replace--matches))
                  :to-equal (get-file-buffer (expand-file-name "open.txt"))))
        (projectile-replace-review-test--apply buf)
        (with-current-buffer (get-file-buffer (expand-file-name "open.txt"))
          (expect (buffer-string) :to-equal "first bar\nlast bar\n"))
        (expect (projectile-test-disk "open.txt")
                :to-equal "first bar\nlast bar\n"))))

  (it "leaves a disabled match untouched and applies only the enabled ones"
    (projectile-test-with-project
        (("t.txt" . "foo foo foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          ;; disable the first of the three matches
          (setf (projectile-replace--match-enabled
                 (car projectile-replace--matches))
                nil))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "t.txt")
                :to-equal "foo bar bar\n")))))

(describe "projectile-replace-regexp-review"
  (it "honors Emacs-only regexp constructs a shell regexp couldn't express"
    (projectile-test-with-project
        ;; \_< \_> are Emacs symbol boundaries: grep/rg cannot express them
        (("s.txt" . "foo foobar barfoo foo\n"))
      (let ((buf (projectile-replace-review-test--run "\\_<foo\\_>" "X" 'regexp)))
        (with-current-buffer buf
          ;; only the two standalone `foo' symbols match
          (expect (length projectile-replace--matches) :to-equal 2))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "s.txt")
                :to-equal "X foobar barfoo X\n"))))

  (it "substitutes capture groups in the replacement"
    (projectile-test-with-project
        (("c.txt" . "foo_bar and baz_qux\n"))
      (let ((buf (projectile-replace-review-test--run
                  "\\([a-z]+\\)_\\([a-z]+\\)" "\\2-\\1" 'regexp)))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "c.txt")
                :to-equal "bar-foo and qux-baz\n"))))

  (it "excludes files ignored via .projectile"
    (projectile-test-with-project
        ((".projectile" . "-secret\n")
         ("keep.txt" . "foo\n")
         ("secret/hide.txt" . "foo\n"))
      (let ((buf (projectile-replace-review-test--run "foo" "bar" 'regexp)))
        (with-current-buffer buf
          ;; the ignored file must not contribute any match
          (expect (cl-some (lambda (m)
                             (string-match-p
                              "secret"
                              (projectile-replace--match-file m)))
                           projectile-replace--matches)
                  :to-be nil))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "keep.txt")
                :to-equal "bar\n")
        (expect (projectile-test-disk "secret/hide.txt")
                :to-equal "foo\n")))))

(describe "projectile-replace--expand"
  (it "returns the replacement verbatim in literal mode"
    (expect (projectile-replace--expand "a\\1b" '("whole") t)
            :to-equal "a\\1b"))
  (it "expands \\N and \\& group references in regexp mode"
    (expect (projectile-replace--expand "\\2-\\1-\\&"
                                        '("foo_bar" "foo" "bar") nil)
            :to-equal "bar-foo-foo_bar"))
  (it "treats \\\\ as a literal backslash"
    (expect (projectile-replace--expand "x\\\\y" '("m") nil)
            :to-equal "x\\y")))

(describe "projectile-replace write-back safety"
  (it "skips a closed file that changed on disk since the scan"
    (projectile-test-with-project
        (("a.txt" . "hello foo world\nsecond foo line\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; the file changes on disk between scan and apply; stale positions
        ;; must NOT be applied to the shifted text
        (with-temp-file (expand-file-name "a.txt")
          (insert "PREPENDED LINE\nhello foo world\nsecond foo line\n"))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "a.txt")
                :to-equal
                "PREPENDED LINE\nhello foo world\nsecond foo line\n"))))

  (it "does not hang scanning a regexp that matches empty at end of buffer"
    (projectile-test-with-project
        (("z.txt" . "aaa\nbbb\n"))
      ;; `^' matches the empty string at every line start, including at
      ;; end-of-buffer; the scan must terminate instead of spinning forever
      (let ((result (projectile-replace--gather
                     (list (expand-file-name "z.txt")) "^")))
        (expect (plist-member result :matches) :to-be-truthy))))

  (it "applies via disk when the scanned buffer was killed before apply"
    (projectile-test-with-project
        (("k.txt" . "foo here\n"))
      (projectile-test-use-plain-grep)
      (find-file-noselect (expand-file-name "k.txt"))
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; the scan tagged matches with the live buffer; kill it before apply
        (let (kill-buffer-query-functions)
          (kill-buffer (get-file-buffer (expand-file-name "k.txt"))))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "k.txt")
                :to-equal "bar here\n"))))

  (it "edits the buffer, not disk, for a file opened and modified after scan"
    (projectile-test-with-project
        (("o.txt" . "foo tail\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; closed at scan; now open it and make an unsaved edit AFTER the
        ;; match, so the recorded position still spans `foo'
        (let ((fb (find-file-noselect (expand-file-name "o.txt"))))
          (with-current-buffer fb
            (goto-char (point-max))
            (insert "UNSAVED\n"))
          (projectile-replace-review-test--apply buf)
          ;; edit lands in the buffer; disk is not clobbered behind it
          (with-current-buffer fb
            (expect (buffer-string) :to-equal "bar tail\nUNSAVED\n"))
          (expect (projectile-test-disk "o.txt")
                  :to-equal "foo tail\n")))))

  (it "skips a live buffer's match when text is inserted before it after the scan"
    (projectile-test-with-project
        (("p.txt" . "keep\nfoo tail\n"))
      (projectile-test-use-plain-grep)
      ;; open before the scan so the match is tagged against the live buffer
      (find-file-noselect (expand-file-name "p.txt"))
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; insert BEFORE the match, shifting the recorded position off `foo';
        ;; `--positions-valid-p' must reject it and the match must be skipped
        (with-current-buffer (get-file-buffer (expand-file-name "p.txt"))
          (goto-char (point-min))
          (insert "PREPENDED\n"))
        (projectile-replace-review-test--apply buf)
        (with-current-buffer (get-file-buffer (expand-file-name "p.txt"))
          (expect (buffer-string) :to-equal "PREPENDED\nkeep\nfoo tail\n")))))

  (it "preserves CRLF line endings on the disk write-back path"
    (projectile-test-with-project
        (("crlf.txt" . "foo\r\nbar\r\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "baz")))
        (projectile-replace-review-test--apply buf)
        ;; raw bytes: CRLF must survive, not be normalized to LF
        (expect (projectile-test-disk-raw "crlf.txt")
                :to-equal "baz\r\nbar\r\n"))))

  (it "keeps applying the remaining files when one file's write fails"
    (projectile-test-with-project
        (("g.txt" . "foo\n")
         ("h.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; make applying g.txt error; h.txt must still be replaced
        (let ((orig (symbol-function 'projectile-replace--apply-file)))
          (cl-letf (((symbol-function 'projectile-replace--apply-file)
                     (lambda (file matches replacement literal)
                       (if (string-suffix-p "g.txt" file)
                           (error "boom")
                         (funcall orig file matches replacement literal)))))
            (projectile-replace-review-test--apply buf)))
        (expect (projectile-test-disk "h.txt")
                :to-equal "bar\n"))))

  (it "does not force-save a buffer that had unsaved edits"
    (projectile-test-with-project
        (("u.txt" . "foo mid\n"))
      (projectile-test-use-plain-grep)
      (find-file-noselect (expand-file-name "u.txt"))
      (with-current-buffer (get-file-buffer (expand-file-name "u.txt"))
        (goto-char (point-max))
        (insert "EXTRA\n"))
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (projectile-replace-review-test--apply buf)
        ;; buffer edited, but left modified (not saved) so its other unsaved
        ;; change isn't silently persisted; disk still original
        (with-current-buffer (get-file-buffer (expand-file-name "u.txt"))
          (expect (buffer-string) :to-equal "bar mid\nEXTRA\n")
          (expect (buffer-modified-p) :to-be-truthy))
        (expect (projectile-test-disk "u.txt")
                :to-equal "foo mid\n")))))

(describe "projectile-replace-review case-sensitivity toggle"
  (it "flips which matches are found and re-renders"
    (projectile-test-with-project
        (("case.txt" . "Foo foo FOO\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          ;; `case-fold-search' is t in the sandbox, so all three case
          ;; variants match to start with
          (expect projectile-replace--case-fold :to-be-truthy)
          (expect (length projectile-replace--matches) :to-equal 3)
          (projectile-replace--toggle-case)
          (expect projectile-replace--case-fold :to-be nil)
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect (projectile-replace--match-string
                   (car projectile-replace--matches))
                  :to-equal "foo")
          ;; toggling back restores the case-insensitive match set
          (projectile-replace--toggle-case)
          (expect (length projectile-replace--matches) :to-equal 3)))))

  (it "applies only the case-sensitive survivors after a toggle"
    (projectile-test-with-project
        (("s.txt" . "Foo foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (projectile-replace--toggle-case))
        (projectile-replace-review-test--apply buf)
        ;; only the lowercase `foo' is replaced; `Foo' is left alone
        (expect (projectile-test-disk "s.txt")
                :to-equal "Foo bar\n"))))

  (it "still excludes ignored files on the case-insensitive fallback path"
    ;; the case-insensitive literal search scans the full file list rather
    ;; than the grep narrowing, so it must still honor .projectile ignores
    (projectile-test-with-project
        ((".projectile" . "-secret\n")
         ("keep.txt" . "FOO\n")
         ("secret/hide.txt" . "FOO\n"))
      (projectile-test-use-plain-grep)
      ;; case-insensitive (sandbox default) literal search for `foo' finds
      ;; `FOO' in keep.txt but never in the ignored secret/ tree
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect (cl-some (lambda (m)
                             (string-match-p
                              "secret"
                              (projectile-replace--match-file m)))
                           projectile-replace--matches)
                  :to-be nil))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "keep.txt")
                :to-equal "bar\n")
        (expect (projectile-test-disk "secret/hide.txt")
                :to-equal "FOO\n"))))

  (it "re-enables all matches when re-scanning (documented reset)"
    (projectile-test-with-project
        (("e.txt" . "foo foo foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          ;; disable one match, then toggle case: re-scan rebuilds the list,
          ;; so every match comes back enabled (the documented behavior)
          (setf (projectile-replace--match-enabled
                 (car projectile-replace--matches))
                nil)
          (projectile-replace--toggle-case)
          (projectile-replace--toggle-case)
          (expect (cl-every #'projectile-replace--match-enabled
                            projectile-replace--matches)
                  :to-be-truthy))))))

(describe "projectile-replace-review regexp/literal toggle"
  (it "re-scans and changes the count for a term with metacharacters"
    (projectile-test-with-project
        (("meta.txt" . "a.b axb a.b\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "a.b" "Z")))
        (with-current-buffer buf
          (expect projectile-replace--literal :to-be-truthy)
          ;; literal: only the two real `a.b' occurrences
          (expect (length projectile-replace--matches) :to-equal 2)
          (projectile-replace--toggle-regexp)
          (expect projectile-replace--literal :to-be nil)
          (expect projectile-replace--search :to-equal "a.b")
          ;; regexp: the `.' now also matches the `x' in `axb'
          (expect (length projectile-replace--matches) :to-equal 3)))))

  (it "refuses an invalid regexp and stays literal without erroring"
    (projectile-test-with-project
        (("br.txt" . "foo[bar\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo[" "X")))
        (with-current-buffer buf
          (expect projectile-replace--literal :to-be-truthy)
          (expect (length projectile-replace--matches) :to-equal 1)
          ;; `foo[' is not a valid regexp; the toggle must not error
          (projectile-replace--toggle-regexp)
          (expect projectile-replace--literal :to-be-truthy)
          (expect (length projectile-replace--matches) :to-equal 1))))))

(describe "projectile-replace-review match filtering"
  (it "keeps only matches whose line matches and applies just those"
    (projectile-test-with-project
        (("f.txt" . "keep foo here\ndrop foo there\nkeep foo again\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 3)
          (projectile-replace--keep-matches "keep")
          (expect (length projectile-replace--matches) :to-equal 2)
          (expect projectile-replace--filtered :to-be-truthy))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "f.txt")
                :to-equal "keep bar here\ndrop foo there\nkeep bar again\n"))))

  (it "flushes matches whose line matches and applies the survivors"
    (projectile-test-with-project
        (("f.txt" . "keep foo here\ndrop foo there\nkeep foo again\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (projectile-replace--flush-matches "drop")
          (expect (length projectile-replace--matches) :to-equal 2))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "f.txt")
                :to-equal "keep bar here\ndrop foo there\nkeep bar again\n"))))

  (it "restores a filtered-away match on re-search"
    (projectile-test-with-project
        (("f.txt" . "keep foo here\ndrop foo there\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 2)
          (projectile-replace--flush-matches "drop")
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect projectile-replace--filtered :to-be-truthy)
          ;; re-search gathers from scratch, bringing the flushed match back
          (projectile-replace--refresh)
          (expect (length projectile-replace--matches) :to-equal 2)
          (expect projectile-replace--filtered :to-be nil))))))

(describe "projectile-replace-review file filtering"
  (it "keeps only matches whose file matches the regexp"
    (projectile-test-with-project
        (("keep.txt" . "foo\n")
         ("lib/skip.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 2)
          (projectile-replace--keep-files "keep")
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect (file-name-nondirectory
                   (projectile-replace--match-file
                    (car projectile-replace--matches)))
                  :to-equal "keep.txt"))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "keep.txt")
                :to-equal "bar\n")
        (expect (projectile-test-disk "lib/skip.txt")
                :to-equal "foo\n"))))

  (it "flushes matches whose project-relative path matches the regexp"
    (projectile-test-with-project
        (("keep.txt" . "foo\n")
         ("lib/skip.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          ;; matches the directory component of the relative path
          (projectile-replace--flush-files "lib/")
          (expect (length projectile-replace--matches) :to-equal 1)
          (expect (file-name-nondirectory
                   (projectile-replace--match-file
                    (car projectile-replace--matches)))
                  :to-equal "keep.txt"))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-test-disk "lib/skip.txt")
                :to-equal "foo\n")))))

(describe "projectile-replace-review status header"
  (it "reflects the mode flags and updates when they toggle"
    (projectile-test-with-project
        (("h.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (let ((header (projectile-replace-review-test--header buf)))
          (expect header :to-match "Replace \"foo\" with \"bar\"")
          (expect header :to-match "\\[literal\\]")
          (expect header :to-match "\\[ignore-case\\]"))
        (with-current-buffer buf (projectile-replace--toggle-case))
        (expect (projectile-replace-review-test--header buf)
                :to-match "\\[case-sensitive\\]")
        (with-current-buffer buf (projectile-replace--toggle-regexp))
        (expect (projectile-replace-review-test--header buf)
                :to-match "\\[regexp\\]"))))

  (it "shows (none) when there is no replacement yet"
    (projectile-test-with-project
        (("h.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "")))
        (expect (projectile-replace-review-test--header buf)
                :to-match "with (none)"))))

  (it "notes when the list has been filtered"
    (projectile-test-with-project
        (("h.txt" . "keep foo\ndrop foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (expect (projectile-replace-review-test--header buf)
                :not :to-match "filtered")
        (with-current-buffer buf (projectile-replace--flush-matches "drop"))
        (expect (projectile-replace-review-test--header buf)
                :to-match "filtered")))))

(defun projectile-replace-review-test--export (buf)
  "Export results buffer BUF to a grep buffer and return that grep buffer."
  (with-current-buffer buf
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (projectile-replace--export))))

(describe "projectile-replace-review export to grep-mode"
  (it "renders the shown matches as a navigable grep-mode buffer"
    (projectile-test-with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-test-use-plain-grep)
      (let* ((buf (projectile-replace-review-test--run "foo" "bar"))
             (gbuf (projectile-replace-review-test--export buf)))
        (unwind-protect
            (with-current-buffer gbuf
              ;; it is a real grep-mode buffer rooted at the project
              (expect major-mode :to-equal 'grep-mode)
              (expect (file-truename default-directory)
                      :to-equal (file-truename
                                 (buffer-local-value
                                  'projectile-replace--root buf)))
              ;; every shown match appears as a RELPATH:LINE: grep hit
              (let ((text (buffer-string)))
                (expect text :to-match "^a\\.txt:1:foo one foo$")
                (expect text :to-match "^lib/b\\.txt:1:start foo end$"))
              ;; the lines are recognized as grep hits, not inert text
              (goto-char (point-min))
              (compilation-next-error 1)
              (expect (get-text-property (point) 'compilation-message)
                      :not :to-be nil)
              (expect (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
                      :to-match "\\`a\\.txt:1:"))
          (kill-buffer gbuf)))))

  (it "exports only the filtered (shown) matches, not the removed ones"
    (projectile-test-with-project
        (("keep.txt" . "keep foo\n")
         ("drop.txt" . "drop foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf (projectile-replace--flush-files "drop"))
        (let ((gbuf (projectile-replace-review-test--export buf)))
          (unwind-protect
              (with-current-buffer gbuf
                (let ((text (buffer-string)))
                  (expect text :to-match "^keep\\.txt:1:")
                  (expect text :not :to-match "drop\\.txt")))
            (kill-buffer gbuf))))))

  (it "errors when there are no matches to export"
    (projectile-test-with-project
        (("z.txt" . "foo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (setq projectile-replace--matches nil)
          (expect (projectile-replace--export) :to-throw 'user-error)))))

  (it "excludes matches toggled off, matching what apply would do"
    (projectile-test-with-project
        (("e.txt" . "foo\nfoo\nfoo\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          ;; disable the first (line 1) match
          (setf (projectile-replace--match-enabled
                 (car projectile-replace--matches))
                nil))
        (let ((gbuf (projectile-replace-review-test--export buf)))
          (unwind-protect
              (with-current-buffer gbuf
                (let ((text (buffer-string)))
                  ;; the disabled line-1 match is gone; the enabled ones remain
                  (expect text :not :to-match "^e\\.txt:1:")
                  (expect text :to-match "^e\\.txt:2:")
                  (expect text :to-match "^e\\.txt:3:")))
            (kill-buffer gbuf))))))

  (it "does not create a phantom grep hit for a numeric term like 10:30"
    (projectile-test-with-project
        (("t.txt" . "before 10:30 after\n"))
      (projectile-test-use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "10:30" "NOON")))
        (let ((gbuf (projectile-replace-review-test--export buf)))
          (unwind-protect
              (with-current-buffer gbuf
                ;; the header carries no colon, so the term can't parse as a
                ;; bogus `file:line:' hit; the first real hit is t.txt
                (expect (buffer-string) :not :to-match "Projectile replace:")
                (goto-char (point-min))
                (compilation-next-error 1)
                (expect (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))
                        :to-match "\\`t\\.txt:1:"))
            (kill-buffer gbuf)))))))

(describe "projectile-replace export guidance"
  (it "points at wgrep when wgrep-change-to-wgrep-mode is available"
    (cl-letf (((symbol-function 'wgrep-change-to-wgrep-mode) #'ignore))
      (expect (projectile-replace--export-guidance) :to-match "C-c C-p")))

  (it "points at grep-edit-mode when only that is available"
    (cl-letf (((symbol-function 'wgrep-change-to-wgrep-mode) nil)
              ((symbol-function 'grep-edit-mode) #'ignore))
      (expect (projectile-replace--export-guidance) :to-match "grep-edit-mode")))

  (it "falls back to a read-only note pointing at MELPA otherwise"
    (cl-letf (((symbol-function 'wgrep-change-to-wgrep-mode) nil)
              ((symbol-function 'grep-edit-mode) nil))
      (let ((msg (projectile-replace--export-guidance)))
        (expect msg :to-match "read-only")
        (expect msg :to-match "MELPA")))))

;;; projectile-replace-review-test.el ends here
