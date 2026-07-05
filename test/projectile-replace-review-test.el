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

(defun projectile-replace-review-test--kill-project-buffers (root)
  "Kill all buffers visiting files under ROOT, discarding modifications."
  (dolist (buffer (buffer-list))
    (when-let* ((file (buffer-file-name buffer)))
      (when (string-prefix-p root (file-truename file))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (let (kill-buffer-query-functions)
          (kill-buffer buffer)))))
  (when-let* ((buf (get-buffer projectile-replace-buffer-name)))
    (kill-buffer buf)))

(defmacro projectile-replace-review-test--with-project (files &rest body)
  "Evaluate BODY in a sandbox project containing FILES.

FILES is a literal alist of (NAME . CONTENT) file specs created relative
to the project root.  A `.projectile' marker is created; include one in
FILES to override its contents (e.g. to add ignore rules).  BODY runs
with `default-directory' at the root and `projectile-project-root'
stubbed accordingly.  Project buffers and the results buffer are killed
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
         (projectile-replace-review-test--kill-project-buffers default-directory)))))

(defun projectile-replace-review-test--use-plain-grep ()
  "Force `projectile-files-with-string' to shell out to plain grep."
  (assume (projectile-unixy-system-p) "needs unixy text utilities")
  (spy-on 'executable-find :and-call-fake
          (lambda (command &rest _)
            (member command '("grep" "cut" "uniq")))))

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

(defun projectile-replace-review-test--disk (file)
  "Return the on-disk contents of project FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (buffer-string)))

(defun projectile-replace-review-test--disk-raw (file)
  "Return the raw (unconverted) on-disk bytes of project FILE as a string."
  (with-temp-buffer
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents (expand-file-name file)))
    (buffer-string)))

(defun projectile-replace-review-test--apply (buf)
  "Apply the enabled matches in results buffer BUF."
  (with-current-buffer buf
    (projectile-replace--apply)))

(describe "projectile-replace-review (literal)"
  (it "finds every literal match and applies them to a file on disk"
    (projectile-replace-review-test--with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-replace-review-test--use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 3))
        (projectile-replace-review-test--apply buf)
        ;; the files were never opened, so this proves the disk write-back
        (expect (get-file-buffer (expand-file-name "a.txt")) :to-be nil)
        (expect (projectile-replace-review-test--disk "a.txt")
                :to-equal "bar one bar\n")
        (expect (projectile-replace-review-test--disk "lib/b.txt")
                :to-equal "start bar end\n"))))

  (it "applies multiple matches on one line in descending order"
    (projectile-replace-review-test--with-project
        (("m.txt" . "xx xx xx\n"))
      (projectile-replace-review-test--use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "xx" "yyy")))
        (with-current-buffer buf
          (expect (length projectile-replace--matches) :to-equal 3))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-replace-review-test--disk "m.txt")
                :to-equal "yyy yyy yyy\n"))))

  (it "edits an already-open buffer in place rather than the file on disk"
    (projectile-replace-review-test--with-project
        (("open.txt" . "first foo\nlast foo\n"))
      (projectile-replace-review-test--use-plain-grep)
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
        (expect (projectile-replace-review-test--disk "open.txt")
                :to-equal "first bar\nlast bar\n"))))

  (it "leaves a disabled match untouched and applies only the enabled ones"
    (projectile-replace-review-test--with-project
        (("t.txt" . "foo foo foo\n"))
      (projectile-replace-review-test--use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        (with-current-buffer buf
          ;; disable the first of the three matches
          (setf (projectile-replace--match-enabled
                 (car projectile-replace--matches))
                nil))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-replace-review-test--disk "t.txt")
                :to-equal "foo bar bar\n")))))

(describe "projectile-replace-regexp-review"
  (it "honors Emacs-only regexp constructs a shell regexp couldn't express"
    (projectile-replace-review-test--with-project
        ;; \_< \_> are Emacs symbol boundaries: grep/rg cannot express them
        (("s.txt" . "foo foobar barfoo foo\n"))
      (let ((buf (projectile-replace-review-test--run "\\_<foo\\_>" "X" 'regexp)))
        (with-current-buffer buf
          ;; only the two standalone `foo' symbols match
          (expect (length projectile-replace--matches) :to-equal 2))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-replace-review-test--disk "s.txt")
                :to-equal "X foobar barfoo X\n"))))

  (it "substitutes capture groups in the replacement"
    (projectile-replace-review-test--with-project
        (("c.txt" . "foo_bar and baz_qux\n"))
      (let ((buf (projectile-replace-review-test--run
                  "\\([a-z]+\\)_\\([a-z]+\\)" "\\2-\\1" 'regexp)))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-replace-review-test--disk "c.txt")
                :to-equal "bar-foo and qux-baz\n"))))

  (it "excludes files ignored via .projectile"
    (projectile-replace-review-test--with-project
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
        (expect (projectile-replace-review-test--disk "keep.txt")
                :to-equal "bar\n")
        (expect (projectile-replace-review-test--disk "secret/hide.txt")
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
    (projectile-replace-review-test--with-project
        (("a.txt" . "hello foo world\nsecond foo line\n"))
      (projectile-replace-review-test--use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; the file changes on disk between scan and apply; stale positions
        ;; must NOT be applied to the shifted text
        (with-temp-file (expand-file-name "a.txt")
          (insert "PREPENDED LINE\nhello foo world\nsecond foo line\n"))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-replace-review-test--disk "a.txt")
                :to-equal
                "PREPENDED LINE\nhello foo world\nsecond foo line\n"))))

  (it "does not hang scanning a regexp that matches empty at end of buffer"
    (projectile-replace-review-test--with-project
        (("z.txt" . "aaa\nbbb\n"))
      ;; `^' matches the empty string at every line start, including at
      ;; end-of-buffer; the scan must terminate instead of spinning forever
      (let ((result (projectile-replace--gather
                     (list (expand-file-name "z.txt")) "^")))
        (expect (plist-member result :matches) :to-be-truthy))))

  (it "applies via disk when the scanned buffer was killed before apply"
    (projectile-replace-review-test--with-project
        (("k.txt" . "foo here\n"))
      (projectile-replace-review-test--use-plain-grep)
      (find-file-noselect (expand-file-name "k.txt"))
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; the scan tagged matches with the live buffer; kill it before apply
        (let (kill-buffer-query-functions)
          (kill-buffer (get-file-buffer (expand-file-name "k.txt"))))
        (projectile-replace-review-test--apply buf)
        (expect (projectile-replace-review-test--disk "k.txt")
                :to-equal "bar here\n"))))

  (it "edits the buffer, not disk, for a file opened and modified after scan"
    (projectile-replace-review-test--with-project
        (("o.txt" . "foo tail\n"))
      (projectile-replace-review-test--use-plain-grep)
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
          (expect (projectile-replace-review-test--disk "o.txt")
                  :to-equal "foo tail\n")))))

  (it "preserves CRLF line endings on the disk write-back path"
    (projectile-replace-review-test--with-project
        (("crlf.txt" . "foo\r\nbar\r\n"))
      (projectile-replace-review-test--use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "baz")))
        (projectile-replace-review-test--apply buf)
        ;; raw bytes: CRLF must survive, not be normalized to LF
        (expect (projectile-replace-review-test--disk-raw "crlf.txt")
                :to-equal "baz\r\nbar\r\n"))))

  (it "keeps applying the remaining files when one file's write fails"
    (projectile-replace-review-test--with-project
        (("g.txt" . "foo\n")
         ("h.txt" . "foo\n"))
      (projectile-replace-review-test--use-plain-grep)
      (let ((buf (projectile-replace-review-test--run "foo" "bar")))
        ;; make applying g.txt error; h.txt must still be replaced
        (let ((orig (symbol-function 'projectile-replace--apply-file)))
          (cl-letf (((symbol-function 'projectile-replace--apply-file)
                     (lambda (file matches replacement literal)
                       (if (string-suffix-p "g.txt" file)
                           (error "boom")
                         (funcall orig file matches replacement literal)))))
            (projectile-replace-review-test--apply buf)))
        (expect (projectile-replace-review-test--disk "h.txt")
                :to-equal "bar\n"))))

  (it "does not force-save a buffer that had unsaved edits"
    (projectile-replace-review-test--with-project
        (("u.txt" . "foo mid\n"))
      (projectile-replace-review-test--use-plain-grep)
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
        (expect (projectile-replace-review-test--disk "u.txt")
                :to-equal "foo mid\n")))))

;;; projectile-replace-review-test.el ends here
