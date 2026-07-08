;;; projectile-session-test.el --- Tests for per-project session tabs -*- lexical-binding: t -*-

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

;; Tests for `projectile-session-mode' and its tab-per-project behavior.

;;; Code:

(require 'projectile-test-helpers)
(require 'tab-bar)

(defun projectile-session-test--reset-tabs ()
  "Reset the selected frame to a single, unowned tab."
  (set-frame-parameter nil 'tabs nil)
  ;; Recreate a fresh single current-tab.
  (tab-bar-tabs))

(defun projectile-session-test--populate ()
  "Stand-in populate action; spied on in tests."
  nil)

(defun projectile-session-test--tab-names ()
  "Return the names of all open tabs on the selected frame."
  (mapcar (lambda (tab) (alist-get 'name (cdr tab))) (tab-bar-tabs)))

(describe "projectile-session-mode"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (when projectile-session-mode
      (projectile-session-mode -1))
    (projectile-session-test--reset-tabs))

  (describe "wiring"
    (it "installs and restores the switch-project action"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        (expect projectile-switch-project-action
                :to-equal #'projectile-session-switch-project-action)
        (projectile-session-mode -1)
        (expect projectile-switch-project-action :to-equal 'projectile-find-file)
        (expect projectile-session--saved-switch-action :to-be nil)))

    (it "leaves a user-changed action alone on disable"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        ;; the user rebinds the action while the mode is on
        (setq projectile-switch-project-action 'projectile-dired)
        (projectile-session-mode -1)
        (expect projectile-switch-project-action :to-equal 'projectile-dired)))

    (it "does not lose the saved action when re-enabled while already on"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        ;; a second enable must not capture our own action as the saved value
        (projectile-session-mode 1)
        (projectile-session-mode -1)
        (expect projectile-switch-project-action :to-equal 'projectile-find-file))))

  (describe "adoption on enable"
    (it "stamps the current tab with the current project"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value "/x/proj/")
        (projectile-session-mode 1)
        (let ((tab (projectile-session--current-tab)))
          (expect (projectile-session--tab-root tab) :to-equal "/x/proj/")
          (expect (alist-get 'name (cdr tab)) :to-equal "proj"))))

    (it "does nothing when not inside a project"
      (let ((projectile-switch-project-action 'projectile-find-file))
        (spy-on 'projectile-project-root :and-return-value nil)
        (projectile-session-mode 1)
        (expect (projectile-session--tab-root (projectile-session--current-tab))
                :to-be nil)))))

(describe "projectile-session-switch-project-action"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs))

  (it "creates and populates a new tab on first switch"
    (let ((projectile-session-default-action 'projectile-session-test--populate))
      (spy-on 'projectile-session-test--populate)
      (spy-on 'projectile-project-root :and-return-value "/one/foo/")
      (projectile-session-switch-project-action)
      (expect (length (projectile-session--project-tabs)) :to-equal 1)
      (expect (projectile-session--tab-root (projectile-session--current-tab))
              :to-equal "/one/foo/")
      (expect 'projectile-session-test--populate :to-have-been-called)))

  (it "selects the existing tab instead of re-populating on a repeat switch"
    (let ((projectile-session-default-action 'projectile-session-test--populate)
          (current-root nil))
      (spy-on 'projectile-session-test--populate)
      (spy-on 'projectile-project-root
              :and-call-fake (lambda (&optional _dir) current-root))
      ;; open foo, then bar; now the current tab is bar's
      (setq current-root "/one/foo/")
      (projectile-session-switch-project-action)
      (setq current-root "/two/bar/")
      (projectile-session-switch-project-action)
      (expect (projectile-session--tab-root (projectile-session--current-tab))
              :to-equal "/two/bar/")
      ;; switching back to foo must select its tab, not create/populate again
      (spy-calls-reset 'projectile-session-test--populate)
      (setq current-root "/one/foo/")
      (projectile-session-switch-project-action)
      (expect (projectile-session--tab-root (projectile-session--current-tab))
              :to-equal "/one/foo/")
      (expect 'projectile-session-test--populate :not :to-have-been-called)
      (expect (length (projectile-session--project-tabs)) :to-equal 2)))

  (it "binds default-directory to the project root while populating"
    (let ((projectile-session-default-action 'projectile-session-test--populate)
          (seen-dir nil))
      (spy-on 'projectile-session-test--populate
              :and-call-fake (lambda () (setq seen-dir default-directory)))
      (spy-on 'projectile-project-root :and-return-value "/one/foo/")
      (projectile-session-switch-project-action)
      (expect seen-dir :to-equal "/one/foo/"))))

(describe "projectile-session tab naming"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs))

  (it "names a lone project tab after the project"
    (projectile-session--make-project-tab "/solo/bar/")
    (expect (projectile-session--tab-root (projectile-session--current-tab))
            :to-equal "/solo/bar/")
    (expect (alist-get 'name (cdr (projectile-session--current-tab)))
            :to-equal "bar"))

  (it "disambiguates same-named projects with a parent component"
    (projectile-session--make-project-tab "/work/foo/")
    (projectile-session--make-project-tab "/home/foo/")
    (let ((names (projectile-session-test--tab-names)))
      (expect names :to-contain "work/foo")
      (expect names :to-contain "home/foo")))

  (it "adds a second parent component when the first also clashes"
    (projectile-session--make-project-tab "/p/shared/foo/")
    (projectile-session--make-project-tab "/q/shared/foo/")
    (let ((names (projectile-session-test--tab-names)))
      (expect names :to-contain "p/shared/foo")
      (expect names :to-contain "q/shared/foo")))

  (it "leaves a hand-renamed tab alone when refreshing names"
    (projectile-session--make-project-tab "/work/foo/")
    ;; the user renames the tab by hand (only the name, not the auto-name param)
    (setf (alist-get 'name (cdr (projectile-session--current-tab))) "MyWork")
    ;; opening another same-named project refreshes names, but must skip the
    ;; hand-renamed tab
    (projectile-session--make-project-tab "/home/foo/")
    (let ((names (projectile-session-test--tab-names)))
      (expect names :to-contain "MyWork")
      (expect names :not :to-contain "work/foo")))

  (it "honors a custom tab-name function"
    (let ((projectile-session-tab-name-function
           (lambda (root) (concat "P:" (directory-file-name root)))))
      (projectile-session--make-project-tab "/some/thing/")
      (expect (alist-get 'name (cdr (projectile-session--current-tab)))
              :to-equal "P:/some/thing"))))

(describe "projectile-session-switch-to-buffer"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs))

  (it "completes over just the current tab's project buffers"
    (let ((buf-a (get-buffer-create "session-a"))
          (buf-b (get-buffer-create "session-b")))
      (unwind-protect
          (progn
            (projectile-session--set-tab-root
             (projectile-session--current-tab) "/proj/")
            (spy-on 'projectile-project-buffers
                    :and-return-value (list buf-a buf-b))
            (spy-on 'projectile-completing-read
                    :and-call-fake (lambda (_prompt choices &rest _) (car choices)))
            (spy-on 'switch-to-buffer)
            (projectile-session-switch-to-buffer)
            (expect 'projectile-project-buffers :to-have-been-called-with "/proj/")
            (expect 'switch-to-buffer :to-have-been-called-with "session-a"))
        (kill-buffer buf-a)
        (kill-buffer buf-b))))

  (it "falls back to plain switch-to-buffer with no project tab"
    (spy-on 'projectile-project-buffers)
    (spy-on 'call-interactively)
    (projectile-session-switch-to-buffer)
    (expect 'projectile-project-buffers :not :to-have-been-called)
    (expect 'call-interactively :to-have-been-called-with #'switch-to-buffer)))

(defvar projectile-session-test--dir nil
  "Temporary directory holding session files during a test.")

(defun projectile-session-test--make-dir ()
  "Create and return a fresh temporary session directory."
  (make-temp-file "projectile-session-test" t))

(defun projectile-session-test--session-files (data)
  "Return the list of :file paths recorded in session DATA."
  (delq nil (mapcar (lambda (buf) (plist-get (cdr buf) :file))
                    (plist-get data :buffers))))

(describe "projectile-session file naming"
  (it "keys files by root, stable and collision-free"
    (let ((projectile-session-directory "/state/sessions/"))
      (spy-on 'projectile-session--project-name
              :and-call-fake (lambda (root) (file-name-nondirectory
                                             (directory-file-name root))))
      (let ((foo1 (projectile-session--file "/a/foo/"))
            (foo1-again (projectile-session--file "/a/foo/"))
            (foo2 (projectile-session--file "/b/foo/")))
        ;; same root -> same file
        (expect foo1 :to-equal foo1-again)
        ;; same project name, different root -> different file
        (expect foo1 :not :to-equal foo2)
        ;; readable project-name prefix and the session directory are used
        (expect (file-name-directory foo1) :to-equal "/state/sessions/")
        (expect (string-prefix-p "foo-" (file-name-nondirectory foo1))
                :to-be t))))

  (it "sanitizes unsafe characters in the project name"
    (let ((projectile-session-directory "/state/sessions/"))
      (spy-on 'projectile-session--project-name
              :and-return-value "we ird/name")
      (let ((file (file-name-nondirectory (projectile-session--file "/x/y/"))))
        (expect (string-match-p "[ /]" file) :to-be nil)))))

(describe "projectile-session buffer serializers"
  (it "round-trips a file-visiting buffer with point"
    (let ((tmp (make-temp-file "projectile-session-file" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "hello world\nsecond line\n"))
            (let* ((buf (find-file-noselect tmp)))
              (unwind-protect
                  (let (saved)
                    (with-current-buffer buf (goto-char 5))
                    (setq saved (projectile-session--serialize-buffer buf))
                    (expect (car saved) :to-be t)
                    (expect (plist-get (cdr saved) :file) :to-equal tmp)
                    (expect (plist-get (cdr saved) :point) :to-equal 5)
                    ;; round-trip must be readable and recreate the buffer
                    (let ((round (read (prin1-to-string saved))))
                      (kill-buffer buf)
                      (let ((restored (projectile-session--recreate-buffer round)))
                        (expect (buffer-live-p restored) :to-be t)
                        (expect (buffer-file-name restored) :to-equal tmp)
                        (with-current-buffer restored
                          (expect (point) :to-equal 5))
                        (kill-buffer restored))))
                (when (buffer-live-p buf) (kill-buffer buf)))))
        (delete-file tmp))))

  (it "recreates a file buffer without interactive prompts"
    (let ((tmp (make-temp-file "projectile-session-file" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "x\n"))
            (let (seen-lfwt seen-elv)
              (spy-on 'find-file-noselect :and-call-fake
                      (lambda (&rest _)
                        (setq seen-lfwt large-file-warning-threshold
                              seen-elv enable-local-variables)
                        (get-buffer-create " *psession-fake*")))
              (projectile-session--deserialize-file (list :file tmp))
              ;; large-file warnings and unsafe file-locals must be suppressed,
              ;; so restoring a session never blocks on a prompt
              (expect seen-lfwt :to-be nil)
              (expect seen-elv :to-be :safe)))
        (when (get-buffer " *psession-fake*") (kill-buffer " *psession-fake*"))
        (delete-file tmp))))

  (it "round-trips a dired buffer via its directory"
    (let* ((dir (make-temp-file "projectile-session-dired" t))
           (buf (dired-noselect dir)))
      (unwind-protect
          (let ((saved (projectile-session--serialize-buffer buf)))
            (expect (car saved) :to-equal 'dired-mode)
            (expect (plist-get (cdr saved) :dir)
                    :to-equal (file-name-as-directory (expand-file-name dir)))
            (kill-buffer buf)
            (let ((restored (projectile-session--recreate-buffer
                             (read (prin1-to-string saved)))))
              (expect (buffer-live-p restored) :to-be t)
              (with-current-buffer restored
                (expect (derived-mode-p 'dired-mode) :to-be-truthy))
              (kill-buffer restored)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-directory dir t))))

  (it "recreates nothing when a saved file is gone"
    (let ((saved (cons t (list :file "/no/such/file.txt" :point 1))))
      (expect (projectile-session--recreate-buffer saved) :to-be nil)))

  (it "honors a custom serializer for a non-file buffer"
    (let* ((projectile-session-buffer-serializers
            (cons '(special-mode
                    . (projectile-session-test--ser-special
                       . projectile-session-test--deser-special))
                  projectile-session-buffer-serializers))
           (buf (get-buffer-create "session-special")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (special-mode)
              (setq-local projectile-session-test--payload "abc"))
            (cl-letf (((symbol-function 'projectile-session-test--ser-special)
                       (lambda (_buffer)
                         (list :payload projectile-session-test--payload)))
                      ((symbol-function 'projectile-session-test--deser-special)
                       (lambda (record)
                         (let ((b (get-buffer-create "session-special")))
                           (with-current-buffer b
                             (setq-local projectile-session-test--payload
                                         (plist-get record :payload)))
                           b))))
              (let ((saved (projectile-session--serialize-buffer buf)))
                (expect (car saved) :to-equal 'special-mode)
                (expect (plist-get (cdr saved) :payload) :to-equal "abc")
                (let ((restored (projectile-session--recreate-buffer
                                 (read (prin1-to-string saved)))))
                  (with-current-buffer restored
                    (expect projectile-session-test--payload :to-equal "abc"))))))
        (when (buffer-live-p buf) (kill-buffer buf)))))

  (it "skips a buffer no serializer handles"
    (let ((buf (get-buffer-create "session-plain")))
      (unwind-protect
          (progn
            (with-current-buffer buf (fundamental-mode))
            (expect (projectile-session--serialize-buffer buf) :to-be nil))
        (kill-buffer buf))))

  (it "isolates a throwing serializer and falls through to the next entry"
    (let ((tmp (make-temp-file "projectile-session-throw" nil ".txt")))
      (let* ((projectile-session-buffer-serializers
              (cons (cons 'text-mode
                          (cons #'projectile-session-test--ser-boom #'ignore))
                    projectile-session-buffer-serializers))
             (buf (find-file-noselect tmp)))
        (unwind-protect
            (cl-letf (((symbol-function 'projectile-session-test--ser-boom)
                       (lambda (_b) (error "boom"))))
              (with-current-buffer buf (text-mode))
              ;; the text-mode entry throws; must fall through to the `t' handler
              (let ((saved (projectile-session--serialize-buffer buf)))
                (expect (car saved) :to-be t)
                (expect (plist-get (cdr saved) :file) :to-equal tmp)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (delete-file tmp)))))

  (it "drops a record that can't be read back"
    (let* ((projectile-session-buffer-serializers
            (list (cons 'fundamental-mode
                        (cons #'projectile-session-test--ser-live #'ignore))))
           (buf (get-buffer-create "session-live")))
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-session-test--ser-live)
                     ;; embeds a live buffer object, which prin1/read can't round-trip
                     (lambda (b) (list :live b))))
            (with-current-buffer buf (fundamental-mode))
            (expect (projectile-session--serialize-buffer buf) :to-be nil))
        (kill-buffer buf))))

  (it "round-trips a predicate-keyed serializer"
    (let* ((projectile-session-buffer-serializers
            (list (cons (lambda (b)
                          (with-current-buffer b (derived-mode-p 'special-mode)))
                        (cons #'projectile-session-test--ser-pred
                              #'projectile-session-test--deser-pred))))
           (buf (get-buffer-create "session-pred")))
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-session-test--ser-pred)
                     (lambda (_b) (list :tag "hi")))
                    ((symbol-function 'projectile-session-test--deser-pred)
                     (lambda (_record) (get-buffer-create "session-pred-restored"))))
            (with-current-buffer buf (special-mode))
            (let ((saved (projectile-session--serialize-buffer buf)))
              ;; a predicate key stores the buffer's major mode as the kind
              (expect (car saved) :to-equal 'special-mode)
              ;; ...and restore must still resolve the predicate entry's deserializer
              (let ((restored (projectile-session--recreate-buffer
                               (read (prin1-to-string saved)))))
                (expect (buffer-live-p restored) :to-be t))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (get-buffer "session-pred-restored")
          (kill-buffer "session-pred-restored"))))))

(describe "projectile-session save and restore"
  (before-each
    (setq projectile-session-test--dir (projectile-session-test--make-dir))
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs)
    (when (and projectile-session-test--dir
               (file-directory-p projectile-session-test--dir))
      (delete-directory projectile-session-test--dir t)))

  (it "writes a readable versioned sexp and restores the layout"
    (let ((tmp1 (make-temp-file "projectile-session-a" nil ".txt"))
          (tmp2 (make-temp-file "projectile-session-b" nil ".txt")))
      (unwind-protect
          (let* ((projectile-session-directory projectile-session-test--dir)
                 (buf1 (find-file-noselect tmp1))
                 (buf2 (find-file-noselect tmp2)))
            (unwind-protect
                (progn
                  ;; lay out two windows, one per file
                  (delete-other-windows)
                  (switch-to-buffer buf1)
                  (split-window)
                  (other-window 1)
                  (switch-to-buffer buf2)
                  (expect (projectile-session-save "/proj/root/") :to-be-truthy)
                  ;; on-disk data is a readable, versioned plist
                  (let* ((file (projectile-session--file "/proj/root/"))
                         (data (with-temp-buffer
                                 (insert-file-contents file)
                                 (read (buffer-string)))))
                    (expect (plist-get data :projectile-session-version)
                            :to-equal projectile-session--format-version)
                    (expect (plist-get data :root) :to-equal "/proj/root/")
                    (expect (length (plist-get data :buffers)) :to-equal 2))
                  ;; tear the layout down, then restore it
                  (kill-buffer buf1)
                  (kill-buffer buf2)
                  (delete-other-windows)
                  (switch-to-buffer "*scratch*")
                  (expect (projectile-session-restore "/proj/root/") :to-be-truthy)
                  (let ((names (mapcar (lambda (w)
                                         (buffer-file-name (window-buffer w)))
                                       (window-list nil 'nomini))))
                    (expect names :to-contain tmp1)
                    (expect names :to-contain tmp2)))
              (dolist (b (list buf1 buf2))
                (when (buffer-live-p b) (kill-buffer b)))
              (dolist (n (list (file-name-nondirectory tmp1)
                               (file-name-nondirectory tmp2)))
                (when (get-buffer n) (kill-buffer n)))))
        (delete-file tmp1)
        (delete-file tmp2))))

  (it "survives a corrupt window layout by falling back to the recreated buffers"
    (let ((tmp (make-temp-file "projectile-session-corrupt" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "x\n"))
            ;; a data blob that passes the version check but carries a bogus
            ;; :window-state must not abort the whole restore
            (spy-on 'projectile-session--read :and-return-value
                    (list :buffers (list (cons t (list :file tmp :point 1)))
                          :window-state '(this is not a valid window state)))
            ;; a throw here would fail the test; the fix demotes it to a message
            (expect (projectile-session-restore "/proj/root/") :to-be t))
        (when (get-buffer (file-name-nondirectory tmp))
          (kill-buffer (file-name-nondirectory tmp)))
        (delete-file tmp))))

  (it "skips a session file from an incompatible version with a message"
    (projectile-test-with-temp-files ((file ".eld"))
      (projectile-serialize
       (list :projectile-session-version
             (1+ projectile-session--format-version)
             :buffers nil)
       file)
      (spy-on 'message)
      (expect (projectile-session--read-file file) :to-be nil)
      (expect 'message :to-have-been-called)))

  (it "does not error restoring a layout whose file is gone"
    (let ((tmp (make-temp-file "projectile-session-gone" nil ".txt")))
      (let* ((projectile-session-directory projectile-session-test--dir)
             (buf (find-file-noselect tmp)))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer buf)
              (projectile-session-save "/gone/root/")
              (kill-buffer buf)
              (delete-file tmp)
              (switch-to-buffer "*scratch*")
              ;; the file the layout points at is gone; restore must not error,
              ;; and returns nil (nothing recreated) so a caller can fall back
              (expect (projectile-session-restore "/gone/root/") :to-be nil))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (get-buffer (file-name-nondirectory tmp))
            (kill-buffer (file-name-nondirectory tmp)))
          (when (file-exists-p tmp) (delete-file tmp))))))

  (it "saves nothing for a degenerate layout"
    (let ((projectile-session-directory projectile-session-test--dir))
      (delete-other-windows)
      (switch-to-buffer "*scratch*")
      (expect (projectile-session-save "/empty/root/") :to-be nil)
      (expect (projectile-session--saved-p "/empty/root/") :to-be nil)))

  (it "reports failure when the session file cannot be written"
    ;; point the session directory *under a regular file* so make-directory and
    ;; the write fail; save must return nil rather than lying about success
    (let ((blocker (make-temp-file "projectile-session-blocker"))
          (tmp (make-temp-file "projectile-session-w" nil ".txt")))
      (let* ((projectile-session-directory (expand-file-name "sub" blocker))
             (buf (find-file-noselect tmp)))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer buf)
              (expect (projectile-session-save "/blocked/root/") :to-be nil))
          (when (buffer-live-p buf) (kill-buffer buf))
          (delete-file tmp)
          (delete-file blocker)))))

  (it "placeholders a dead pane in a split layout without erroring"
    (let* ((projectile-session-directory projectile-session-test--dir)
           (t1 (make-temp-file "projectile-session-mix-a" nil ".txt"))
           (t2 (make-temp-file "projectile-session-mix-b" nil ".txt"))
           (b1 (find-file-noselect t1))
           (b2 (find-file-noselect t2)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer b1)
            (split-window-right)
            (other-window 1)
            (switch-to-buffer b2)
            (projectile-session-save "/mix/root/")
            ;; only t2 goes away; t1 stays recreatable
            (kill-buffer b2) (delete-file t2)
            (kill-buffer b1)
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            ;; t1 recreatable so restore proceeds; the dead t2 pane is sanitized
            ;; to a placeholder and window-state-put must not error
            (expect (projectile-session-restore "/mix/root/") :to-be t))
        (when (buffer-live-p b1) (kill-buffer b1))
        (when (buffer-live-p b2) (kill-buffer b2))
        (when (file-exists-p t1) (delete-file t1))
        (when (file-exists-p t2) (delete-file t2)))))

  (it "forgets a saved session by deleting its file"
    (let ((tmp (make-temp-file "projectile-session-forget" nil ".txt")))
      (let* ((projectile-session-directory projectile-session-test--dir)
             (buf (find-file-noselect tmp)))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer buf)
              (projectile-session-save "/forget/root/")
              (expect (projectile-session--saved-p "/forget/root/") :to-be t)
              (projectile-session-forget "/forget/root/")
              (expect (projectile-session--saved-p "/forget/root/") :to-be nil))
          (when (buffer-live-p buf) (kill-buffer buf))
          (delete-file tmp))))))

(describe "projectile-session restore-on-switch"
  (before-each
    (setq projectile-session-test--dir (projectile-session-test--make-dir))
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs)
    (when (and projectile-session-test--dir
               (file-directory-p projectile-session-test--dir))
      (delete-directory projectile-session-test--dir t)))

  (it "restores instead of populating when a session exists on disk"
    (let ((projectile-session-directory projectile-session-test--dir)
          (projectile-session-restore-on-switch t)
          (projectile-session-default-action 'projectile-session-test--populate))
      (spy-on 'projectile-session-test--populate)
      (spy-on 'projectile-project-root :and-return-value "/switch/proj/")
      (spy-on 'projectile-session--saved-p :and-return-value t)
      (spy-on 'projectile-session-restore :and-return-value t)
      (projectile-session-switch-project-action)
      (expect 'projectile-session-restore
              :to-have-been-called-with "/switch/proj/")
      (expect 'projectile-session-test--populate :not :to-have-been-called)))

  (it "populates when restore-on-switch is off"
    (let ((projectile-session-directory projectile-session-test--dir)
          (projectile-session-restore-on-switch nil)
          (projectile-session-default-action 'projectile-session-test--populate))
      (spy-on 'projectile-session-test--populate)
      (spy-on 'projectile-project-root :and-return-value "/switch/proj/")
      (spy-on 'projectile-session--saved-p :and-return-value t)
      (spy-on 'projectile-session-restore)
      (projectile-session-switch-project-action)
      (expect 'projectile-session-restore :not :to-have-been-called)
      (expect 'projectile-session-test--populate :to-have-been-called)))

  (it "populates when there is no saved session"
    (let ((projectile-session-directory projectile-session-test--dir)
          (projectile-session-restore-on-switch t)
          (projectile-session-default-action 'projectile-session-test--populate))
      (spy-on 'projectile-session-test--populate)
      (spy-on 'projectile-project-root :and-return-value "/switch/proj/")
      (spy-on 'projectile-session--saved-p :and-return-value nil)
      (spy-on 'projectile-session-restore)
      (projectile-session-switch-project-action)
      (expect 'projectile-session-restore :not :to-have-been-called)
      (expect 'projectile-session-test--populate :to-have-been-called))))

(describe "projectile-session autosave wiring"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (when projectile-session-mode
      (projectile-session-mode -1))
    (projectile-session-test--reset-tabs))

  (it "adds and removes the autosave hooks with the mode"
    (let ((projectile-switch-project-action 'projectile-find-file)
          (projectile-before-switch-project-hook nil)
          (kill-emacs-hook nil))
      (spy-on 'projectile-project-root :and-return-value nil)
      (projectile-session-mode 1)
      (expect (memq 'projectile-session--maybe-autosave
                    projectile-before-switch-project-hook)
              :to-be-truthy)
      (expect (memq 'projectile-session--autosave-on-kill kill-emacs-hook)
              :to-be-truthy)
      (projectile-session-mode -1)
      (expect (memq 'projectile-session--maybe-autosave
                    projectile-before-switch-project-hook)
              :to-be nil)
      (expect (memq 'projectile-session--autosave-on-kill kill-emacs-hook)
              :to-be nil)))

  (it "does not stack duplicate hooks on a double enable"
    (let ((projectile-switch-project-action 'projectile-find-file)
          (projectile-before-switch-project-hook nil)
          (kill-emacs-hook nil))
      (spy-on 'projectile-project-root :and-return-value nil)
      (projectile-session-mode 1)
      (projectile-session-mode 1)
      (expect (length (delq nil (mapcar
                                 (lambda (h)
                                   (eq h 'projectile-session--maybe-autosave))
                                 projectile-before-switch-project-hook)))
              :to-equal 1)))

  (it "autosaves the outgoing project only when enabled"
    (let ((projectile-session-autosave nil))
      (spy-on 'projectile-session-save)
      (projectile-session--maybe-autosave)
      (expect 'projectile-session-save :not :to-have-been-called)
      (setq projectile-session-autosave t)
      (projectile-session--maybe-autosave)
      (expect 'projectile-session-save :to-have-been-called)))

  (it "keeps saving the other tabs when one can't be selected on kill"
    (let ((projectile-session-autosave t)
          (saved '()))
      (spy-on 'projectile-session--current-tab-index :and-return-value 1)
      (spy-on 'tab-bar-select-tab)
      (spy-on 'projectile-session--project-tabs
              :and-return-value (list 'tab-a 'tab-b 'tab-c))
      (spy-on 'projectile-session--tab-root
              :and-call-fake (lambda (tab) (format "/%s/" tab)))
      (spy-on 'projectile-session--select-tab-by-root
              :and-call-fake (lambda (root)
                               (if (equal root "/tab-b/")
                                   (error "cannot select tab-b")
                                 t)))
      (spy-on 'projectile-session-save
              :and-call-fake (lambda (root) (push root saved) t))
      ;; tab-b throws on select, but that must not abandon tab-a and tab-c
      (projectile-session--autosave-on-kill)
      (expect saved :to-contain "/tab-a/")
      (expect saved :to-contain "/tab-c/")
      (expect saved :not :to-contain "/tab-b/"))))

(describe "projectile-session-save-all"
  (before-each
    (setq projectile-session-test--dir (projectile-session-test--make-dir))
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs)
    (when (and projectile-session-test--dir
               (file-directory-p projectile-session-test--dir))
      (delete-directory projectile-session-test--dir t)))

  ;; Real-tab test: each project tab must be saved with ITS OWN layout, and
  ;; the user restored to the tab they started on.  This is what a mocked spec
  ;; can't catch: `tab-bar-select-tab' rebuilds cons cells as it switches, so
  ;; selecting by a captured cons mid-loop saved the wrong tab's layout.
  (it "saves each project tab's own layout and returns to the starting tab"
    (let* ((projectile-session-directory projectile-session-test--dir)
           (ta (make-temp-file "projectile-session-all-a" nil ".txt"))
           (tb (make-temp-file "projectile-session-all-b" nil ".txt"))
           (root-a "/proj/alpha/")
           (root-b "/proj/beta/")
           (ba (find-file-noselect ta))
           (bb (find-file-noselect tb)))
      (unwind-protect
          (progn
            ;; tab A shows file A, tab B shows file B; B is current
            (projectile-session--make-project-tab root-a)
            (delete-other-windows) (switch-to-buffer ba)
            (projectile-session--make-project-tab root-b)
            (delete-other-windows) (switch-to-buffer bb)
            (expect (projectile-session-save-all) :to-equal 2)
            (let ((files-a (projectile-session-test--session-files
                            (projectile-session--read root-a)))
                  (files-b (projectile-session-test--session-files
                            (projectile-session--read root-b))))
              (expect files-a :to-contain ta)
              (expect files-a :not :to-contain tb)
              (expect files-b :to-contain tb)
              (expect files-b :not :to-contain ta))
            ;; started save-all on tab B, must end up back on tab B
            (expect (projectile-session--tab-root (projectile-session--current-tab))
                    :to-equal root-b))
        (when (buffer-live-p ba) (kill-buffer ba))
        (when (buffer-live-p bb) (kill-buffer bb))
        (delete-file ta)
        (delete-file tb))))

  (it "skips a tab that errors without abandoning the rest"
    (let ((saved '()))
      (spy-on 'projectile-session--current-tab-index :and-return-value 1)
      (spy-on 'tab-bar-select-tab)
      (spy-on 'projectile-session--project-tabs
              :and-return-value (list 'tab-a 'tab-b 'tab-c))
      (spy-on 'projectile-session--tab-root
              :and-call-fake (lambda (tab) (format "/%s/" tab)))
      (spy-on 'projectile-session--select-tab-by-root
              :and-call-fake (lambda (root)
                               (if (equal root "/tab-b/")
                                   (error "cannot select tab-b")
                                 t)))
      (spy-on 'projectile-session-save
              :and-call-fake (lambda (root) (push root saved) t))
      ;; tab-b throws on select, but that must not abandon tab-a and tab-c
      (expect (projectile-session-save-all) :to-equal 2)
      (expect saved :to-contain "/tab-a/")
      (expect saved :to-contain "/tab-c/")
      (expect saved :not :to-contain "/tab-b/")))

  (it "does not count a tab whose layout has nothing to save"
    (spy-on 'projectile-session--current-tab-index :and-return-value 1)
    (spy-on 'tab-bar-select-tab)
    (spy-on 'projectile-session--project-tabs
            :and-return-value (list 'tab-a 'tab-b))
    (spy-on 'projectile-session--tab-root
            :and-call-fake (lambda (tab) (format "/%s/" tab)))
    (spy-on 'projectile-session--select-tab-by-root :and-return-value t)
    ;; tab-b has no serializable buffer, so `projectile-session-save' returns nil
    (spy-on 'projectile-session-save
            :and-call-fake (lambda (root) (equal root "/tab-a/")))
    (expect (projectile-session-save-all) :to-equal 1)))

(describe "projectile-session survivor re-simplify"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs))

  (it "reverts a survivor's name when its clashing sibling tab is closed"
    (let ((tab-bar-tab-pre-close-functions
           (list #'projectile-session--on-tab-close)))
      (projectile-session--make-project-tab "/work/foo/")
      (projectile-session--make-project-tab "/home/foo/")
      ;; both are disambiguated while they coexist
      (let ((names (projectile-session-test--tab-names)))
        (expect names :to-contain "work/foo")
        (expect names :to-contain "home/foo"))
      ;; close the current (home/foo) tab; the pre-close hook must re-simplify
      ;; the survivor even though the closing tab is still in the tab list
      (tab-bar-close-tab)
      (let ((names (projectile-session-test--tab-names)))
        (expect names :to-contain "foo")
        (expect names :not :to-contain "work/foo"))))

  (it "adds and removes the pre-close hook with the mode"
    (let ((projectile-switch-project-action 'projectile-find-file)
          (tab-bar-tab-pre-close-functions nil))
      (spy-on 'projectile-project-root :and-return-value nil)
      (projectile-session-mode 1)
      (expect (memq 'projectile-session--on-tab-close
                    tab-bar-tab-pre-close-functions)
              :to-be-truthy)
      (projectile-session-mode -1)
      (expect (memq 'projectile-session--on-tab-close
                    tab-bar-tab-pre-close-functions)
              :to-be nil)))

  (it "does not block a tab from closing when name refresh errors"
    (let ((tab-bar-tab-pre-close-functions
           (list #'projectile-session--on-tab-close)))
      ;; build the tabs with the normal namer first
      (projectile-session--set-tab-root
       (projectile-session--current-tab) "/work/foo/")
      (projectile-session--make-project-tab "/home/foo/")
      (let ((count (length (tab-bar-tabs)))
            ;; now make the pre-close name refresh signal
            (projectile-session-tab-name-function
             (lambda (_root) (error "boom in naming"))))
        (tab-bar-close-tab)
        (expect (length (tab-bar-tabs)) :to-equal (1- count))))))

(describe "projectile-session--saved-roots"
  (before-each
    (setq projectile-session-test--dir (projectile-session-test--make-dir)))

  (after-each
    (when (and projectile-session-test--dir
               (file-directory-p projectile-session-test--dir))
      (delete-directory projectile-session-test--dir t)))

  (it "collects saved roots in a stable order, skipping junk files"
    (let ((tmp (make-temp-file "projectile-session-roots" nil ".txt")))
      (let* ((projectile-session-directory projectile-session-test--dir)
             (buf (find-file-noselect tmp)))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer buf)
              ;; two real, current-version session files
              (projectile-session-save "/proj/zeta/")
              (projectile-session-save "/proj/alpha/")
              ;; a file that isn't a readable session: must be skipped
              (with-temp-file (expand-file-name "garbage.eld"
                                                projectile-session-test--dir)
                (insert "not ( a valid sexp"))
              (expect (projectile-session--saved-roots)
                      :to-equal '("/proj/alpha/" "/proj/zeta/")))
          (when (buffer-live-p buf) (kill-buffer buf))
          (delete-file tmp)))))

  (it "returns nil when the session directory is absent"
    (let ((projectile-session-directory
           (expand-file-name "does-not-exist/" projectile-session-test--dir)))
      (expect (projectile-session--saved-roots) :to-be nil)))

  (it "skips a version-current file whose root is not a string"
    (let ((projectile-session-directory projectile-session-test--dir))
      ;; a corrupt/hand-edited file with a non-string :root must not crash the
      ;; sort (which would silently disable all restore-on-startup)
      (with-temp-file (expand-file-name "corrupt.eld" projectile-session-test--dir)
        (prin1 (list :projectile-session-version projectile-session--format-version
                     :root 42 :buffers nil :window-state nil)
               (current-buffer)))
      (expect (projectile-session--saved-roots) :to-be nil))))

(describe "projectile-session-restore-all"
  (before-each
    (setq projectile-session-test--dir (projectile-session-test--make-dir))
    (projectile-session-test--reset-tabs))

  (after-each
    (projectile-session-test--reset-tabs)
    (when (and projectile-session-test--dir
               (file-directory-p projectile-session-test--dir))
      (delete-directory projectile-session-test--dir t)))

  ;; Real-tab test: restore-all must reopen each saved session into its own
  ;; tab, land the user on the first restored project, and put the right
  ;; file back in each tab.  Building actual tabs (not mocked symbols) is
  ;; what catches a cons-based selection going stale mid-loop.
  (it "reopens each saved session into its own tab"
    (let* ((projectile-session-directory projectile-session-test--dir)
           (ta (make-temp-file "projectile-session-ra-a" nil ".txt"))
           (tb (make-temp-file "projectile-session-ra-b" nil ".txt"))
           (root-a "/proj/alpha/")
           (root-b "/proj/beta/")
           (ba (find-file-noselect ta))
           (bb (find-file-noselect tb)))
      (unwind-protect
          (progn
            ;; write a one-file session for each project
            (delete-other-windows)
            (switch-to-buffer ba)
            (projectile-session-save root-a)
            (switch-to-buffer bb)
            (projectile-session-save root-b)
            ;; tear everything down so restore genuinely recreates
            (kill-buffer ba) (kill-buffer bb)
            (projectile-session-test--reset-tabs)
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (expect (projectile-session-restore-all) :to-equal 2)
            ;; a tab per project now exists
            (expect (projectile-session--project-tab root-a) :to-be-truthy)
            (expect (projectile-session--project-tab root-b) :to-be-truthy)
            ;; landed on the first restored (sorted) project, alpha
            (expect (projectile-session--tab-root
                     (projectile-session--current-tab))
                    :to-equal root-a)
            ;; each tab holds its own file
            (projectile-session--select-tab-by-root root-a)
            (expect (mapcar (lambda (w) (buffer-file-name (window-buffer w)))
                            (window-list nil 'nomini))
                    :to-contain ta)
            (projectile-session--select-tab-by-root root-b)
            (expect (mapcar (lambda (w) (buffer-file-name (window-buffer w)))
                            (window-list nil 'nomini))
                    :to-contain tb))
        (when (buffer-live-p ba) (kill-buffer ba))
        (when (buffer-live-p bb) (kill-buffer bb))
        (dolist (n (list (file-name-nondirectory ta)
                         (file-name-nondirectory tb)))
          (when (get-buffer n) (kill-buffer n)))
        (delete-file ta)
        (delete-file tb))))

  (it "skips a project whose tab is already open, without duplicating it"
    (let* ((projectile-session-directory projectile-session-test--dir)
           (ta (make-temp-file "projectile-session-ra-open" nil ".txt"))
           (root-a "/proj/open/")
           (ba (find-file-noselect ta)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer ba)
            (projectile-session-save root-a)
            (kill-buffer ba)
            (projectile-session-test--reset-tabs)
            ;; pre-open a tab for root-a; restore-all must leave it be
            (projectile-session--make-project-tab root-a)
            (expect (projectile-session-restore-all) :to-equal 0)
            ;; exactly one tab is bound to root-a (no duplicate)
            (expect (length (seq-filter
                             (lambda (tab)
                               (projectile-session--same-root-p
                                (projectile-session--tab-root tab) root-a))
                             (tab-bar-tabs)))
                    :to-equal 1))
        (when (buffer-live-p ba) (kill-buffer ba))
        (when (get-buffer (file-name-nondirectory ta))
          (kill-buffer (file-name-nondirectory ta)))
        (delete-file ta))))

  (it "closes the empty tab and skips a stale session whose files are gone"
    (let* ((projectile-session-directory projectile-session-test--dir)
           (ta (make-temp-file "projectile-session-ra-stale" nil ".txt"))
           (root-a "/proj/stale/")
           (ba (find-file-noselect ta)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer ba)
            (projectile-session-save root-a)
            ;; the project's file is gone: nothing can be recreated
            (kill-buffer ba)
            (delete-file ta)
            (projectile-session-test--reset-tabs)
            (switch-to-buffer "*scratch*")
            (let ((before (length (tab-bar-tabs))))
              (expect (projectile-session-restore-all) :to-equal 0)
              ;; no leftover empty tab, and none bound to the stale root
              (expect (length (tab-bar-tabs)) :to-equal before)
              (expect (projectile-session--project-tab root-a) :to-be nil)))
        (when (buffer-live-p ba) (kill-buffer ba))
        (when (get-buffer (file-name-nondirectory ta))
          (kill-buffer (file-name-nondirectory ta)))
        (when (file-exists-p ta) (delete-file ta)))))

  (it "restores around a stale middle session and lands on the first live one"
    (let* ((projectile-session-directory projectile-session-test--dir)
           (ta (make-temp-file "projectile-session-ra-mid-a" nil ".txt"))
           (tc (make-temp-file "projectile-session-ra-mid-c" nil ".txt"))
           (tb (make-temp-file "projectile-session-ra-mid-b" nil ".txt"))
           ;; sorted order is a < b < c; b will be the stale one in the middle
           (root-a "/proj/aaa/")
           (root-b "/proj/bbb/")
           (root-c "/proj/ccc/")
           (ba (find-file-noselect ta))
           (bb (find-file-noselect tb))
           (bc (find-file-noselect tc)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer ba) (projectile-session-save root-a)
            (switch-to-buffer bb) (projectile-session-save root-b)
            (switch-to-buffer bc) (projectile-session-save root-c)
            ;; b's file disappears; a and c stay restorable
            (kill-buffer ba) (kill-buffer bb) (kill-buffer bc)
            (delete-file tb)
            (projectile-session-test--reset-tabs)
            (switch-to-buffer "*scratch*")
            (expect (projectile-session-restore-all) :to-equal 2)
            ;; a and c reopened, the stale b left no tab
            (expect (projectile-session--project-tab root-a) :to-be-truthy)
            (expect (projectile-session--project-tab root-c) :to-be-truthy)
            (expect (projectile-session--project-tab root-b) :to-be nil)
            ;; landed on the first live project in sorted order, a
            (expect (projectile-session--tab-root
                     (projectile-session--current-tab))
                    :to-equal root-a))
        (dolist (b (list ba bb bc)) (when (buffer-live-p b) (kill-buffer b)))
        (dolist (n (list (file-name-nondirectory ta)
                         (file-name-nondirectory tc)))
          (when (get-buffer n) (kill-buffer n)))
        (when (file-exists-p ta) (delete-file ta))
        (when (file-exists-p tc) (delete-file tc)))))

  (it "reports the count when called interactively"
    (spy-on 'projectile-session--saved-roots :and-return-value nil)
    (spy-on 'message)
    (call-interactively #'projectile-session-restore-all)
    (expect 'message :to-have-been-called-with
            "Restored %d project session%s" 0 "s")))

(describe "projectile-session restore-on-startup"
  (before-each
    (projectile-session-test--reset-tabs))

  (after-each
    (when projectile-session-mode
      (projectile-session-mode -1))
    (projectile-session-test--reset-tabs))

  (it "runs restore-all only when restore-on-startup is set"
    (let ((projectile-session-restore-on-startup nil))
      (spy-on 'projectile-session-restore-all)
      (projectile-session--maybe-restore-on-startup)
      (expect 'projectile-session-restore-all :not :to-have-been-called)
      (setq projectile-session-restore-on-startup t)
      (projectile-session--maybe-restore-on-startup)
      (expect 'projectile-session-restore-all :to-have-been-called)))

  (it "swallows an error out of the startup handler"
    (let ((projectile-session-restore-on-startup t))
      (spy-on 'projectile-session-restore-all
              :and-call-fake (lambda () (error "boom")))
      ;; must not propagate the error (would abort `emacs-startup-hook')
      (expect (projectile-session--maybe-restore-on-startup) :not :to-throw)))

  (it "adds and removes the startup hook with the mode"
    (let ((projectile-switch-project-action 'projectile-find-file)
          (emacs-startup-hook nil))
      (spy-on 'projectile-project-root :and-return-value nil)
      (projectile-session-mode 1)
      (expect (memq 'projectile-session--maybe-restore-on-startup
                    emacs-startup-hook)
              :to-be-truthy)
      ;; a double enable must not stack duplicates
      (projectile-session-mode 1)
      (expect (length (delq nil (mapcar
                                 (lambda (h)
                                   (eq h 'projectile-session--maybe-restore-on-startup))
                                 emacs-startup-hook)))
              :to-equal 1)
      (projectile-session-mode -1)
      (expect (memq 'projectile-session--maybe-restore-on-startup
                    emacs-startup-hook)
              :to-be nil))))

(describe "projectile-session keybindings"
  (it "binds the w sub-prefix to the session commands"
    (expect (lookup-key projectile-command-map (kbd "w s"))
            :to-equal #'projectile-session-save)
    (expect (lookup-key projectile-command-map (kbd "w S"))
            :to-equal #'projectile-session-save-all)
    (expect (lookup-key projectile-command-map (kbd "w r"))
            :to-equal #'projectile-session-restore)
    (expect (lookup-key projectile-command-map (kbd "w R"))
            :to-equal #'projectile-session-restore-all)
    (expect (lookup-key projectile-command-map (kbd "w f"))
            :to-equal #'projectile-session-forget)
    (expect (lookup-key projectile-command-map (kbd "w b"))
            :to-equal #'projectile-session-switch-to-buffer)))

(provide 'projectile-session-test)

;;; projectile-session-test.el ends here
