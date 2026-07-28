;;; projectile-doctor-test.el --- Tests for projectile-doctor -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov

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

;; Tests for `projectile-doctor' - the report rendering, its behavior
;; outside a project, its hands-off treatment of the file cache, and the
;; findings logic (which is driven off a plain plist and so can be tested
;; without a project at all).

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-doctor-test--render (data)
  "Render DATA and return the report as a string."
  (with-temp-buffer
    (projectile-doctor--render data)
    (buffer-string)))

(describe "projectile-doctor"
  (it "renders a report for the current project"
    (projectile-test-with-stub-root "project/" ("src/a.el" ".projectile")
      (let ((projectile-indexing-method 'native)
            (projectile-enable-caching nil))
        (unwind-protect
            (progn
              (projectile-doctor)
              (with-current-buffer projectile-doctor-buffer-name
                (expect major-mode :to-be 'projectile-doctor-mode)
                (expect buffer-read-only :to-be t)
                (let ((report (buffer-string)))
                  (dolist (section '("Project" "Type" "Indexing"
                                     "Files" "Ignores" "Findings"))
                    (expect report :to-match section))
                  (expect report :to-match (projectile-project-root))
                  (expect report :to-match "detected by")
                  (expect report :to-match "fresh index"))))
          (kill-buffer projectile-doctor-buffer-name)))))

  (it "reports which root function found the project"
    (projectile-test-with-sandbox
      (projectile-test-with-files ("project/.projectile" "project/src/a.el")
        (let* ((dir (file-truename (expand-file-name "project/src/")))
               (data (projectile-doctor--collect dir)))
          (expect (plist-get data :root-function) :to-be 'projectile-root-marked)
          (expect (plist-get data :root-marker) :to-equal ".projectile")))))

  (it "degrades gracefully outside a project"
    (projectile-test-with-sandbox
      (projectile-test-with-files ("nothing/a.el")
        (spy-on 'projectile-project-root :and-return-value nil)
        (let ((report (projectile-doctor-test--render
                       (projectile-doctor--collect
                        (expand-file-name "nothing/")))))
          (expect report :to-match "not inside a project")
          (expect report :to-match "projectile-root-bottom-up")
          (expect report :to-match "\\.projectile")))))

  (it "leaves the file cache exactly as it found it"
    (projectile-test-with-stub-root "project/" ("src/a.el")
      (let ((projectile-indexing-method 'native)
            (projectile-enable-caching t))
        (projectile-doctor--collect (projectile-project-root))
        (expect (gethash (projectile-project-root) projectile-projects-cache)
                :to-be nil)
        (expect (gethash (projectile-project-root) projectile-projects-cache-time)
                :to-be nil))))

  (it "uses the cached file list when there is one"
    (projectile-test-with-stub-root "project/" ("src/a.el")
      (puthash (projectile-project-root) '("one" "two" "three")
               projectile-projects-cache)
      (let ((data (projectile-doctor--collect (projectile-project-root))))
        (expect (plist-get data :files-source) :to-be 'cache)
        (expect (plist-get data :file-count) :to-equal 3)))))

(defun projectile-doctor-test--finding (data pattern)
  "Return DATA's finding whose text matches PATTERN, if any."
  (seq-find (lambda (finding)
              (string-match-p pattern (plist-get finding :message)))
            (projectile-doctor--findings data)))

(defun projectile-doctor-test--severity (finding)
  "Return FINDING's severity."
  (plist-get finding :severity))

(describe "projectile-doctor findings"
  (it "suggests installing fd when it's missing under alien indexing"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :fd missing :projectile-mode t)
                    "fd is not installed")))
      (expect (projectile-doctor-test--severity finding) :to-be 'warn))
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :fd present :projectile-mode t)
                    "fd is installed")))
      (expect (projectile-doctor-test--severity finding) :to-be 'ok)))

  (it "flags a large project with caching disabled"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method native :file-count 20000
                      :caching nil :projectile-mode t)
                    "caching")))
      (expect (projectile-doctor-test--severity finding) :to-be 'warn))
    (expect (projectile-doctor-test--finding
             '(:indexing-method native :file-count 20000
               :caching t :projectile-mode t)
             "caching")
            :to-be nil))

  (it "flags a huge project regardless of caching"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :file-count 100000
                      :caching t :projectile-mode t)
                    "That's a lot")))
      (expect (projectile-doctor-test--severity finding) :to-be 'warn)))

  (it "flags a remote project indexed synchronously"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :remote "/ssh:host:"
                      :async-indexing nil :projectile-mode t)
                    "Remote project")))
      (expect (projectile-doctor-test--severity finding) :to-be 'warn))
    (expect (projectile-doctor-test--finding
             '(:indexing-method alien :remote "/ssh:host:"
               :async-indexing t :projectile-mode t)
             "Remote project")
            :to-be nil))

  (it "flags prefix-less dirconfig entries and reports keep entries"
    (let* ((cfg (make-projectile-dirconfig :keep '("src/")
                                           :ignore '("tmp")
                                           :prefixless-ignore '("tmp")))
           (data (list :indexing-method 'native :projectile-mode t
                       :dirconfig cfg)))
      (expect (projectile-doctor-test--severity (projectile-doctor-test--finding data "without a"))
              :to-be 'warn)
      (expect (projectile-doctor-test--severity (projectile-doctor-test--finding data "keep"))
              :to-be 'info)))

  (it "warns when the project type wasn't detected"
    (expect (projectile-doctor-test--severity (projectile-doctor-test--finding
                  '(:type generic :indexing-method native :projectile-mode t)
                  "type not detected"))
            :to-be 'warn)
    (expect (projectile-doctor-test--severity (projectile-doctor-test--finding
                  '(:type emacs-eldev :indexing-method native :projectile-mode t)
                  "type detected"))
            :to-be 'ok)))

(describe "actionable findings"
  (it "offers to enable projectile-mode when it is off"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method native :projectile-mode nil)
                    "projectile-mode. is not enabled")))
      (expect (plist-get finding :action-label) :to-equal "enable")
      (spy-on 'projectile-mode)
      (funcall (plist-get finding :action))
      (expect 'projectile-mode :to-have-been-called-with 1)))

  (it "offers to enable caching on a large uncached project"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method native :file-count 20000
                      :caching nil :projectile-mode t)
                    "caching")))
      (expect (plist-get finding :action-label) :to-equal "enable caching")
      (spy-on 'customize-set-variable)
      (funcall (plist-get finding :action))
      (expect 'customize-set-variable
              :to-have-been-called-with 'projectile-enable-caching t)))

  (it "offers to open the dirconfig behind a prefix-less-lines warning"
    (projectile-test-with-stub-root "proj" (".projectile")
      (let* ((cfg (make-projectile-dirconfig :prefixless-ignore t))
             (finding (projectile-doctor-test--finding
                       (list :indexing-method 'native :projectile-mode t
                             :dirconfig cfg)
                       "without a")))
        (expect (plist-get finding :action-label) :to-equal "open dirconfig")
        (spy-on 'find-file)
        (funcall (plist-get finding :action))
        (expect 'find-file :to-have-been-called-with
                (expand-file-name ".projectile" (projectile-project-root))))))

  (it "leaves a finding you cannot act on without a button"
    ;; Projectile can't install fd for you, so that one stays advice.
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :fd missing :projectile-mode t)
                    "fd is not installed")))
      (expect (plist-get finding :action) :to-be nil))
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method native :file-count 5 :projectile-mode t)
                    "5 files indexed")))
      (expect (plist-get finding :action) :to-be nil)))

  (it "renders an action as a button and runs it on RET"
    (let ((ran nil))
      (spy-on 'projectile-doctor--findings :and-return-value
              (list (projectile-doctor--finding
                     'warn "something is off" "fix it"
                     (lambda () (setq ran t)))))
      (with-temp-buffer
        (projectile-doctor-mode)
        (let ((inhibit-read-only t))
          (projectile-doctor--render '(:root "/proj/")))
        (goto-char (point-min))
        (expect (re-search-forward "\\[fix it\\]" nil t) :to-be-truthy)
        ;; the label is part of the report text, so a pasted report still
        ;; says what could be done about the finding
        (goto-char (point-min))
        (search-forward "fix it")
        (spy-on 'revert-buffer)
        (push-button (1- (point)))
        (expect ran :to-be t)
        ;; and the report regenerates, so the finding answers for itself
        (expect 'revert-buffer :to-have-been-called)))))

(describe "report rendering"
  (defun projectile-report-test--faces-at (regexp)
    "Return the face of the text matched by REGEXP in the current buffer."
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (get-text-property (match-beginning 0) 'face)))

  (describe "projectile--report-title"
    (it "faces the title and dims its underline"
      (with-temp-buffer
        (projectile--report-title "Hello")
        (expect (projectile-report-test--faces-at "Hello")
                :to-be 'projectile-report-section)
        ;; the rule is text, so a yanked report still reads the same...
        (expect (buffer-string) :to-equal "Hello\n=====\n")
        ;; ...but it recedes rather than competing with the title
        (expect (projectile-report-test--faces-at "=====")
                :to-be 'projectile-report-label))))

  (describe "projectile--report-status-face"
    (it "colors the status words whose polarity is unambiguous"
      (expect (projectile--report-status-face "on") :to-be 'projectile-report-ok)
      (expect (projectile--report-status-face "present") :to-be 'projectile-report-ok)
      (expect (projectile--report-status-face "missing") :to-be 'projectile-report-warning)
      (expect (projectile--report-status-face "off") :to-be 'projectile-report-info))

    (it "leaves anything else alone"
      (expect (projectile--report-status-face "alien") :to-be nil)
      (expect (projectile--report-status-face "git") :to-be nil)))

  (describe "projectile-doctor--sort-findings"
    (it "puts what wants action first, keeping each severity's own order"
      (expect (mapcar (lambda (f) (plist-get f :message))
                      (projectile-doctor--sort-findings
                       (list (projectile-doctor--finding 'ok "a")
                             (projectile-doctor--finding 'info "b")
                             (projectile-doctor--finding 'warn "c")
                             (projectile-doctor--finding 'ok "d")
                             (projectile-doctor--finding 'warn "e"))))
              :to-equal '("c" "e" "b" "a" "d"))))

  (describe "projectile-report-copy"
    (it "copies the report without its text properties"
      (with-temp-buffer
        (projectile-doctor-mode)
        (let ((inhibit-read-only t))
          (projectile--report-title "Projectile doctor report")
          (insert (propertize "root" 'face 'projectile-report-label))
          (insert-text-button "a button" 'action #'ignore))
        (let ((kill-ring nil))
          (projectile-report-copy)
          (let ((copied (current-kill 0)))
            (expect copied :to-equal "Projectile doctor report\n========================\nroota button")
            ;; nothing carries a face or a button into the clipboard
            (expect (text-properties-at 0 copied) :to-be nil)
            (expect (next-property-change 0 copied) :to-be nil)))))

    (it "refuses outside a report buffer"
      (with-temp-buffer
        (fundamental-mode)
        (expect (projectile-report-copy) :to-throw 'user-error))))

  (describe "the doctor buffer"
    (it "renders its findings colored by severity"
      (spy-on 'projectile-doctor--findings :and-return-value
              (list (projectile-doctor--finding 'warn "fd is not installed")
                    (projectile-doctor--finding 'ok "all good")))
      (with-temp-buffer
        (projectile-doctor--render '(:root "/proj/" :type 'npm))
        (expect (projectile-report-test--faces-at "^warn")
                :to-be 'projectile-report-warning)
        (expect (projectile-report-test--faces-at "^ok")
                :to-be 'projectile-report-ok)))

    (it "describes its keys by looking them up, not by hardcoding them"
      ;; `substitute-command-keys' means the footer keeps telling the truth
      ;; after a rebind, rather than advertising a key that no longer works.
      (spy-on 'projectile-doctor--findings :and-return-value nil)
      (with-temp-buffer
        (projectile-doctor-mode)
        (let ((inhibit-read-only t))
          (projectile-doctor--render '(:root "/proj/")))
        (expect (buffer-string) :to-match "w copy"))
      (with-temp-buffer
        (projectile-doctor-mode)
        (use-local-map (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map projectile-doctor-mode-map)
                         (define-key map (kbd "C-c C-w") #'projectile-report-copy)
                         (define-key map (kbd "w") nil)
                         map))
        (let ((inhibit-read-only t))
          (projectile-doctor--render '(:root "/proj/")))
        (expect (buffer-string) :to-match "C-c C-w copy")))))

(provide 'projectile-doctor-test)

;;; projectile-doctor-test.el ends here
