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
  (seq-find (lambda (finding) (string-match-p pattern (cdr finding)))
            (projectile-doctor--findings data)))

(describe "projectile-doctor findings"
  (it "suggests installing fd when it's missing under alien indexing"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :fd missing :projectile-mode t)
                    "fd is not installed")))
      (expect (car finding) :to-be 'warn))
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :fd present :projectile-mode t)
                    "fd is installed")))
      (expect (car finding) :to-be 'ok)))

  (it "flags a large project with caching disabled"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method native :file-count 20000
                      :caching nil :projectile-mode t)
                    "caching")))
      (expect (car finding) :to-be 'warn))
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
      (expect (car finding) :to-be 'warn)))

  (it "flags a remote project indexed synchronously"
    (let ((finding (projectile-doctor-test--finding
                    '(:indexing-method alien :remote "/ssh:host:"
                      :async-indexing nil :projectile-mode t)
                    "Remote project")))
      (expect (car finding) :to-be 'warn))
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
      (expect (car (projectile-doctor-test--finding data "without a"))
              :to-be 'warn)
      (expect (car (projectile-doctor-test--finding data "keep"))
              :to-be 'info)))

  (it "warns when the project type wasn't detected"
    (expect (car (projectile-doctor-test--finding
                  '(:type generic :indexing-method native :projectile-mode t)
                  "type not detected"))
            :to-be 'warn)
    (expect (car (projectile-doctor-test--finding
                  '(:type emacs-eldev :indexing-method native :projectile-mode t)
                  "type detected"))
            :to-be 'ok)))

(provide 'projectile-doctor-test)

;;; projectile-doctor-test.el ends here
