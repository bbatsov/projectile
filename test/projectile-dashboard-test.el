;;; projectile-dashboard-test.el --- Tests for projectile-dashboard -*- lexical-binding: t -*-

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

;; Tests for `projectile-dashboard' - the rendering, the buttons the
;; entries carry, its behavior outside a project, its hands-off
;; treatment of the file cache, and its use as a
;; `projectile-switch-project-action'.  The rendering is driven off a
;; plain plist, so most of it can be tested without a project at all.

;;; Code:

(require 'projectile-test-helpers)

(defun projectile-dashboard-test--render (data)
  "Render DATA and return the dashboard as a string."
  (with-temp-buffer
    (projectile-dashboard--render data)
    (buffer-string)))

(defmacro projectile-dashboard-test--with-render (data &rest body)
  "Render DATA into a temp buffer and evaluate BODY there."
  (declare (indent 1) (debug (form &rest form)))
  `(with-temp-buffer
     (projectile-dashboard--render ,data)
     (goto-char (point-min))
     ,@body))

(defun projectile-dashboard-test--button (label)
  "Return the button labelled LABEL in the current buffer, if any."
  (goto-char (point-min))
  (when (search-forward label nil t)
    (button-at (match-beginning 0))))

(describe "projectile-dashboard"
  (it "renders a dashboard for the current project"
    (projectile-test-with-stub-root "project/" ("src/a.el" ".projectile")
      (let ((projectile-dashboard-sections '(project recent tasks commands)))
        (unwind-protect
            (progn
              (save-window-excursion (projectile-dashboard))
              (with-current-buffer projectile-dashboard-buffer-name
                (expect major-mode :to-be 'projectile-dashboard-mode)
                (expect buffer-read-only :to-be t)
                (expect (file-equal-p default-directory
                                      (projectile-project-root))
                        :to-be-truthy)
                (let ((dashboard (buffer-string)))
                  (dolist (section '("Project" "Recent files" "Tasks"
                                     "Lifecycle commands"))
                    (expect dashboard :to-match section))
                  (expect dashboard
                          :to-match (regexp-quote (projectile-project-root))))))
          (kill-buffer projectile-dashboard-buffer-name)))))

  (it "renders the same thing again on revert"
    (projectile-test-with-stub-root "project/"
        (".projectile"
         ".dir-locals.el")
      (with-temp-file "project/.dir-locals.el"
        (insert "((nil . ((projectile-tasks . ((\"lint\" . \"make lint\"))))))"))
      (let ((projectile-dashboard-sections '(tasks))
            (enable-local-variables :all))
        (unwind-protect
            (let (initial)
              (save-window-excursion (projectile-dashboard))
              (with-current-buffer projectile-dashboard-buffer-name
                (setq initial (buffer-string))
                (expect initial :to-match "make lint")
                ;; The dashboard buffer has none of the project's
                ;; directory-local variables, so a naive refresh would
                ;; quietly lose the tasks defined there.
                (revert-buffer)
                (expect (buffer-string) :to-equal initial)))
          (kill-buffer projectile-dashboard-buffer-name)))))

  (it "renders only the enabled sections, in order"
    (let ((projectile-dashboard-sections '(tasks project))
          (data '(:root "/tmp/p/" :name "p" :type generic)))
      (let ((dashboard (projectile-dashboard-test--render data)))
        (expect dashboard :to-match "Tasks")
        (expect dashboard :to-match "Project")
        (expect dashboard :not :to-match "Recent files")
        (expect dashboard :not :to-match "Version control")
        (expect (string-match-p "^Tasks$" dashboard)
                :to-be-less-than (string-match-p "^Project$" dashboard)))))

  (it "degrades gracefully outside a project"
    (projectile-test-with-sandbox
      (projectile-test-with-files ("nothing/a.el")
        (spy-on 'projectile-project-root :and-return-value nil)
        (let* ((dir (expand-file-name "nothing/"))
               (data (projectile-dashboard--collect dir)))
          (expect (plist-get data :root) :to-be nil)
          (let ((dashboard (projectile-dashboard-test--render data)))
            (expect dashboard :to-match "not inside a project")
            (expect dashboard :to-match "\\.projectile")
            (expect dashboard :to-match "projectile-doctor"))))))

  (it "never indexes an uncached project"
    (projectile-test-with-stub-root "project/" ("src/a.el" "src/b.el")
      (let ((projectile-indexing-method 'native)
            (projectile-enable-caching t)
            (projectile-dashboard-sections '(project)))
        (spy-on 'projectile-project-files :and-call-through)
        (let ((data (projectile-dashboard--collect (projectile-project-root))))
          (expect 'projectile-project-files :not :to-have-been-called)
          (expect (plist-get data :file-count) :to-be nil)
          (expect (projectile-dashboard-test--render data)
                  :to-match "not indexed yet"))
        ;; and the caches are exactly as we found them
        (expect (gethash (projectile-project-root) projectile-projects-cache)
                :to-be nil)
        (expect (gethash (projectile-project-root)
                         projectile-projects-cache-time)
                :to-be nil))))

  (it "reports the file count when the project is cached"
    (projectile-test-with-stub-root "project/" ("src/a.el")
      (puthash (projectile-project-root) '("one" "two" "three")
               projectile-projects-cache)
      (let ((projectile-dashboard-sections '(project))
            (data (projectile-dashboard--collect (projectile-project-root))))
        (expect (plist-get data :file-count) :to-equal 3)
        (expect (projectile-dashboard-test--render data)
                :to-match (regexp-quote "3 (cached)")))))

  (it "skips the git query on a remote project"
    (spy-on 'projectile-project-root :and-return-value "/ssh:host:/p/")
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    ;; Reaching the remote host at all is the thing under test.
    (spy-on 'hack-dir-local-variables-non-file-buffer)
    (spy-on 'projectile-dashboard--git :and-call-fake
            (lambda (&rest _) (error "Must not shell out on a remote project")))
    (let* ((projectile--frecency-table (make-hash-table :test 'equal))
           (projectile-dashboard-sections '(vcs recent))
           (data (projectile-dashboard--collect "/ssh:host:/p/")))
      (expect (plist-get data :vcs-state) :to-be 'skipped)
      (expect (plist-get data :recent-files) :to-be nil))))

(describe "projectile-dashboard recent files"
  (it "ranks the project's files by frecency"
    (projectile-test-with-stub-root "project/" ("a.el" "b.el" "c.el")
      (let* ((root (projectile-project-root))
             (now (projectile-time-seconds))
             (files (make-hash-table :test 'equal))
             (projectile--frecency-table (make-hash-table :test 'equal))
             (projectile-enable-frecency t))
        (puthash root files projectile--frecency-table)
        (puthash "a.el" (cons 1 (- now 86400)) files)
        (puthash "b.el" (cons 50 now) files)
        (puthash "c.el" (cons 5 now) files)
        (expect (mapcar #'car (projectile-dashboard--recent-files root))
                :to-equal '("b.el" "c.el" "a.el")))))

  (it "drops files that no longer exist and honors the cap"
    (projectile-test-with-stub-root "project/" ("a.el" "b.el")
      (let* ((root (projectile-project-root))
             (now (projectile-time-seconds))
             (files (make-hash-table :test 'equal))
             (projectile--frecency-table (make-hash-table :test 'equal))
             (projectile-enable-frecency t)
             (projectile-dashboard-recent-files 2))
        (puthash root files projectile--frecency-table)
        ;; The top-ranked file is gone, so it has to be filtered out
        ;; rather than merely fall off the end of the cap.
        (puthash "gone.el" (cons 99 now) files)
        (puthash "a.el" (cons 9 now) files)
        (puthash "b.el" (cons 8 now) files)
        (expect (mapcar #'car (projectile-dashboard--recent-files root))
                :to-equal '("a.el" "b.el")))))

  (it "says so when frecency tracking is off"
    (let ((projectile-dashboard-sections '(recent)))
      (expect (projectile-dashboard-test--render
               '(:root "/tmp/p/" :frecency nil :recent-files nil))
              :to-match "Frecency tracking is off")
      (expect (projectile-dashboard-test--render
               '(:root "/tmp/p/" :frecency t :recent-files nil))
              :to-match "Nothing visited"))))

(describe "projectile-dashboard version control"
  (it "shows the branch and the working tree counts"
    (let ((projectile-dashboard-sections '(vcs))
          (data '(:root "/tmp/p/" :vcs git :vcs-state ok :branch "feature/x"
                  :modified 3 :untracked 1)))
      (let ((dashboard (projectile-dashboard-test--render data)))
        (expect dashboard :to-match "feature/x")
        (expect dashboard :to-match "3 modified, 1 untracked"))))

  (it "shows the branch alone when the status query failed"
    (spy-on 'projectile-dashboard--git :and-call-fake
            (lambda (_root &rest args)
              (when (equal (car args) "rev-parse") "main\n")))
    (let ((status (projectile-dashboard--git-status "/tmp/p/")))
      (expect (plist-get status :vcs-state) :to-be 'partial)
      (let ((projectile-dashboard-sections '(vcs))
            (data (append '(:root "/tmp/p/" :vcs git) status)))
        (let ((dashboard (projectile-dashboard-test--render data)))
          (expect dashboard :to-match "main")
          (expect dashboard :to-match "unavailable")))))

  (it "does not pass off a detached HEAD as a branch"
    (spy-on 'projectile-dashboard--git :and-call-fake
            (lambda (_root &rest args)
              (if (equal (car args) "rev-parse") "HEAD\n" "")))
    (expect (plist-get (projectile-dashboard--git-status "/tmp/p/") :branch)
            :to-equal "detached HEAD"))

  (it "scopes the status query to the project and takes no index lock"
    (spy-on 'process-file :and-return-value 0)
    (projectile-dashboard--git "/tmp/p/" "status" "--porcelain")
    (expect (spy-calls-args-for 'process-file 0)
            :to-equal '("git" nil (t nil) nil "--no-optional-locks"
                        "status" "--porcelain"))
    (spy-on 'projectile-dashboard--git :and-return-value "main\n")
    (projectile-dashboard--git-status "/tmp/p/")
    (expect (spy-calls-args-for 'projectile-dashboard--git 1)
            :to-equal '("/tmp/p/" "status" "--porcelain"
                        "--untracked-files=normal" "--" ".")))

  (it "skips the VCS query on a remote project"
    (expect (projectile-dashboard--vcs-info "/ssh:host:/p/" 'git "/ssh:host:")
            :to-equal '(:vcs-state skipped))
    (let ((projectile-dashboard-sections '(vcs)))
      (expect (projectile-dashboard-test--render
               '(:root "/ssh:host:/p/" :vcs git :vcs-state skipped))
              :to-match "not checked (remote project)")))

  (it "degrades to the bare VCS name for anything but git"
    (expect (projectile-dashboard--vcs-info "/tmp/p/" 'hg nil)
            :to-equal '(:vcs-state unsupported))
    (let ((projectile-dashboard-sections '(vcs)))
      (let ((dashboard (projectile-dashboard-test--render
                        '(:root "/tmp/p/" :vcs hg :vcs-state unsupported))))
        (expect dashboard :to-match "hg")
        (expect dashboard :not :to-match "branch"))))

  (it "counts modified and untracked entries out of the porcelain output"
    (spy-on 'projectile-dashboard--git :and-call-fake
            (lambda (_root &rest args)
              (if (equal (car args) "rev-parse")
                  "main\n"
                " M projectile.el\nA  new.el\n?? scratch/\n")))
    (let ((status (projectile-dashboard--git-status "/tmp/p/")))
      (expect (plist-get status :vcs-state) :to-be 'ok)
      (expect (plist-get status :branch) :to-equal "main")
      (expect (plist-get status :modified) :to-equal 2)
      (expect (plist-get status :untracked) :to-equal 1)))

  (it "reports git being unable to answer"
    (spy-on 'projectile-dashboard--git :and-return-value nil)
    (expect (projectile-dashboard--git-status "/tmp/p/")
            :to-equal '(:vcs-state unavailable))
    (let ((projectile-dashboard-sections '(vcs)))
      (expect (projectile-dashboard-test--render
               '(:root "/tmp/p/" :vcs git :vcs-state unavailable))
              :to-match "unavailable"))))

(describe "projectile-dashboard lifecycle commands"
  (it "lists the configured commands without resolving dynamic ones"
    (projectile-test-with-stub-root "project/" ("a.el")
      (spy-on 'projectile-project-type :and-return-value 'test-dashboard-type)
      (spy-on 'projectile--phase-command-dynamic-p :and-call-fake
              (lambda (phase) (eq phase 'run)))
      (spy-on 'projectile-compilation-command :and-return-value "make")
      (spy-on 'projectile-test-command :and-return-value nil)
      (spy-on 'projectile-configure-command :and-return-value nil)
      (spy-on 'projectile-install-command :and-return-value nil)
      (spy-on 'projectile-package-command :and-return-value nil)
      (spy-on 'projectile-run-command :and-call-fake
              (lambda (&rest _) (error "Must not resolve a dynamic command")))
      (let ((commands (projectile-dashboard--commands
                       (projectile-project-root))))
        (expect commands :to-equal '((compile . "make") (run . nil))))))

  (it "names the command that runs each phase"
    (expect (projectile-dashboard--phase-command 'compile)
            :to-be 'projectile-compile-project)
    (expect (projectile-dashboard--phase-command 'test)
            :to-be 'projectile-test-project)
    (dolist (phase '(configure compile test install package run))
      (expect (commandp (projectile-dashboard--phase-command phase))
              :to-be-truthy))))

(describe "projectile-dashboard buttons"
  (it "visits the file a file entry stands for"
    (let ((projectile-dashboard-sections '(recent))
          (data '(:root "/tmp/p/" :recent-files (("src/a.el" . 1.0)))))
      (projectile-dashboard-test--with-render data
        (spy-on 'find-file)
        (let ((button (projectile-dashboard-test--button "src/a.el")))
          (expect button :to-be-truthy)
          (push-button (button-start button))
          (expect 'find-file :to-have-been-called-with "/tmp/p/src/a.el")))))

  (it "runs the task a task entry stands for"
    (let ((projectile-dashboard-sections '(tasks))
          (data '(:root "/tmp/p/" :tasks (("lint" . "make lint")))))
      (projectile-dashboard-test--with-render data
        (spy-on 'projectile--run-task)
        (let ((button (projectile-dashboard-test--button "lint")))
          (expect button :to-be-truthy)
          (push-button (button-start button))
          (expect 'projectile--run-task
                  :to-have-been-called-with "lint" "make lint" nil)))))

  (it "labels a task with a function command instead of resolving it"
    (let ((projectile-dashboard-sections '(tasks)))
      (expect (projectile-dashboard-test--render
               (list :root "/tmp/p/" :tasks (list (cons "gen" #'ignore))))
              :to-match "computed at run time")))

  (it "runs the lifecycle command a command entry stands for"
    (let ((projectile-dashboard-sections '(commands))
          (data '(:root "/tmp/p/" :commands ((compile . "make")))))
      (projectile-dashboard-test--with-render data
        (spy-on 'projectile-compile-project)
        (let ((button (projectile-dashboard-test--button "compile")))
          (expect button :to-be-truthy)
          (push-button (button-start button))
          (expect 'projectile-compile-project :to-have-been-called)))))

  (it "opens the VC interface from the branch entry"
    (let ((projectile-dashboard-sections '(vcs))
          (data '(:root "/tmp/p/" :vcs git :vcs-state ok :branch "main"
                  :modified 0 :untracked 0)))
      (projectile-dashboard-test--with-render data
        (spy-on 'projectile-vc)
        (let ((button (projectile-dashboard-test--button "main")))
          (expect button :to-be-truthy)
          (push-button (button-start button))
          (expect 'projectile-vc :to-have-been-called-with "/tmp/p/")))))

  (it "opens the project root in Dired"
    (let ((projectile-dashboard-sections '(project))
          (data '(:root "/tmp/p/" :name "p" :type generic)))
      (projectile-dashboard-test--with-render data
        (spy-on 'dired)
        (let ((button (projectile-dashboard-test--button "/tmp/p/")))
          (expect button :to-be-truthy)
          (push-button (button-start button))
          (expect 'dired :to-have-been-called-with "/tmp/p/"))))))

(describe "projectile-dashboard as a switch-project action"
  (it "renders the project that was just switched to"
    (projectile-test-with-sandbox
      (projectile-test-with-files ("project-a/" "project-a/.projectile")
        (let* ((a (file-name-as-directory
                   (file-truename (expand-file-name "project-a"))))
               (projectile-known-projects-file (projectile-test-tmp-file-path))
               (projectile-known-projects nil)
               (projectile-switch-project-action #'projectile-dashboard)
               (projectile-dashboard-sections '(project)))
          (projectile-add-known-project a)
          (unwind-protect
              (progn
                (save-window-excursion (projectile-switch-project-by-name a))
                (with-current-buffer projectile-dashboard-buffer-name
                  (expect major-mode :to-be 'projectile-dashboard-mode)
                  (expect (buffer-string) :to-match "project-a")))
            (kill-buffer projectile-dashboard-buffer-name)))))))

(describe "projectile-dashboard keybinding"
  (it "is bound to P in projectile-command-map"
    (expect (lookup-key projectile-command-map (kbd "P"))
            :to-be 'projectile-dashboard)))

(provide 'projectile-dashboard-test)

;;; projectile-dashboard-test.el ends here
