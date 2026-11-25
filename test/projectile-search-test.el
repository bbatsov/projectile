;;; projectile-search-test.el --- Tests for projectile-search and its backends -*- lexical-binding: t -*-

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

;; Tests for the pluggable `projectile-search' backends.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile search backend registry"
  (it "ships grep, ripgrep and ag backends"
    (dolist (name '(grep ripgrep ag))
      (expect (assq name projectile-search-backends) :to-be-truthy)))

  (it "registers and replaces backends"
    (let ((projectile-search-backends nil))
      (projectile-register-search-backend 'x :description "X" :search #'ignore)
      (expect (assq 'x projectile-search-backends) :to-be-truthy)
      (projectile-register-search-backend 'x :description "X2" :search #'ignore)
      (expect (length projectile-search-backends) :to-equal 1)
      (expect (plist-get (cdr (assq 'x projectile-search-backends)) :description)
              :to-equal "X2")))

  (it "treats a nil :available predicate as always available"
    (expect (projectile--backend-available-p '(g :search ignore)) :to-be-truthy)
    (expect (projectile--backend-available-p
             '(g :available (lambda () nil) :search ignore))
            :not :to-be-truthy)))

(describe "projectile--resolve-backend"
  ;; `a' is never available, `b' always is.
  :var (backends)
  (before-each
    (setq backends
          (list (cons 'a (list :available (lambda () nil) :search #'ignore))
                (cons 'b (list :search #'ignore)))))

  (it "auto picks the first available backend"
    (expect (car (projectile--resolve-backend backends 'auto "search")) :to-be 'b))

  (it "returns a named, available backend"
    (expect (car (projectile--resolve-backend backends 'b "search")) :to-be 'b))

  (it "errors for a named but unavailable backend"
    (expect (projectile--resolve-backend backends 'a "search") :to-throw 'user-error))

  (it "errors for an unknown backend"
    (expect (projectile--resolve-backend backends 'zzz "search") :to-throw 'user-error))

  (it "errors when nothing is available"
    (expect (projectile--resolve-backend
             (list (cons 'a (list :available #'ignore :search #'ignore)))
             'auto "search")
            :to-throw 'user-error))

  (it "prompts among the available backends"
    (spy-on 'completing-read :and-return-value "b")
    (expect (car (projectile--resolve-backend backends 'prompt "search")) :to-be 'b)))

(describe "projectile-search"
  (it "dispatches the term and regexp flag to the resolved backend"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    ;; `let*' so the backend's lambda closes over the just-bound `captured'.
    (let* ((captured nil)
           (projectile-search-backends
            (list (cons 'test (list :search (lambda (term regexp)
                                              (setq captured (list term regexp)))))))
           (projectile-search-backend 'test))
      (projectile-search "foo" t)
      (expect captured :to-equal '("foo" t)))))

(describe "search prompt highlighting"
  (it "wraps the tool in brackets and faces it"
    (let ((tag (projectile--search-tool-tag "ripgrep")))
      ;; face aside, it's a plain [tool] tag
      (expect tag :to-equal "[ripgrep]")
      (expect (get-text-property 1 'face tag)
              :to-equal 'projectile-search-prompt-tool)))

  (it "accepts a backend name symbol as well as a string (#2094)"
    ;; `projectile-search' passes the backend symbol, not a string
    (expect (projectile--search-tool-tag 'grep) :to-equal "[grep]"))

  (it "projectile-search builds its prompt without erroring on the backend symbol (#2094)"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile-prepend-project-name :and-call-fake #'identity)
    (let (prompt
          (projectile-search-backends
           (list (cons 'grep (list :search (lambda (&rest _) nil)))))
          (projectile-search-backend 'grep))
      (spy-on 'read-from-minibuffer :and-call-fake (lambda (p &rest _) (setq prompt p) "foo"))
      ;; term nil -> the interactive prompt path that used to crash
      (projectile-search nil nil)
      (expect prompt :to-match (regexp-quote "[grep]"))))

  (describe "projectile--read-search-string-with-default"
    (before-each
      (spy-on 'projectile-prepend-project-name :and-call-fake #'identity))

    (it "shows the symbol-at-point default, faced, and returns it on RET"
      (spy-on 'projectile-symbol-or-selection-at-point :and-return-value "foo")
      (let (prompt)
        (spy-on 'read-from-minibuffer :and-call-fake
                (lambda (p &rest _) (setq prompt p) ""))
        (expect (projectile--read-search-string-with-default "Grep for") :to-equal "foo")
        (expect prompt :to-match (regexp-quote "(default: foo)"))
        ;; the default value itself carries the highlight face
        (let ((i (string-match "foo)" prompt)))
          (expect (get-text-property i 'face prompt)
                  :to-equal 'projectile-search-prompt-default))))

    (it "omits the default label when there is no symbol at point"
      (spy-on 'projectile-symbol-or-selection-at-point :and-return-value nil)
      (let (prompt)
        (spy-on 'read-from-minibuffer :and-call-fake
                (lambda (p &rest _) (setq prompt p) "typed"))
        (projectile--read-search-string-with-default "Grep for")
        (expect prompt :not :to-match "default")))))

(describe "projectile--search-term-suggestions"
  (it "offers the symbol at point, the enclosing function, the file and the last isearch"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun my-fn ()\n  (other-thing))\n")
      (search-backward "other-thing")
      (setq buffer-file-name "/tmp/widgets.el")
      (unwind-protect
          (let ((search-ring '("needle")))
            (expect (projectile--search-term-suggestions)
                    :to-equal '("other-thing" "my-fn" "widgets" "needle")))
        (setq buffer-file-name nil))))

  (it "prefers the region and drops blanks and repeats"
    (with-temp-buffer
      (insert "alpha beta")
      (let ((search-ring '("alpha beta" "")))
        (spy-on 'projectile-symbol-or-selection-at-point :and-return-value "alpha beta")
        (spy-on 'add-log-current-defun :and-return-value " ")
        (expect (projectile--search-term-suggestions) :to-equal '("alpha beta"))))))

(describe "projectile--read-search-term"
  (before-each
    (spy-on 'projectile-project-root :and-return-value "/project/"))

  (it "reads free text with its own history and the suggestions on M-n"
    (spy-on 'projectile--search-term-suggestions :and-return-value '("sym" "fn"))
    (spy-on 'read-from-minibuffer :and-return-value "typed")
    (expect (projectile--read-search-term "Search: ") :to-equal "typed")
    (expect 'read-from-minibuffer :to-have-been-called-with
            "Search: " nil projectile--search-term-map nil
            'projectile-search-history '("sym" "fn")))

  (it "falls back to the symbol at point on empty input, not to a later suggestion"
    (spy-on 'read-from-minibuffer :and-return-value "")
    (spy-on 'projectile-symbol-or-selection-at-point :and-return-value "sym")
    (expect (projectile--read-search-term "Search: ") :to-equal "sym"))

  (it "returns the empty string when there is nothing at point either"
    (spy-on 'read-from-minibuffer :and-return-value "")
    (spy-on 'projectile-symbol-or-selection-at-point :and-return-value "")
    (spy-on 'projectile--search-term-suggestions :and-return-value '("widgets"))
    (expect (projectile--read-search-term "Search: ") :to-equal ""))

  (it "completes identifiers in the minibuffer, over the roots it was given"
    (let (capfs)
      (spy-on 'read-from-minibuffer :and-call-fake
              (lambda (&rest _)
                ;; what the prompt's setup hook would do in the minibuffer
                (with-temp-buffer
                  (run-hooks 'minibuffer-setup-hook)
                  (setq capfs completion-at-point-functions))
                "x"))
      (spy-on 'projectile--identifier-capf :and-return-value #'ignore)
      (projectile--read-search-term "Search: " '("/a/" "/b/"))
      (expect (car capfs) :to-be #'ignore)
      (expect (car (spy-calls-args-for 'projectile--identifier-capf 0))
              :to-equal '("/a/" "/b/"))))

  (it "leaves completion, and the TAB key, alone when the option is off"
    (let ((projectile-search-complete-identifiers nil)
          keymap)
      (spy-on 'read-from-minibuffer :and-call-fake
              (lambda (_prompt _initial map &rest _)
                (setq keymap map)
                (with-temp-buffer (run-hooks 'minibuffer-setup-hook))
                "x"))
      (spy-on 'projectile--identifier-capf)
      (projectile--read-search-term "Search: ")
      (expect 'projectile--identifier-capf :not :to-have-been-called)
      (expect keymap :to-be minibuffer-local-map)))

  (it "takes hyphens for operators only on the word of a programming mode"
    (spy-on 'read-from-minibuffer :and-call-fake
            (lambda (&rest _)
              (with-temp-buffer (run-hooks 'minibuffer-setup-hook))
              "x"))
    (spy-on 'projectile--identifier-capf :and-return-value #'ignore)
    (with-temp-buffer
      (c-mode)
      (projectile--read-search-term "Search: "))
    (with-temp-buffer
      (emacs-lisp-mode)
      (projectile--read-search-term "Search: "))
    (with-temp-buffer
      (text-mode)
      (projectile--read-search-term "Search: "))
    (expect (mapcar #'cadr (spy-calls-all-args 'projectile--identifier-capf))
            :to-equal '(nil t t)))

  (it "binds TAB to completion without losing the minibuffer's own keys"
    (expect (lookup-key projectile--search-term-map (kbd "TAB"))
            :to-be #'completion-at-point)
    (expect (lookup-key projectile--search-term-map (kbd "RET"))
            :to-be (lookup-key minibuffer-local-map (kbd "RET")))))

(describe "projectile--identifiers"
  (it "lists the identifiers with a prefix, the commonest first"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("a.el" . "(foo-bar 1)\n(foo-bar 2)\n(foo-baz)\n(food)\n(unrelated)\n")
         ("b.el" . "(foo-bar 3) (foo_qux)\n"))
      (expect (projectile--identifiers "foo" (list default-directory) t)
              :to-equal '("foo-bar" "foo-baz" "foo_qux" "food"))))

  (it "stops at a hyphen in the languages where it is an operator"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("a.c" . "total = count-offset;\ncount_max = 2;\n"))
      (expect (projectile--identifiers "count" (list default-directory) nil)
              :to-equal '("count" "count_max"))))

  (it "matches the prefix case-sensitively"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("a.el" . "Foobar foobar FOOBAR\n"))
      (expect (projectile--identifiers "foo" (list default-directory) t)
              :to-equal '("foobar"))))

  (it "only matches where an identifier starts, not after a hyphen inside one"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("a.el" . "(my-find-file)\n(my-find-file)\n(find-file)\nfind-dir\n"))
      (expect (projectile--identifiers "find" (list default-directory) t)
              :to-equal '("find-dir" "find-file"))
      (expect (projectile--identifiers "-find" (list default-directory) t)
              :to-be nil)))

  (it "drops the hyphen of an operator that follows an identifier"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("a.c" . "length--;\nlength - 1;\n"))
      (expect (projectile--identifiers "len" (list default-directory) t)
              :to-equal '("length"))))

  (it "looks into dotfiles, which are project files too"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        ((".config.el" . "(hidden-setting)\n"))
      (expect (projectile--identifiers "hidden" (list default-directory) t)
              :to-equal '("hidden-setting"))))

  (it "is not thrown off by the user's ripgrep configuration"
    (assume (executable-find "rg") "ripgrep is not installed")
    (projectile-test-with-project
        (("a.el" . "(foo-bar)\n")
         ("ripgreprc" . "--column\n--stats\n"))
      (let ((process-environment
             (cons (concat "RIPGREP_CONFIG_PATH=" (expand-file-name "ripgreprc"))
                   process-environment)))
        (expect (projectile--identifiers "foo" (list default-directory) t)
                :to-equal '("foo-bar")))))

  (it "refuses a prefix that is not made of identifier characters"
    (spy-on 'call-process)
    (expect (projectile--identifiers "foo(" '("/project/") t) :to-be nil)
    (expect (projectile--identifiers "a|b" '("/project/") t) :to-be nil)
    (expect 'call-process :not :to-have-been-called))

  (it "does not look up a prefix too short to narrow anything down"
    (spy-on 'call-process)
    (expect (projectile--identifiers "f" '("/project/") t) :to-be nil)
    (expect 'call-process :not :to-have-been-called))

  (it "never runs a process for a remote project"
    (spy-on 'call-process)
    (expect (projectile--identifiers "foo" '("/ssh:host:/project/") t) :to-be nil)
    (expect 'call-process :not :to-have-been-called)))

(describe "projectile--identifier-command"
  (it "falls back to git grep in a git project when ripgrep is missing"
    (spy-on 'executable-find :and-call-fake
            (lambda (program &rest _) (and (equal program "git") "/usr/bin/git")))
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (let ((command (projectile--identifier-command "foo" '("/project/") nil)))
      (expect (seq-take command 3) :to-equal '("git" "--no-pager" "grep"))
      ;; no `\\b': the regex library of Apple's git does not have it
      (expect (car (last command)) :to-equal "(^|[^[:alnum:]_])foo[[:alnum:]_]*")))

  (it "produces a git grep that finds identifiers"
    (assume (executable-find "git") "git is not installed")
    (spy-on 'executable-find :and-call-fake
            (lambda (program &rest _) (and (equal program "git") "git")))
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    ;; a repository of its own: the suite's sandbox sits inside Projectile's
    ;; checkout, and CI runs as a user git will not trust that checkout for
    (let ((root (file-name-as-directory (make-temp-file "projectile-git-" t))))
      (unwind-protect
          (let ((default-directory root))
            (with-temp-file (expand-file-name "a.el" root)
              (insert "(my-find-file)\n(find-file)\n(find-file)\n(find-dir)\n"))
            (assume (and (eq 0 (call-process "git" nil nil nil "init" "-q"))
                         (eq 0 (call-process "git" nil nil nil "add" "a.el")))
                    "could not set up a git repository")
            (expect (projectile--identifiers "find" (list root) t)
                    :to-equal '("find-file" "find-dir")))
        (delete-directory root t))))

  (it "has nothing to offer without ripgrep outside git, or across projects"
    (spy-on 'executable-find :and-call-fake
            (lambda (program &rest _) (and (equal program "git") "/usr/bin/git")))
    (spy-on 'projectile-project-vcs :and-return-value 'none)
    (expect (projectile--identifier-command "foo" '("/project/") nil) :to-be nil)
    (spy-on 'projectile-project-vcs :and-return-value 'git)
    (expect (projectile--identifier-command "foo" '("/a/" "/b/") nil) :to-be nil)))

(describe "projectile--identifier-capf"
  (before-each
    (spy-on 'projectile--identifiers :and-return-value '("foo-bar" "foo-baz")))

  (it "completes the word before point"
    (with-temp-buffer
      (insert "defun foo-b")
      (let ((result (funcall (projectile--identifier-capf '("/project/") t))))
        (expect (buffer-substring (nth 0 result) (nth 1 result)) :to-equal "foo-b")
        (expect (all-completions "foo-b" (nth 2 result))
                :to-equal '("foo-bar" "foo-baz"))
        (expect 'projectile--identifiers
                :to-have-been-called-with "foo-b" '("/project/") t))))

  (it "looks up the word, not whatever a completion style asks the table about"
    (with-temp-buffer
      (insert "foo-b")
      (let ((table (nth 2 (funcall (projectile--identifier-capf '("/project/") t)))))
        ;; flex and substring ask about the empty string
        (expect (all-completions "" table) :to-equal '("foo-bar" "foo-baz"))
        (expect (all-completions "foo-ba" table) :to-equal '("foo-bar" "foo-baz"))
        (expect (spy-calls-all-args 'projectile--identifiers)
                :to-equal '(("foo-b" ("/project/") t))))))

  (it "runs one lookup per word, however often it is asked"
    (with-temp-buffer
      (insert "foo-b")
      (let ((capf (projectile--identifier-capf '("/project/") t)))
        (all-completions "" (nth 2 (funcall capf)))
        (all-completions "" (nth 2 (funcall capf)))
        (expect 'projectile--identifiers :to-have-been-called-times 1)
        (insert "a")
        (all-completions "" (nth 2 (funcall capf)))
        (expect 'projectile--identifiers :to-have-been-called-times 2))))

  (it "stops the word at a hyphen where hyphens are operators"
    (with-temp-buffer
      (insert "count -off")
      (let ((result (funcall (projectile--identifier-capf '("/project/") nil))))
        (expect (buffer-substring (nth 0 result) (nth 1 result)) :to-equal "-off")
        (all-completions "" (nth 2 result))
        ;; a word typed with a hyphen in it is taken at its word
        (expect 'projectile--identifiers
                :to-have-been-called-with "-off" '("/project/") t)))
    (with-temp-buffer
      (insert "count")
      (let ((result (funcall (projectile--identifier-capf '("/project/") nil))))
        (all-completions "" (nth 2 result))
        (expect 'projectile--identifiers
                :to-have-been-called-with "count" '("/project/") nil))))

  (it "offers nothing for a word too short to look up"
    (with-temp-buffer
      (insert "some f")
      (expect (funcall (projectile--identifier-capf '("/project/") t)) :to-be nil)))

  (it "asks for the candidates to be shown in the order they were found"
    (with-temp-buffer
      (insert "foo")
      (let* ((table (nth 2 (funcall (projectile--identifier-capf '("/project/") t))))
             (metadata (completion-metadata "foo" table nil)))
        (expect (completion-metadata-get metadata 'display-sort-function)
                :to-be #'identity)
        (expect (completion-metadata-get metadata 'category)
                :to-be 'projectile-identifier)))))

(describe "the search wrapper commands"
  (it "projectile-ripgrep forces the ripgrep backend"
    (let (seen)
      (spy-on 'projectile-search :and-call-fake
              (lambda (&rest _) (setq seen projectile-search-backend)))
      (projectile-ripgrep "bar" t)
      (expect seen :to-be 'ripgrep)
      (expect 'projectile-search :to-have-been-called-with "bar" t)))

  (it "projectile-ag forces the ag backend"
    (let (seen)
      (spy-on 'projectile-search :and-call-fake
              (lambda (&rest _) (setq seen projectile-search-backend)))
      (projectile-ag "baz" nil)
      (expect seen :to-be 'ag)))

  (it "projectile-grep passes the regexp and files to the grep engine"
    (spy-on 'projectile-acquire-root :and-return-value "/proj/")
    (spy-on 'projectile--grep)
    (projectile-grep "pat")
    (expect 'projectile--grep :to-have-been-called-with "pat" nil)))

(describe "projectile shell backend registry"
  (it "ships the built-in and package-backed shell backends"
    (dolist (name '(shell eshell ielm term vterm eat ghostel))
      (expect (assq name projectile-shell-backends) :to-be-truthy)))

  (it "projectile-run dispatches new-process/other-window to the backend"
    (let* (captured
           (projectile-shell-backends
            (list (cons 'test (list :run (lambda (np ow) (setq captured (list np ow)))))))
           (projectile-shell-backend 'test))
      (projectile-run t)
      (expect captured :to-equal '(t nil))))

  (it "the -other-window wrappers force the backend and request another window"
    (let (captured)
      (spy-on 'projectile--run :and-call-fake
              (lambda (pref np ow) (setq captured (list pref np ow))))
      (projectile-run-vterm-other-window t)
      (expect captured :to-equal '(vterm t t))))

  (it "the plain run wrappers force their backend in the current window"
    (let (captured)
      (spy-on 'projectile--run :and-call-fake
              (lambda (pref np ow) (setq captured (list pref np ow))))
      (projectile-run-eshell nil)
      (expect captured :to-equal '(eshell nil nil)))))

(provide 'projectile-search-test)

;;; projectile-search-test.el ends here
