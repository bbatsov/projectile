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
      (spy-on 'read-string :and-call-fake (lambda (p &rest _) (setq prompt p) "foo"))
      ;; term nil -> the interactive prompt path that used to crash
      (projectile-search nil nil)
      (expect prompt :to-match (regexp-quote "[grep]"))))

  (describe "projectile--read-search-string-with-default"
    (before-each
      (spy-on 'projectile-prepend-project-name :and-call-fake #'identity))

    (it "shows the symbol-at-point default, faced, and returns it on RET"
      (spy-on 'projectile-symbol-or-selection-at-point :and-return-value "foo")
      (let (prompt)
        (spy-on 'read-string :and-call-fake
                (lambda (p &rest _) (setq prompt p) "foo"))
        (expect (projectile--read-search-string-with-default "Grep for") :to-equal "foo")
        (expect prompt :to-match (regexp-quote "(default: foo)"))
        ;; the default value itself carries the highlight face
        (let ((i (string-match "foo)" prompt)))
          (expect (get-text-property i 'face prompt)
                  :to-equal 'projectile-search-prompt-default))))

    (it "omits the default label when there is no symbol at point"
      (spy-on 'projectile-symbol-or-selection-at-point :and-return-value nil)
      (let (prompt)
        (spy-on 'read-string :and-call-fake
                (lambda (p &rest _) (setq prompt p) "typed"))
        (projectile--read-search-string-with-default "Grep for")
        (expect prompt :not :to-match "default")))))

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
