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
