;;; projectile-test-at-point-test.el --- Tests for projectile-run-test-at-point -*- lexical-binding: t -*-

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

;; Tests for `projectile-run-test-at-point' and its rule machinery.  The
;; tree-sitter primitives are faked with `cl-letf' throughout (nodes are
;; plain plists), so the suite runs on any Emacs, with or without
;; tree-sitter support or installed grammars.

;;; Code:

(require 'projectile-test-helpers)

;; Fake tree-sitter nodes are plists: :type, :parent, :text, :fields (an
;; alist of field name -> child node) and :children (a list of named
;; children).  `projectile-tap--with-fake-treesit' routes the treesit
;; accessors used by the production code to those plists.

(defmacro projectile-tap--with-fake-treesit (leaf &rest body)
  "Evaluate BODY with the treesit API faked; point resolves to LEAF."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
             ((symbol-function 'treesit-parser-list) (lambda () '(fake-parser)))
             ((symbol-function 'treesit-node-at) (lambda (_pos) ,leaf))
             ((symbol-function 'treesit-node-type)
              (lambda (node) (plist-get node :type)))
             ((symbol-function 'treesit-node-parent)
              (lambda (node) (plist-get node :parent)))
             ((symbol-function 'treesit-node-text)
              (lambda (node &optional _no-properties) (plist-get node :text)))
             ((symbol-function 'treesit-node-child-by-field-name)
              (lambda (node field)
                (cdr (assoc field (plist-get node :fields)))))
             ((symbol-function 'treesit-node-child)
              (lambda (node n &optional _named)
                (nth n (plist-get node :children)))))
     ,@body))

(defun projectile-tap--function-node (type name)
  "Return a fake function definition node of TYPE with a NAME field."
  (list :type type
        :fields (list (cons "name" (list :type "identifier" :text name)))))

(defun projectile-tap--call-node (fn-node first-arg)
  "Return a fake `call_expression' node calling FN-NODE with FIRST-ARG."
  (list :type "call_expression"
        :fields (list (cons "function" fn-node)
                      (cons "arguments"
                            (list :type "arguments"
                                  :children (and first-arg (list first-arg)))))))

(describe "projectile--test-at-point-rule"
  (it "matches the buffer's major mode against the keys with derived-mode-p"
    (let ((projectile-test-at-point-rules
           '((prog-mode :command-fn ignore))))
      (with-temp-buffer
        (emacs-lisp-mode)                ; derived from `prog-mode'
        (expect (projectile--test-at-point-rule)
                :to-equal '(:command-fn ignore)))))

  (it "prefers an exact mode match listed before a parent mode"
    (let ((projectile-test-at-point-rules
           '((emacs-lisp-mode :command-fn first)
             (prog-mode :command-fn second))))
      (with-temp-buffer
        (emacs-lisp-mode)
        (expect (projectile--test-at-point-rule)
                :to-equal '(:command-fn first)))))

  (it "returns nil when no rule matches"
    (let ((projectile-test-at-point-rules
           '((prog-mode :command-fn ignore))))
      (with-temp-buffer
        (fundamental-mode)
        (expect (projectile--test-at-point-rule) :to-be nil))))

  (it "ships rules for the built-in languages"
    (dolist (mode '(python-ts-mode go-ts-mode js-ts-mode
                    typescript-ts-mode tsx-ts-mode))
      (expect (assq mode projectile-test-at-point-rules) :to-be-truthy))))

(describe "projectile--test-at-point-name"
  (it "walks up to the first node whose type matches and name-fn accepts"
    ;; leaf -> non-matching type -> rejected function -> accepted function
    (let* ((outer (projectile-tap--function-node "function_definition" "test_outer"))
           (helper (append (projectile-tap--function-node "function_definition" "helper")
                           (list :parent outer)))
           (block (list :type "block" :parent helper))
           (leaf (list :type "identifier" :parent block)))
      (projectile-tap--with-fake-treesit leaf
        (expect (projectile--test-at-point-name
                 '(:node-types ("function_definition")
                   :name-fn projectile-test-at-point-python-name))
                :to-equal "test_outer"))))

  (it "returns nil when no enclosing node matches"
    (let* ((top (list :type "module"))
           (leaf (list :type "identifier" :parent top)))
      (projectile-tap--with-fake-treesit leaf
        (expect (projectile--test-at-point-name
                 '(:node-types ("function_definition")
                   :name-fn projectile-test-at-point-python-name))
                :to-be nil))))

  (it "returns nil when name-fn rejects every matching node"
    (let* ((fn (projectile-tap--function-node "function_definition" "not_a_test"))
           (leaf (list :type "identifier" :parent fn)))
      (projectile-tap--with-fake-treesit leaf
        (expect (projectile--test-at-point-name
                 '(:node-types ("function_definition")
                   :name-fn projectile-test-at-point-python-name))
                :to-be nil)))))

(describe "projectile-test-at-point-python-name"
  (it "returns the name of a test_-prefixed function"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-python-name
               (projectile-tap--function-node "function_definition" "test_addition"))
              :to-equal "test_addition")))

  (it "rejects functions without the test_ prefix"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-python-name
               (projectile-tap--function-node "function_definition" "make_fixture"))
              :to-be nil)))

  (it "tolerates a node without a name field"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-python-name '(:type "function_definition"))
              :to-be nil))))

(describe "projectile-test-at-point-go-name"
  (it "returns the name of a TestXxx function"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-go-name
               (projectile-tap--function-node "function_declaration" "TestParse"))
              :to-equal "TestParse")))

  (it "accepts a function named exactly Test"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-go-name
               (projectile-tap--function-node "function_declaration" "Test"))
              :to-equal "Test")))

  (it "rejects Test followed by a lowercase letter (not a go test)"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-go-name
               (projectile-tap--function-node "function_declaration" "Testify"))
              :to-be nil)))

  (it "rejects ordinary functions"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-go-name
               (projectile-tap--function-node "function_declaration" "parseInput"))
              :to-be nil))))

(describe "projectile-test-at-point-jest-name"
  (it "returns the string argument of an it() call"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-jest-name
               (projectile-tap--call-node '(:type "identifier" :text "it")
                                          '(:type "string" :text "\"adds numbers\"")))
              :to-equal "adds numbers")))

  (it "matches test() and describe() too"
    (projectile-tap--with-fake-treesit nil
      (dolist (fn '("test" "describe"))
        ;; The node is built outside `expect' on purpose: `expect' wraps
        ;; its argument in an oclosure, and Emacs 29's interpreted
        ;; oclosures leak an internal (:type . TYPE) entry into the
        ;; body's lexical environment, so an unquoted :type keyword in
        ;; there evaluates to `buttercup--thunk' (fixed in Emacs 30).
        (let ((node (projectile-tap--call-node
                     (list :type "identifier" :text fn)
                     '(:type "string" :text "'suite'"))))
          (expect (projectile-test-at-point-jest-name node)
                  :to-equal "suite")))))

  (it "matches member calls like it.only()"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-jest-name
               (projectile-tap--call-node
                '(:type "member_expression"
                  :fields (("object" . (:type "identifier" :text "it"))))
                '(:type "string" :text "\"focused\"")))
              :to-equal "focused")))

  (it "strips the backticks of a template string argument"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-jest-name
               (projectile-tap--call-node '(:type "identifier" :text "describe")
                                          '(:type "template_string" :text "`math`")))
              :to-equal "math")))

  (it "rejects calls to other functions"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-jest-name
               (projectile-tap--call-node '(:type "identifier" :text "expect")
                                          '(:type "string" :text "\"nope\"")))
              :to-be nil)))

  (it "rejects test calls whose first argument is not a string"
    (projectile-tap--with-fake-treesit nil
      (expect (projectile-test-at-point-jest-name
               (projectile-tap--call-node '(:type "identifier" :text "it")
                                          '(:type "identifier" :text "name")))
              :to-be nil))))

(describe "test-at-point command functions"
  (it "builds a pytest command"
    (expect (projectile-test-at-point-python-command "test_addition" "tests/test_math.py")
            :to-equal "python -m pytest tests/test_math.py::test_addition"))

  (it "builds a go test command scoped to the file's package"
    (expect (projectile-test-at-point-go-command "TestParse" "pkg/parser/parser_test.go")
            :to-equal "go test -run \\^TestParse\\$ ./pkg/parser"))

  (it "builds a go test command for a file in the project root"
    (expect (projectile-test-at-point-go-command "TestMain" "main_test.go")
            :to-equal "go test -run \\^TestMain\\$ ."))

  (it "builds a jest command"
    (expect (projectile-test-at-point-jest-command "adds numbers" "src/math.test.js")
            :to-equal "npx jest src/math.test.js -t adds\\ numbers")))

(describe "test-at-point command injection"
  ;; A jest/pytest test name is arbitrary source text and a file name
  ;; comes from the repository, so both must be shell-quoted or a
  ;; hostile project could inject shell code at a routine keystroke.
  (cl-flet ((one-arg-p (command marker)
              ;; The dangerous payload survives as a single inert shell
              ;; token: splitting the command on unescaped whitespace,
              ;; no token past MARKER introduces a new word boundary
              ;; that would start a fresh command.
              (with-temp-buffer
                (insert command)
                (goto-char (point-min))
                ;; A correctly quoted argument contains no unescaped
                ;; ';' or '$(' or unescaped space that isn't preceded
                ;; by a backslash.
                (not (string-match-p "[^\\];\\|[^\\]\\$(" command)))))
    (it "neutralizes a jest test name that tries to inject"
      (let ((cmd (projectile-test-at-point-jest-command
                  "x'; touch PWNED #" "a.test.js")))
        (expect cmd :to-equal
                (concat "npx jest a.test.js -t "
                        (shell-quote-argument "x'; touch PWNED #")))
        (expect (one-arg-p cmd "-t") :to-be-truthy)))

    (it "neutralizes a file name that tries to inject"
      (let ((cmd (projectile-test-at-point-python-command
                  "test_x" "test_$(touch PWNED).py")))
        (expect cmd :to-equal
                (concat "python -m pytest "
                        (shell-quote-argument "test_$(touch PWNED).py")
                        "::test_x"))
        (expect (one-arg-p cmd "pytest") :to-be-truthy)))

    (it "neutralizes an injecting go package directory"
      (let ((cmd (projectile-test-at-point-go-command
                  "TestX" "pkg; rm -rf ~/foo_test.go")))
        (expect (one-arg-p cmd "-run") :to-be-truthy)))))

(describe "projectile-run-test-at-point"
  (it "signals a user-error when tree-sitter is not available"
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () nil)))
      (expect (projectile-run-test-at-point nil)
              :to-throw 'user-error
              '("This command requires Emacs 29+ built with tree-sitter support"))))

  (it "signals a user-error when the buffer has no tree-sitter parser"
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
              ((symbol-function 'treesit-parser-list) (lambda () nil)))
      (with-temp-buffer
        (expect (projectile-run-test-at-point nil) :to-throw 'user-error))))

  (it "signals a user-error when no rule matches the major mode"
    (let ((projectile-test-at-point-rules nil))
      (projectile-tap--with-fake-treesit nil
        (with-temp-buffer
          (setq buffer-file-name "/tmp/proj/test_foo.py")
          (expect (projectile-run-test-at-point nil) :to-throw 'user-error)))))

  (it "signals a user-error when no test encloses point"
    (let ((projectile-test-at-point-rules
           '((fundamental-mode
              :node-types ("function_definition")
              :name-fn projectile-test-at-point-python-name
              :command-fn projectile-test-at-point-python-command))))
      ;; The fake leaf is nil, so the upward walk finds nothing.
      (projectile-tap--with-fake-treesit nil
        (with-temp-buffer
          (setq buffer-file-name "/tmp/proj/test_foo.py")
          (expect (projectile-run-test-at-point nil)
                  :to-throw 'user-error '("No test found at point"))))))

  (it "runs the built command like a test command, without recording it"
    (let ((projectile-test-at-point-rules
           '((fundamental-mode
              :node-types ("function_definition")
              :name-fn projectile-test-at-point-python-name
              :command-fn projectile-test-at-point-python-command)))
          (projectile-test-cmd-map (make-hash-table :test 'equal))
          (projectile-project-command-history (make-hash-table :test 'equal))
          (leaf (projectile-tap--function-node "function_definition" "test_addition")))
      (spy-on 'projectile-compilation-dir :and-return-value "/tmp/proj/")
      (spy-on 'projectile--run-project-cmd)
      (projectile-tap--with-fake-treesit leaf
        (with-temp-buffer
          (setq buffer-file-name "/tmp/proj/tests/test_math.py")
          (projectile-run-test-at-point nil)))
      (let ((args (spy-calls-args-for 'projectile--run-project-cmd 0)))
        ;; The command is built from the test at point and the file name
        ;; relative to the directory the command runs in.
        (expect (nth 0 args)
                :to-equal "python -m pytest tests/test_math.py::test_addition")
        ;; A nil command-map means the project's cached test command and
        ;; the command histories stay untouched.
        (expect (nth 1 args) :to-be nil)
        (expect (plist-get (nthcdr 2 args) :save-buffers) :to-be-truthy))
      (expect (hash-table-count projectile-test-cmd-map) :to-equal 0)
      (expect (hash-table-count projectile-project-command-history) :to-equal 0))))

(provide 'projectile-test-at-point-test)

;;; projectile-test-at-point-test.el ends here
