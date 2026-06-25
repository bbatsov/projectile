;;; projectile-relation-test.el --- Tests for implementation/test and related files -*- lexical-binding: t -*-

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

;; Tests for implementation/test and related files.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-get-other-files"
  (it "returns files with same names but different extensions"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("root.c"
           "root.h"
           "src/root.h"
           "src/test1.c"
           "src/test2.c"
           "src/test+copying.m"
           "src/test1.cpp"
           "src/test2.cpp"
           "src/Makefile"
           "src/test.vert"
           "src/test.frag"
           "src/same_name.c"
           "src/some_module/same_name.c"
           "include1/root.h"
           "include1/same_name.h"
           "include1/test1.h"
           "include1/test1.h~"
           "include1/test2.h"
           "include1/test+copying.h"
           "include1/test1.hpp"
           "include2/some_module/same_name.h"
           "include2/test1.h"
           "include2/test2.h"
           "include2/test2.hpp"
           "src/test1.service.js"
           "src/test2.service.spec.js"
           "include1/test1.service.spec.js"
           "include2/test1.service.spec.js"
           "include1/test2.js"
           "include2/test2.js")
          ()
        (let ((projectile-other-file-alist '(;; handle C/C++ extensions
                                             ("cpp" . ("h" "hpp" "ipp"))
                                             ("ipp" . ("h" "hpp" "cpp"))
                                             ("hpp" . ("h" "ipp" "cpp"))
                                             ("cxx" . ("hxx" "ixx"))
                                             ("ixx" . ("cxx" "hxx"))
                                             ("hxx" . ("ixx" "cxx"))
                                             ("c" . ("h"))
                                             ("m" . ("h"))
                                             ("mm" . ("h"))
                                             ("h" . ("c" "cpp" "ipp" "hpp" "m" "mm"))
                                             ("cc" . ("hh"))
                                             ("hh" . ("cc"))

                                             ;; vertex shader and fragment shader extensions in glsl
                                             ("vert" . ("frag"))
                                             ("frag" . ("vert"))

                                             ;; handle files with no extension
                                             (nil . ("lock" "gpg"))
                                             ("lock" . (""))
                                             ("gpg" . (""))

                                             ;; handle files with nested extensions
                                             ("service.js" . ("service.spec.js"))
                                             ("js" . ("js")))))
          (expect (projectile-get-other-files "root.c") :to-equal '("root.h" "include1/root.h" "src/root.h"))
          (expect (projectile-get-other-files "src/test1.c") :to-equal '("include1/test1.h" "include2/test1.h"))
          (expect (projectile-get-other-files "src/test1.cpp") :to-equal '("include1/test1.h" "include2/test1.h" "include1/test1.hpp"))
          (expect (projectile-get-other-files "test2.c") :to-equal '("include1/test2.h" "include2/test2.h"))
          (expect (projectile-get-other-files "test2.cpp") :to-equal '("include1/test2.h" "include2/test2.h" "include2/test2.hpp"))
          (expect (projectile-get-other-files "test1.h") :to-equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp"))
          (expect (projectile-get-other-files "test2.h") :to-equal '("src/test2.c" "src/test2.cpp" "include2/test2.hpp"))
          (expect (projectile-get-other-files "include1/test1.h" t) :to-equal '("src/test1.c" "src/test1.cpp" "include1/test1.hpp"))
          (expect (projectile-get-other-files "Makefile.lock") :to-equal '("src/Makefile"))
          (expect (projectile-get-other-files "include2/some_module/same_name.h") :to-equal '("src/some_module/same_name.c" "src/same_name.c"))
          ;; nested extensions
          (expect (projectile-get-other-files "src/test1.service.js") :to-equal '("include1/test1.service.spec.js" "include2/test1.service.spec.js"))
          ;; fallback to outer extensions if no rule for nested extension defined
          (expect (projectile-get-other-files "src/test2.service.spec.js") :to-equal '("include1/test2.js" "include2/test2.js"))
          (expect (projectile-get-other-files "src/test+copying.m") :to-equal '("include1/test+copying.h"))))))

  (it "returns files based on the paths returned by :related-files-fn option"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/test1.cpp"
           "src/test1.def"
           "src/test2.def"
           "src/test2.cpp"
           "src/test2.h"
           "src/test3.cpp"
           "src/test3.h")
          (:related-files-fn (lambda (file)
                           (cond ((equal file "src/test1.def") '(:other "src/test1.cpp"))
                                 ((equal file "src/test2.def") '(:other ("src/test2.cpp" "src/test2.h" "src/test4.h")))
                                 ((equal file "src/test3.cpp") '(:other nil)))))
        (expect (projectile-get-other-files "src/test1.def") :to-equal '("src/test1.cpp"))
        (expect (projectile-get-other-files "src/test2.def") :to-equal '("src/test2.cpp" "src/test2.h"))
        ;; Make sure extension based mechanism is still working
        (expect (projectile-get-other-files "src/test2.cpp") :to-equal '("src/test2.h"))
        ;; Make sure that related-files-fn option has priority over existing mechanism
        (expect (projectile-get-other-files "src/test3.cpp") :to-equal nil))))

  (it "returns files based on the predicate returned by :related-files-fn option"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/test1.cpp"
           "src/test1.def"
           "src/test2.def"
           "src/test2.cpp"
           "src/test2.h"
           "src/test3.cpp"
           "src/test3.h")
          (:related-files-fn
           (lambda (file)
             (cond ((equal file "src/test1.def")
                    (list :other (lambda (other-file)
                                   (equal other-file "src/test1.cpp"))))
                   ((equal file "src/test2.def")
                    (list :other (lambda (other-file)
                                   (or (equal other-file "src/test2.cpp")
                                       (equal other-file "src/test2.h")))))
                   ((equal file "src/test3.cpp")
                    (list :other (lambda (other-file) nil))))))

        (expect (projectile-get-other-files "src/test1.def") :to-equal '("src/test1.cpp"))
        (expect (projectile-get-other-files "src/test2.def") :to-equal '("src/test2.cpp" "src/test2.h"))
        ;; Make sure extension based mechanism is still working
        (expect (projectile-get-other-files "src/test2.cpp") :to-equal '("src/test2.h"))
        ;; Make sure that related-files-fn option has priority over existing mechanism
        (expect (projectile-get-other-files "src/test3.cpp") :to-equal nil)))))

(describe "projectile-dirname-matching-count"
  (it "counts matching dirnames ascending file paths"
    (expect (projectile-dirname-matching-count "src/food/sea.c" "src/food/cat.c") :to-equal 2)
    (expect (projectile-dirname-matching-count "src/weed/sea.c" "src/food/sea.c") :to-equal 0)
    (expect (projectile-dirname-matching-count "test/demo-test.el" "demo.el") :to-equal 0)))

(describe "projectile--find-matching-test"
  (it "finds matching test or file"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("app/models/weed/sea.rb"
           "app/models/food/sea.rb"
           "spec/models/weed/sea_spec.rb"
           "spec/models/food/sea_spec.rb")
          (:test-suffix "_spec")
        (expect (projectile--find-matching-test "app/models/food/sea.rb") :to-equal '("spec/models/food/sea_spec.rb"))
        (expect (projectile--find-matching-file "spec/models/food/sea_spec.rb") :to-equal '("app/models/food/sea.rb")))))

  (it "finds matching test or file with dirs"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("source/foo/foo.service.js"
           "source/bar/bar.service.js"
           "spec/foo/foo.service.spec.js"
           "spec/bar/bar.service.spec.js")
          (:test-suffix ".spec" :test-dir "spec/" :src-dir "source/")
        (expect (projectile--find-matching-test
                 "project/source/foo/foo.service.js")
                :to-equal '("spec/foo/foo.service.spec.js"))
        (expect (projectile--find-matching-file
                 "project/spec/bar/bar.service.spec.js")
                :to-equal '("source/bar/bar.service.js")))))

  (it "finds matching test with dirs and inexistent test file"
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("project/src/main/scala/bar/package.scala"
           "project/src/main/scala/foo/package.scala"
           "project/src/test/scala/foo/packageSpec.scala")
          (:test-suffix "Spec" :test-dir "test" :src-dir "main")
        (expect (projectile--find-matching-test
                 "project/src/main/scala/bar/package.scala")
                :to-equal '("src/test/scala/bar/packageSpec.scala")))))

  (it "finds matching test or file based on the paths returned by :related-files-fn option"
    (defun -my/related-files(file)
      (if (string-match (rx (group (or "src" "test")) (group "/" (1+ anything) ".cpp")) file)
          (if (equal (match-string 1 file ) "test")
              (list :impl (concat "src" (match-string 2 file)))
            (list :test (concat "test" (match-string 2 file))))))
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/Foo.cpp"
           "src/Bar.cpp"
           "src/Baz.py"
           "test/Bar.cpp"
           "test/Foo.cpp"
           "other/Test_Baz.py")
          (:related-files-fn #'-my/related-files :test-prefix "Test_")
        (expect (projectile-test-file-p "test/Foo.cpp") :to-equal t)
        (expect (projectile-test-file-p "src/Foo.cpp") :to-equal nil)
        (expect (projectile--find-matching-test "src/Foo.cpp") :to-equal '("test/Foo.cpp"))
        (expect (projectile--find-matching-test "src/Foo2.cpp") :to-equal nil)
        (expect (projectile--find-matching-file "test/Foo.cpp") :to-equal '("src/Foo.cpp"))
        (expect (projectile--find-matching-file "test/Foo2.cpp") :to-equal nil)
        ;; Make sure that existing mechanism(:test-prefix) still works
        (expect (projectile-test-file-p "other/Test_Baz.py") :to-equal t)
        (expect (projectile-test-file-p "other/Baz.py") :to-equal nil)
        (expect (projectile--find-matching-file "other/Test_Baz.py") :to-equal '("src/Baz.py"))
        (expect (projectile--find-matching-test "src/Baz.py") :to-equal '("other/Test_Baz.py")))))

  (it "finds matching test or file by the predicate returned by :related-files-fn option"
    (defun -my/related-files(file)
      (cond ((equal file "src/Foo.cpp")
             (list :test (lambda (other-file)
                      (equal other-file "test/Foo.cpp"))))
            ((equal file "test/Foo.cpp")
             (list :impl (lambda (other-file)
                      (equal other-file "src/Foo.cpp"))))))
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/Foo.cpp"
           "src/Bar.cpp"
           "test/Bar.cpp"
           "test/Foo.cpp")
          (:related-files-fn #'-my/related-files)
        (expect (projectile-test-file-p "test/Foo.cpp") :to-equal t)
        (expect (projectile-test-file-p "src/Foo.cpp") :to-equal nil)
        (expect (projectile--find-matching-test "src/Foo.cpp") :to-equal '("test/Foo.cpp"))
        (expect (projectile--find-matching-test "src/Foo.cpp") :to-equal '("test/Foo.cpp"))
        (expect (projectile--find-matching-file "test/Foo.cpp") :to-equal '("src/Foo.cpp")))))

  (it "defers to test-dir property when it's set to a function"
    (projectile-test-with-sandbox
     (projectile-test-with-files-using-custom-project
          ("src/foo/Foo.cpp"
           "src/bar/Foo.cpp"
           "test/foo/FooTest.cpp")
          (:test-dir
           (lambda (file-path)
             (projectile-complementary-dir file-path "src" "test"))
           :test-suffix "Test")
          (expect (projectile--find-matching-test
                   (projectile-expand-root "src/bar/Foo.cpp"))
                  :to-equal
                  (list "test/bar/FooTest.cpp")))))

  (it "defers to src-dir property when it's set to a function"
    (projectile-test-with-sandbox
     (projectile-test-with-files-using-custom-project
          ("src/foo/Foo.cpp"
           "src/bar/Foo.cpp"
           "test/foo/FooTest.cpp")
          (:src-dir
           (lambda (file-path)
             (projectile-complementary-dir file-path "test" "src"))
           :test-suffix "Test")
          (expect (projectile--find-matching-file
                   (projectile-expand-root "test/foo/FooTest.cpp"))
                  :to-equal
                  (list "src/foo/Foo.cpp")))))

  (it "defers to a fallback using \"src\" and \"test\""
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("project.clj"
           "src/example/core.clj"
           "test/example/core2_test.clj")
          (:test-suffix "_test")
        (expect (projectile--find-matching-test
                 (projectile-expand-root "src/example/core.clj"))
                :to-equal
                (list "test/example/core_test.clj"))
        (expect (projectile--find-matching-file
                 (projectile-expand-root "test/example/core2_test.clj"))
                :to-equal
                (list "src/example/core2.clj"))))))

(describe "projectile--related-files"
  (it "returns related files for the given file"
    (defun -my/related-files(file)
      (cond ((equal file "src/Foo.c")
             (list :test "src/TestFoo.c" :doc "doc/Foo.txt"))
            ((equal file "src/TestFoo.c")
             (list :impl (lambda (other-file)
                           (equal other-file "src/Foo.c"))))))
    (projectile-test-with-sandbox
      (projectile-test-with-files-using-custom-project
          ("src/Foo.c"
           "src/TestFoo.c"
           "doc/Foo.txt")
          (:related-files-fn #'-my/related-files)
        (expect (projectile--related-files-kinds "src/Foo.c") :to-equal '(:test :doc))
        (expect (projectile--related-files-kinds "src/TestFoo.c") :to-equal '(:impl))
        (expect (projectile--related-files "src/TestFoo.c" :impl) :to-equal '("src/Foo.c"))
        (expect (projectile--related-files "src/Foo.c" :doc) :to-equal '("doc/Foo.txt"))
        ;; Support abspath
        (expect (projectile--related-files-kinds (concat (projectile-project-root) "src/Foo.c")) :to-equal '(:test :doc))
        (expect (projectile--related-files (concat (projectile-project-root) "src/Foo.c") :doc) :to-equal '("doc/Foo.txt"))))))

(describe "projectile-related-files-fn-groups"
  (it "generate related files fn which relates members of each group as a specified kind"
    (let ((fn (projectile-related-files-fn-groups :foo '(("a.cpp" "req/a.txt" "doc/a.uml")
                                                         ("b.cpp" "req/b.txt")))))
      (expect (funcall fn "a.cpp") :to-equal '(:foo ("req/a.txt" "doc/a.uml")))
      (expect (funcall fn "req/a.txt") :to-equal '(:foo ("a.cpp" "doc/a.uml")))
      (expect (funcall fn "b.cpp") :to-equal '(:foo ("req/b.txt")))
      (expect (funcall fn "c.cpp") :to-equal nil))))

(describe "projectile-related-files-fn-extensions"
  (it "generate related files fn which relates files with the given extnsions"
    (let* ((fn (projectile-related-files-fn-extensions :foo '("cpp" "h" "hpp")))
           (plist (funcall fn "a.cpp"))
           (predicate (plist-get plist :foo)))
      (expect plist :to-contain :foo)
      (expect (funcall predicate "a.h") :to-equal t)
      (expect (funcall predicate "a.hpp") :to-equal t)
      (expect (funcall predicate "b.cpp") :to-equal nil)
      (expect (funcall predicate "a.cpp") :to-equal nil))))

(describe "projectile-related-files-fn-tests-with-prefix"
  (it "generate related files fn which relates tests and impl based on extension and prefix"
    (let ((fn (projectile-related-files-fn-test-with-prefix "py" "test_")))
      (let* ((plist (funcall fn "foo/a.py"))
            (predicate (plist-get plist :test)))
        (expect plist :to-contain :test)
        (expect (funcall predicate "bar/test_a.py") :to-equal t)
        (expect (funcall predicate "bar/test_a.cpp") :to-equal nil))
      (let* ((plist (funcall fn "foo/test_a.py"))
             (predicate (plist-get plist :impl)))
        (expect plist :to-contain :impl)
        (expect (funcall predicate "bar/a.py") :to-equal t)
        (expect (funcall predicate "bar/a.cpp") :to-equal nil)
        (expect (funcall predicate "bar/test_a.cpp") :to-equal nil)))))

(describe "projectile-related-files-fn-tests-with-suffix"
  (it "generate related files fn which relates tests and impl based on extension and suffix"
    (let ((fn (projectile-related-files-fn-test-with-suffix "py" "-test")))
      (let* ((plist (funcall fn "foo/a.py"))
            (predicate (plist-get plist :test)))
        (expect plist :to-contain :test)
        (expect (funcall predicate "bar/a-test.py") :to-equal t)
        (expect (funcall predicate "bar/a-test.cpp") :to-equal nil))
      (let* ((plist (funcall fn "foo/a-test.py"))
             (predicate (plist-get plist :impl)))
        (expect plist :to-contain :impl)
        (expect (funcall predicate "bar/a.py") :to-equal t)
        (expect (funcall predicate "bar/a.cpp") :to-equal nil)
        (expect (funcall predicate "bar/a-test.cpp") :to-equal nil)))))

(describe "projectile--related-files-plist-by-kind"
  (defun -sample-predicate (other-file)
    (equal other-file "src/foo.c"))
  (defun -sample-predicate2 (other-file)
    (equal other-file "src/bar.c"))
  (describe "when :related-files-fn returns paths"
    (it "returns a plist containing :paths only with the existing files on file system without duplication"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")
            (:related-files-fn (lambda (_)
                                 (list :foo '("src/foo.c" "src/bar.c" "src/foo.c"))))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:paths ("src/foo.c")))))))
  (describe "when :related-files-fn returns one predicate"
    (it "returns a plist containing :predicate with the same predicate"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")  ; Contents does not matter
            (:related-files-fn (lambda (_)
                                 (list :foo '-sample-predicate)))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:predicate -sample-predicate))))))
  (describe "when :related-files-fn returns multiple predicates"
    (it "returns a plist containing :predicate with a merging predicate"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")  ; Contents does not matter
            (:related-files-fn (lambda (_)
                                 (list :foo (list '-sample-predicate '-sample-predicate2))))
          (let* ((plist (projectile--related-files-plist-by-kind "something" :foo))
                 (predicate (plist-get plist :predicate)))
            (expect plist :to-contain :predicate)
            (expect (funcall predicate "src/foo.c") :to-equal t)
            (expect (funcall predicate "src/bar.c") :to-equal t))))))
  (describe "when :related-files-fn returns both paths and predicates"
    (it "returns a plist containing both :paths and :predicates"
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c")
            (:related-files-fn (lambda (_)
                                 (list :foo '("src/foo.c" -sample-predicate))))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:paths ("src/foo.c") :predicate -sample-predicate))))))
  (describe "when :related-files-fn is a list of functions"
    (it "returns a plist containing the merged results"
      (defun -sample-fn(file)
        (list :foo "src/foo.c"))
      (defun -sample-fn2(file)
        (list :foo '-sample-predicate))
      (projectile-test-with-sandbox
        (projectile-test-with-files-using-custom-project
            ("src/foo.c"
             "src/bar.c")
            (:related-files-fn (list '-sample-fn '-sample-fn2))
          (expect (projectile--related-files-plist-by-kind "something" :foo)
                  :to-equal '(:paths ("src/foo.c") :predicate -sample-predicate)))))))

(describe "projectile--impl-name-for-test-name"
  :var ((mock-projectile-project-types
         '((foo test-suffix "Test")
           (bar test-prefix "Test"))))
  (it "removes suffix from test file"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-name-for-test-name "FooTest.cpp")
              :to-equal
              "Foo.cpp")))
  (it "removes prefix from test file"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'bar))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-name-for-test-name "TestFoo.cpp")
              :to-equal
              "Foo.cpp"))))

(describe "projectile-find-implementation-or-test"
  (it "error when test file does not exist and projectile-create-missing-test-files is nil"
    (cl-letf (((symbol-function 'projectile-test-file-p) #'ignore)
              ((symbol-function 'file-exists-p) #'ignore)
              ((symbol-function 'projectile-expand-root) #'identity)
              ((symbol-function 'projectile-find-matching-test) (lambda (file) "dir/foo"))
              (projectile-create-missing-test-files nil)
              (projectile-project-type 'foo))
      (expect (projectile-find-implementation-or-test "foo") :to-throw))))

(describe "projectile--impl-file-from-src-dir-fn"
  :var ((mock-projectile-project-types
         '((foo src-dir (lambda (impl-file) "/outer/foo/test/dir"))
           (bar src-dir "not a function"))))
  (it "returns result of projectile--complementary-file when src-dir property is a function"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              ((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo"))
              ((symbol-function 'file-relative-name) (lambda (f rel) f))
              ((symbol-function 'file-exists-p) (lambda (file) t))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "foo") :to-equal "/outer/foo/test/dir")))
  (it "returns file relative to project root"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              ((symbol-function 'projectile-project-root) (lambda (&optional _dir) "/outer/foo"))
              ((symbol-function 'file-exists-p) (lambda (file) t))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "/outer/foo/bar")
              :to-equal
              "test/dir")))
  (it "returns nil when src-dir property is a not function"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'bar))
              ((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo"))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "bar") :to-equal nil)))
  (it "returns nil when src-dir function result is not an existing file"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              ((symbol-function 'projectile-project-root) (lambda (&optional _dir) "/outer/foo"))
              ((symbol-function 'file-exists-p) #'ignore)
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-file-from-src-dir-fn "bar") :to-equal nil))))

(describe "projectile--test-file-from-test-dir-fn"
  :var ((mock-projectile-project-types
         '((foo test-dir (lambda (impl-file) "/outer/foo/test/dir"))
           (bar test-dir "not a function"))))
  (it "returns result of projectile--complementary-file when test-dir property is a function"
    (cl-letf (((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              ((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo"))
              ((symbol-function 'file-relative-name) (lambda (f rel) f))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-file-from-test-dir-fn "foo") :to-equal "/outer/foo/test/dir")))
  (it "returns file relative to project root"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              ((symbol-function 'projectile-project-root) (lambda (&optional _dir) "/outer/foo"))
              ((symbol-function 'projectile--complementary-file)
               (lambda (impl-file dir-fn file-fn) (funcall dir-fn impl-file)))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-file-from-test-dir-fn "/outer/foo/bar")
              :to-equal
              "test/dir")))
  (it "returns nil when test-dir property is a not function"
    (cl-letf (((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'bar))
              (projectile-project-types mock-projectile-project-types)
              ((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo")))
      (expect (projectile--test-file-from-test-dir-fn "bar") :to-equal nil))))

(describe "projectile--complementary-file"
  (it "dir-fn and filename-fn applied correctly"
    (cl-letf (((symbol-function 'file-exists-p) (lambda (file) t))
              ((symbol-function 'dir-fn) (lambda (dir) "foo/test/dir"))
              ((symbol-function 'filename-fn) (lambda (filename) "Foo.test")))
      (expect (projectile--complementary-file
               "foo/src/dir/Foo.impl"
               #'dir-fn
               #'filename-fn)
              :to-equal "foo/test/dir/Foo.test"))))

(describe "projectile--impl-to-test-dir"
  :var ((mock-projectile-project-types
         '((foo test-dir "test" src-dir "src")
           (bar test-dir identity src-dir "src"))))
  (it "replaces occurrences of src-dir with test-dir"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo"))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-to-test-dir "/foo/src/Foo") :to-equal "/foo/test/")))
  (it "nil returned when test-dir property is not a string"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _dir) "bar"))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'bar))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-to-test-dir "/bar/src/bar") :to-be nil)))
  (it "error when src-dir not a substring of impl file"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo"))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--impl-to-test-dir "/bar/other/bar") :to-throw))))

(describe "projectile--test-to-impl-dir"
  :var ((mock-projectile-project-types
         '((foo test-dir "test" src-dir "src")
           (bar test-dir "test" src-dir identity))))
  (it "replaces occurrences of test-dir with src-dir"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo"))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-to-impl-dir "/foo/test/Foo") :to-equal "/foo/src/")))
  (it "nil returned when src-dir property is not a string"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _dir) "bar"))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'bar))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-to-impl-dir "/bar/test/bar") :to-be nil)))
  (it "error when test-dir not a substring of test file"
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _dir) "foo"))
              ((symbol-function 'projectile-project-type) (lambda (&optional _dir) 'foo))
              (projectile-project-types mock-projectile-project-types))
      (expect (projectile--test-to-impl-dir "/bar/other/bar") :to-throw))))

(describe "projectile-test-prefix"
  :var ((mock-projectile-project-types
         '((foo test-prefix "Test"))))
  (it "gets set test-prefix"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo))
      (expect (projectile-test-prefix'foo) :to-equal "Test")))
  (it "uses local override"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo)
          (projectile-project-test-prefix "Spec"))
      (expect (projectile-test-prefix 'foo) :to-equal "Spec"))))

(describe "projectile-test-suffix"
  :var ((mock-projectile-project-types
         '((foo test-suffix "Test"))))
  (it "gets set test-suffix"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo))
      (expect (projectile-test-suffix'foo) :to-equal "Test")))
  (it "uses local override"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo)
          (projectile-project-test-suffix "Spec"))
      (expect (projectile-test-suffix 'foo) :to-equal "Spec"))))

(describe "projectile-related-files-fn"
  :var ((mock-projectile-project-types
         '((foo related-files-fn ignore))))
  (it "gets set related-files-fn"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo))
      (expect (projectile-related-files-fn 'foo) :to-equal #'ignore)))
  (it "uses local override"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo)
          (projectile-project-related-files-fn #'identity))
      (expect (projectile-related-files-fn 'foo) :to-equal #'identity))))

(describe "projectile-test-directory"
  :var ((mock-projectile-project-types
         '((foo test-dir "test"))))
  (it "gets set test directory"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo))
      (expect (projectile-test-directory 'foo) :to-equal "test")))
  (it "uses local override"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo)
          (projectile-project-test-dir "other"))
      (expect (projectile-test-directory 'foo) :to-equal "other"))))

(describe "projectile-src-directory"
  :var ((mock-projectile-project-types
         '((foo src-dir "src"))))
  (it "gets set src directory"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo))
      (expect (projectile-src-directory 'foo) :to-equal "src")))
  (it "uses local override"
    (let ((projectile-project-types mock-projectile-project-types)
          (projectile-project-type 'foo)
          (projectile-project-src-dir "other"))
      (expect (projectile-src-directory 'foo) :to-equal "other"))))

(describe "projectile--merge-related-files-fns"
  (it "merges values from multiple functions for the same key"
    (let* ((fn1 (lambda (_path) '(:test ("test1.el"))))
           (fn2 (lambda (_path) '(:test ("test2.el"))))
           (merged (projectile--merge-related-files-fns (list fn1 fn2))))
      (expect (plist-get (funcall merged "src.el") :test)
              :to-equal '("test1.el" "test2.el"))))
  (it "merges values across different keys"
    (let* ((fn1 (lambda (_path) '(:test ("test.el"))))
           (fn2 (lambda (_path) '(:impl ("impl.el"))))
           (merged (projectile--merge-related-files-fns (list fn1 fn2))))
      (let ((result (funcall merged "src.el")))
        (expect (plist-get result :test) :to-equal '("test.el"))
        (expect (plist-get result :impl) :to-equal '("impl.el")))))
  (it "flattens a scalar value and a list value under the same key"
    (let* ((fn1 (lambda (_path) (list :foo "file1")))
           (fn2 (lambda (_path) (list :foo (list "file2" "file3"))))
           (merged (projectile--merge-related-files-fns (list fn1 fn2))))
      (expect (funcall merged "something") :to-equal '(:foo ("file1" "file2" "file3")))))
  (it "does not mutate original function return values"
    (let* ((shared-list '("test.el"))
           (fn1 (lambda (_path) (list :test shared-list)))
           (fn2 (lambda (_path) '(:test ("test2.el"))))
           (merged (projectile--merge-related-files-fns (list fn1 fn2))))
      (funcall merged "src.el")
      (expect shared-list :to-equal '("test.el")))))

(describe "projectile--file-name-extensions"
  (it "returns the single extension of a plain file"
    (expect (projectile--file-name-extensions "foo.el") :to-equal "el"))
  (it "returns the full nested extension"
    (expect (projectile--file-name-extensions "dir/bar.tar.gz") :to-equal "tar.gz"))
  (it "ignores a leading dot so dotfiles count as extension-less"
    (expect (projectile--file-name-extensions ".emacs") :to-equal ""))
  (it "returns an empty string when there is no extension"
    (expect (projectile--file-name-extensions "Makefile") :to-equal "")))

(describe "projectile--file-name-sans-extensions"
  (it "drops the extension of a plain file"
    (expect (projectile--file-name-sans-extensions "foo.el") :to-equal "foo"))
  (it "drops every nested extension"
    (expect (projectile--file-name-sans-extensions "dir/bar.tar.gz") :to-equal "bar"))
  (it "keeps a dotfile intact since its leading dot is not an extension"
    (expect (projectile--file-name-sans-extensions ".emacs") :to-equal ".emacs")))

(describe "projectile-associated-file-name-extensions"
  (it "looks up the associated extensions for a known extension"
    (expect (projectile-associated-file-name-extensions "foo.cpp")
            :to-equal '("h" "hpp" "ipp")))
  (it "falls back to a sub-extension when the full one has no mapping"
    (let ((projectile-other-file-alist '(("gz" "zip"))))
      (expect (projectile-associated-file-name-extensions "archive.tar.gz")
              :to-equal '("zip"))))
  (it "returns nil when no extension matches"
    (expect (projectile-associated-file-name-extensions "foo.unknown")
            :to-equal nil)))

(describe "projectile-group-file-candidates"
  (it "groups candidates by the number of shared leading path segments, best first"
    (expect (projectile-group-file-candidates
             "src/foo/test.el"
             '("src/foo/impl.el" "other/x.el" "src/bar.el"))
            :to-equal '((2 "src/foo/impl.el") (0 "other/x.el" "src/bar.el"))))
  (it "returns an empty list when there are no candidates"
    (expect (projectile-group-file-candidates "src/foo/test.el" nil)
            :to-equal nil)))

;;; projectile-relation-test.el ends here
