;;; projectile-cmake-test.el --- Tests for Projectile's CMake support -*- lexical-binding: t -*-

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

;; Tests for the CMake project type: version detection and gating,
;; CMakePresets.json / CMakeUserPresets.json discovery, preset
;; selection, and the command strings produced for configure /
;; compile / test / install / package.

;;; Code:

(require 'projectile-test-helpers)

(defconst projectile-cmake-test--presets-json
  "{
  \"version\": 3,
  \"configurePresets\": [
    { \"name\": \"base\", \"hidden\": true, \"generator\": \"Ninja\" },
    { \"name\": \"release\", \"inherits\": \"base\" },
    { \"name\": \"debug\", \"inherits\": \"base\" }
  ],
  \"buildPresets\": [ { \"name\": \"build-release\" } ],
  \"testPresets\": [ { \"name\": \"test-release\" } ],
  \"packagePresets\": [ { \"name\": \"package-release\" } ]
}"
  "A CMakePresets.json fixture with one hidden configure preset.")

(defconst projectile-cmake-test--user-presets-json
  "{ \"version\": 3, \"configurePresets\": [ { \"name\": \"user-debug\" } ] }"
  "A CMakeUserPresets.json fixture with a single configure preset.")

(defun projectile-cmake-test--setup-project (&rest files)
  "Create a CMake project in the sandbox containing FILES.

FILES are (FILENAME . CONTENTS) cons cells created relative to the
project root; a CMakeLists.txt is always created.
`projectile-project-root' is stubbed to the project root, which is
returned."
  (let ((root (expand-file-name "project/" default-directory)))
    (make-directory root t)
    (with-temp-file (expand-file-name "CMakeLists.txt" root))
    (dolist (file files)
      (let ((path (expand-file-name (car file) root)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert (cdr file)))))
    (let ((true-root (file-truename root)))
      (spy-on 'projectile-project-root :and-return-value true-root)
      true-root)))

(describe "projectile--cmake-version"
  (it "parses the version from cmake --version output"
    (spy-on 'shell-command-to-string
            :and-return-value
            "cmake version 3.25.1\n\nCMake suite maintained and supported by Kitware (kitware.com/cmake).\n")
    (expect (projectile--cmake-version) :to-equal '(3 25 1)))

  (it "tolerates suffixes after the version number"
    (spy-on 'shell-command-to-string
            :and-return-value "cmake version 3.29.2-dirty\n")
    (expect (projectile--cmake-version) :to-equal '(3 29 2)))

  (it "returns nil when cmake is not installed"
    (spy-on 'shell-command-to-string
            :and-return-value "sh: cmake: command not found\n")
    (expect (projectile--cmake-version) :to-be nil)))

(describe "projectile--cmake-check-version"
  (it "accepts a newer installed version"
    (spy-on 'projectile--cmake-version :and-return-value '(3 25 1))
    (expect (projectile--cmake-check-version '(3 19)) :to-be-truthy))

  (it "accepts an equal installed version"
    (spy-on 'projectile--cmake-version :and-return-value '(3 19 0))
    (expect (projectile--cmake-check-version '(3 19)) :to-be-truthy))

  (it "rejects an older installed version"
    (spy-on 'projectile--cmake-version :and-return-value '(3 18 4))
    (expect (projectile--cmake-check-version '(3 19)) :not :to-be-truthy))

  (it "rejects when the version cannot be determined"
    (spy-on 'projectile--cmake-version :and-return-value nil)
    (expect (projectile--cmake-check-version '(3 19)) :not :to-be-truthy)))

(describe "projectile--cmake-read-preset"
  (it "returns nil for a missing file"
    (projectile-test-with-sandbox
      (expect (projectile--cmake-read-preset
               (expand-file-name "CMakePresets.json" default-directory))
              :to-be nil)))

  (it "parses a preset file into a hash table with list arrays"
    (projectile-test-with-sandbox
      (let ((root (projectile-cmake-test--setup-project
                   (cons "CMakePresets.json" projectile-cmake-test--presets-json))))
        (let ((preset (projectile--cmake-read-preset
                       (expand-file-name "CMakePresets.json" root))))
          (expect (hash-table-p preset) :to-be-truthy)
          (expect (listp (gethash "configurePresets" preset)) :to-be-truthy)
          (expect (length (gethash "configurePresets" preset)) :to-equal 3)))))

  (it "signals an error on malformed JSON"
    (projectile-test-with-sandbox
      (let ((root (projectile-cmake-test--setup-project
                   '("CMakePresets.json" . "{ not json"))))
        (expect (projectile--cmake-read-preset
                 (expand-file-name "CMakePresets.json" root))
                :to-throw)))))

(describe "projectile--cmake-command-presets-shallow"
  (it "returns the presets for the command type, excluding hidden ones"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project
       (cons "CMakePresets.json" projectile-cmake-test--presets-json))
      (let ((presets (projectile--cmake-command-presets-shallow
                      "CMakePresets.json" :configure-command)))
        (expect (mapcar (lambda (preset) (gethash "name" preset)) presets)
                :to-equal '("release" "debug")))))

  (it "returns nil when the file has no presets for the command type"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project
       '("CMakePresets.json" . "{ \"version\": 3, \"configurePresets\": [ { \"name\": \"only\" } ] }"))
      (expect (projectile--cmake-command-presets-shallow
               "CMakePresets.json" :test-command)
              :to-be nil)))

  (it "returns nil when the file does not exist"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project)
      (expect (projectile--cmake-command-presets-shallow
               "CMakePresets.json" :configure-command)
              :to-be nil))))

(describe "projectile--cmake-command-presets"
  (it "follows included preset files relative to the including file"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project
       '("CMakePresets.json" .
         "{ \"version\": 4, \"include\": [ \"extra/more-presets.json\" ], \"configurePresets\": [ { \"name\": \"top\" } ] }")
       '("extra/more-presets.json" .
         "{ \"version\": 4, \"include\": [ \"nested.json\" ], \"configurePresets\": [ { \"name\": \"included\" } ] }")
       '("extra/nested.json" .
         "{ \"version\": 4, \"configurePresets\": [ { \"name\": \"nested\" } ] }"))
      ;; `default-directory' is the sandbox here, not the project root, so
      ;; this also covers anchoring the top-level relative filename to the
      ;; project root rather than `default-directory'.
      (expect (mapcar (lambda (preset) (gethash "name" preset))
                      (projectile--cmake-command-presets
                       "CMakePresets.json" :configure-command))
              :to-equal '("top" "included" "nested")))))

(describe "projectile--cmake-command-preset-names"
  (it "merges user presets before system presets"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project
       (cons "CMakePresets.json" projectile-cmake-test--presets-json)
       (cons "CMakeUserPresets.json" projectile-cmake-test--user-presets-json))
      (expect (projectile--cmake-command-preset-names :configure-command)
              :to-equal '("user-debug" "release" "debug"))))

  (it "returns nil when no preset files exist"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project)
      (expect (projectile--cmake-command-preset-names :configure-command)
              :to-be nil))))

(describe "projectile--cmake-use-command-presets"
  (it "returns nil when projectile-enable-cmake-presets is off"
    (spy-on 'projectile--cmake-version :and-return-value '(3 25 1))
    (let ((projectile-enable-cmake-presets nil))
      (expect (projectile--cmake-use-command-presets :configure-command)
              :not :to-be-truthy)))

  (it "returns nil when the installed CMake is too old for presets"
    (spy-on 'projectile--cmake-version :and-return-value '(3 18 0))
    (let ((projectile-enable-cmake-presets t))
      (expect (projectile--cmake-use-command-presets :configure-command)
              :not :to-be-truthy)))

  (it "honors the per-command minimum versions"
    ;; configure/package presets arrived in 3.19, the rest in 3.20
    (spy-on 'projectile--cmake-version :and-return-value '(3 19 0))
    (let ((projectile-enable-cmake-presets t))
      (expect (projectile--cmake-use-command-presets :configure-command) :to-be-truthy)
      (expect (projectile--cmake-use-command-presets :package-command) :to-be-truthy)
      (expect (projectile--cmake-use-command-presets :compile-command) :not :to-be-truthy)
      (expect (projectile--cmake-use-command-presets :test-command) :not :to-be-truthy)
      (expect (projectile--cmake-use-command-presets :install-command) :not :to-be-truthy)))

  (it "returns nil when cmake is not installed"
    (spy-on 'shell-command-to-string :and-return-value "sh: cmake: command not found\n")
    (let ((projectile-enable-cmake-presets t))
      (expect (projectile--cmake-use-command-presets :configure-command)
              :not :to-be-truthy))))

(describe "projectile--cmake-select-command"
  (it "returns *no preset* without prompting when presets are disabled"
    (spy-on 'projectile-completing-read)
    (let ((projectile-enable-cmake-presets nil))
      (expect (projectile--cmake-select-command :configure-command)
              :to-equal projectile--cmake-no-preset)
      (expect 'projectile-completing-read :not :to-have-been-called)))

  (it "returns *no preset* without prompting when there are no preset files"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project)
      (spy-on 'projectile--cmake-version :and-return-value '(3 25 1))
      (spy-on 'projectile-completing-read)
      (let ((projectile-enable-cmake-presets t))
        (expect (projectile--cmake-select-command :configure-command)
                :to-equal projectile--cmake-no-preset)
        (expect 'projectile-completing-read :not :to-have-been-called))))

  (it "prompts with the discovered presets plus the *no preset* choice"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project
       (cons "CMakePresets.json" projectile-cmake-test--presets-json)
       (cons "CMakeUserPresets.json" projectile-cmake-test--user-presets-json))
      (spy-on 'projectile--cmake-version :and-return-value '(3 25 1))
      (spy-on 'projectile-completing-read :and-return-value "debug")
      (let ((projectile-enable-cmake-presets t))
        (expect (projectile--cmake-select-command :configure-command)
                :to-equal "debug")
        (expect 'projectile-completing-read
                :to-have-been-called-with
                "Use preset: "
                `("user-debug" "release" "debug" ,projectile--cmake-no-preset)
                :caller nil)))))

(describe "CMake command generation"
  (describe "without presets"
    (it "produces the default manual commands"
      (let ((projectile-enable-cmake-presets nil))
        (expect (projectile--cmake-configure-command)
                :to-equal "cmake -S . -B build")
        (expect (projectile--cmake-compile-command)
                :to-equal "cmake --build build")
        (expect (projectile--cmake-test-command)
                :to-equal "cmake --build build --target test")
        (expect (projectile--cmake-install-command)
                :to-equal "cmake --build build --target install")
        (expect (projectile--cmake-package-command)
                :to-equal "cmake --build build --target package"))))

  (describe "with presets"
    (it "produces preset commands from the selected preset"
      (projectile-test-with-sandbox
        (projectile-cmake-test--setup-project
         (cons "CMakePresets.json" projectile-cmake-test--presets-json))
        (spy-on 'projectile--cmake-version :and-return-value '(3 25 1))
        ;; pick the first candidate offered for each command type
        (spy-on 'projectile-completing-read
                :and-call-fake (lambda (_prompt choices &rest _) (car choices)))
        (let ((projectile-enable-cmake-presets t))
          (expect (projectile--cmake-configure-command)
                  :to-equal "cmake . --preset release")
          (expect (projectile--cmake-compile-command)
                  :to-equal "cmake --build --preset build-release")
          (expect (projectile--cmake-test-command)
                  :to-equal "ctest --preset test-release")
          (expect (projectile--cmake-install-command)
                  :to-equal "cmake --build --preset build-release --target install")
          (expect (projectile--cmake-package-command)
                  :to-equal "cpack --preset package-release"))))

    (it "falls back to the manual command when *no preset* is selected"
      (projectile-test-with-sandbox
        (projectile-cmake-test--setup-project
         (cons "CMakePresets.json" projectile-cmake-test--presets-json))
        (spy-on 'projectile--cmake-version :and-return-value '(3 25 1))
        (spy-on 'projectile-completing-read
                :and-return-value projectile--cmake-no-preset)
        (let ((projectile-enable-cmake-presets t))
          (expect (projectile--cmake-configure-command)
                  :to-equal "cmake -S . -B build"))))))

(describe "CMake preset selection on repeated invocations (#1676)"
  (it "prompts anew on every direct invocation - the CMake layer doesn't cache"
    (projectile-test-with-sandbox
      (projectile-cmake-test--setup-project
       (cons "CMakePresets.json" projectile-cmake-test--presets-json))
      (spy-on 'projectile--cmake-version :and-return-value '(3 25 1))
      (let ((selections '("release" "debug")))
        (spy-on 'projectile-completing-read
                :and-call-fake (lambda (&rest _) (pop selections)))
        (let ((projectile-enable-cmake-presets t))
          (expect (projectile--cmake-configure-command)
                  :to-equal "cmake . --preset release")
          (expect (projectile--cmake-configure-command)
                  :to-equal "cmake . --preset debug")
          (expect (spy-calls-count 'projectile-completing-read) :to-equal 2)))))

  (it "is bypassed by projectile-configure-command's generic command caching"
    ;; This is the actual mechanism behind #1676: once a command has
    ;; been run, `projectile-configure-command' returns the cached
    ;; string from `projectile-configure-cmd-map' and the preset prompt
    ;; is never reached again.  Users can opt out via
    ;; `projectile-project-enable-cmd-caching' or clear the cache with
    ;; `projectile-discard-command-cache'.
    (let ((projectile-configure-cmd-map (make-hash-table :test 'equal)))
      (puthash "/proj/" "cmake . --preset release" projectile-configure-cmd-map)
      (spy-on 'projectile-default-configure-command)
      (expect (projectile-configure-command "/proj/")
              :to-equal "cmake . --preset release")
      (expect 'projectile-default-configure-command :not :to-have-been-called))))

(provide 'projectile-cmake-test)

;;; projectile-cmake-test.el ends here
