;;; projectile-test.el --- Unit tests for Projectile

;; Copyright (C) 2011-2012 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:


(require 'projectile)

(ert-deftest projectile-test-string-suffix-p ()
  (should (projectile-string-suffix-p "test.txt" "txt"))
  (should (not (projectile-string-suffix-p "test.txt" "rb"))))

(ert-deftest projectile-test-uniquify-file ()
  (should (equal (uniquify-file "ala/bala/portokala") "bala/portokala")))

(ert-deftest projectile-test-project-get-name ()
  (should (equal (projectile-get-project-name) "projectile")))

(ert-deftest projectile-test-prepend-project-name ()
  (should (equal (projectile-prepend-project-name "Test") "[projectile] Test")))

;;; projectile-tests.el ends here
