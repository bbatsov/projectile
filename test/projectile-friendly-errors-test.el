;;; projectile-friendly-errors-test.el --- Tests for friendly errors when called without a project -*- lexical-binding: t -*-

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

;; Tests for friendly errors when called without a project.

;;; Code:

(require 'projectile-test-helpers)

(assert-friendly-error-when-no-project projectile-project-info)
(assert-friendly-error-when-no-project projectile-display-buffer)
(assert-friendly-error-when-no-project projectile-find-implementation-or-test-other-frame)
(assert-friendly-error-when-no-project projectile-find-implementation-or-test-other-window)
(assert-friendly-error-when-no-project projectile-find-other-file)
(assert-friendly-error-when-no-project projectile-find-other-file-other-frame)
(assert-friendly-error-when-no-project projectile-find-other-file-other-window)
(assert-friendly-error-when-no-project projectile-find-test-file)
(assert-friendly-error-when-no-project projectile-grep)
(assert-friendly-error-when-no-project projectile-ibuffer)
(assert-friendly-error-when-no-project projectile-project-buffers-other-buffer)
(assert-friendly-error-when-no-project projectile-remove-current-project-from-known-projects)
(assert-friendly-error-when-no-project projectile-switch-to-buffer)
(assert-friendly-error-when-no-project projectile-switch-to-buffer-other-frame)
(assert-friendly-error-when-no-project projectile-switch-to-buffer-other-window)

;;; projectile-friendly-errors-test.el ends here
