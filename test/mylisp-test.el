;;; -*- lexical-binding: t -*-

;;; Copyright (C) 2024 pxel8063

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.

;;; Code:
(require 'mylisp)
(require 'ert)
(require 'buttercup)

(describe "Buttercup example. ERT support"
	  (it "allows you to use ERT macros in tests"
	      (should (= 1 1))))

(ert-deftest addition-test ()
  (should (= 1 1)))

(describe "clock-in functions"
	  (it "default task id nil"
	      (expect (mylisp-clock-in-organization-task-as-default) :to-throw)))

(describe "clock-in functions"
	  (it "break task id nil"
	      (expect (mylisp-clock-in-break-task-as-default) :to-throw)))

(provide 'mylisp-test)
;;; mylisp-test.el ends here
