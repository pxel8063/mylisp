;;; -*- lexical-binding: t -*-

;;; Copyright (C) 2018 Paul Pogonyshev

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

(describe "tramp-remote-path"
	  (it "can build the tramp-remote-path as desired"
	      (expect (sf/add-nixos-path-to-tramp-remote-path) :to-equal
'("/run/current-system/sw/bin" "/nix/var/nix/profiles/default/bin" "/etc/profiles/per-user/cosmic/bin" "/home/cosmic/.nix-profile/bin" "/run/wrappers/bin" "/home/cosmic/.local/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")
			       )))

(ert-deftest addition-test ()
  (should (= 1 1)))

(ert-deftest nixos ()
   (let* ((tramp-default-remote-path 'tramp-default-remote-path))
     (should
      (equal (sf/add-nixos-path-to-tramp-remote-path)
'("/run/current-system/sw/bin" "/nix/var/nix/profiles/default/bin" "/etc/profiles/per-user/cosmic/bin" "/home/cosmic/.nix-profile/bin" "/run/wrappers/bin" "/home/cosmic/.local/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")
			       ))))

(provide 'mylisp-test)
;;; mylisp-test.el ends here

