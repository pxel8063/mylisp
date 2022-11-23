;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  pxel8063

;; Author: pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.1
;; Keywords:   lisp
;; Package-Requires: ((emacs "27.1") (leaf "4.5.5") (leaf-keywords "1.1"))

;; URL: https://github.com/pxel8063/mylisp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

(leaf-keywords-init)

(leaf leaf-convert :straight t)

(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
