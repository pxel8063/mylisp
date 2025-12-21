;;; mylisp.el --- My lisp functions  -*- lexical-binding: t -*-

;; Copyright (C)   2025 pxel8063

;; Author:     pxel8063 <pxel8063@gmail.com>
;; Version:    0.2.3
;; Keywords:   lisp
;; Package-Requires: ((emacs "29.1") (org "9.5"))
;; URL:        https://github.com/pxel8063/mylisp

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


;;; Commentary:

;;  mylisp functions


;;; Code:

(require 'tramp)
(require 'org)

(defun mylisp-set-ssh-auth-sock ()
  "Set a selected file name as SSH_AUTH_SOCK environmental variable"
  (interactive)
  (setenv "SSH_AUTH_SOCK" (read-file-name "SSH agent socket: " "/tmp/ssh")))

(defun mylisp-add-ssh-agent-to-tramp ()
  "Forward agent for ssh connections.
Be careful!"
  (cl-pushnew '("-A")
              (cadr (assoc 'tramp-login-args
			   (assoc "ssh" tramp-methods)))
              :test #'equal))

(defvar mylisp-default-task-id nil
  "The id org property of the default task heading.")

(defvar mylisp-break-task-id nil
  "The id org property of the break heading.")

(defun mylisp-clock-in-task (task-id arg)
  "Clock in org heading with TASK-ID."
  (save-excursion
    (org-with-point-at (org-id-find task-id 'marker)
      (org-clock-in arg))))

(defun mylisp-clock-in-break-task (arg)
  "Clock in the break task.
The task is defined by `mylisp-break-task-id'."
  (interactive "P")
  (if mylisp-break-task-id
      (mylisp-clock-in-task mylisp-break-task-id arg)
    (error "Error: set mylisp-break-task-id")))


(defun mylisp-clock-in-organization-task-as-default (arg)
  "Clock in the organization task.
The task is defined by `mylisp-default-task-id'."
  (interactive "P")
  (if mylisp-default-task-id
      (mylisp-clock-in-task mylisp-default-task-id arg)
    (error "Error: set mylisp-default-task-id")))

(provide 'mylisp)
;;; mylisp.el ends here
