;;; mylisp.el --- My lisp functions  -*- lexical-binding: t -*-

;; Copyright (C)   2024 pxel8063

;; Author:     pxel8063 <pxel8063@gmail.com>
;; Version:    0.2.0
;; Keywords:   lisp
;; Package-Requires: ((emacs "28.1") (org "9.5") (org-roam "2.2.2"))
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

(require 'org)
(require 'org-roam)
(require 'org-roam-dailies)

(defvar mylisp-organization-task-id nil
    "The id property of the default task heading")

(defun mylisp-clock-in-default-task (task-id)
  (save-excursion
    (org-with-point-at (org-id-find task-id 'marker)
      (org-clock-in '(16)))))

(defun mylisp-clock-in-organization-task-as-default ()
  "Clock in the organization task, which is defined by
 `mylisp-organization-task-id'."
  (interactive)
  (if mylisp-organization-task-id
      (mylisp-clock-in-default-task mylisp-organization-task-id))
  (error "set mylisp-organization-task-id"))

(defvar mylisp-org-roam-agenda-files-store nil "Store org-agenda-files to restore later.")

(defun mylisp-org-roam-filter-by-tag (tag-name)
  "Filter org-roam-node by TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun mylisp-org-roam-list-notes-by-tag (tag-name)
  "Return the file names which contain org-roam TAG-NAME)."
  (delete-dups
   (mapcar #'org-roam-node-file
	   (seq-filter
	    (mylisp-org-roam-filter-by-tag tag-name)
	    (org-roam-node-list)))))

(defconst mylisp-org-roam-filter-time-range 1209600 "Include the roam nodes for four weeks.")


(defun mylisp-org-roam-filter-by-mtime ()
  "Filter org-roam-node by mtime."
  (lambda (node)
    (let (ret)
      (setq ret (org-roam-node-file-mtime node))
      (if (time-less-p ret (time-subtract (current-time) mylisp-org-roam-filter-time-range))
	  (setq ret nil)
	ret))))


(defun mylisp-org-roam-list-notes-by-mtime()
  "Return the file names which are mtime is greter than specific date."
  (delete-dups
   (mapcar #'org-roam-node-file
	   (seq-filter
	    (mylisp-org-roam-filter-by-mtime)
	    (org-roam-node-list)))))


;;;###autoload
(defun mylisp-org-roam-refresh-agenda-list ()
  "Over write org-agenda-files to org-roam nodes."
  (interactive)
  (setq org-agenda-files (mylisp-org-roam-list-notes-by-tag "Project")))

;;;###autoload
(defun mylisp-org-roam-refresh-agenda-list-by-all-roam-files ()
  "Over write org-agenda-files to org-roam nodes."
  (interactive)
  (setq org-agenda-files (mylisp-org-roam-list-notes-by-mtime)))

;;;###autoload
(defun mylisp-org-roam-copy-note-to-daily ()
  "Refile entry with an inactive timestamp to a daily on that date."
  (interactive)
  (let ((org-refile-keep nil)
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head "%<%Y%m%d>.org" "#+title: %<%Y%m%d>\n#+filetags: :journal:\n" ))))
        (org-after-refile-insert-hook #'save-buffer)
        cur
        today-file  ;; buffer-file-name during the capture
        pos)
    (setq cur (encode-time (org-parse-time-string (org-entry-get (point) "TIMESTAMP_IA"))))
    (save-window-excursion
      ;; avoid to recognized as org-roam-node
      (org-entry-delete (point) "ID")
      ;; create a capture buffer and set today-file and pos
      (org-roam-dailies--capture cur t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list nil today-file nil pos)))))

;;;###autoload
(defun mylisp-org-roam-toggle-agenda-files ()
  "Toggle org-agenda-files."
  (interactive)
  (unless mylisp-org-roam-agenda-files-store
    (setq mylisp-org-roam-agenda-files-store org-agenda-files))
  (if (eq mylisp-org-roam-agenda-files-store org-agenda-files)
      (setq org-agenda-files
	    (append org-agenda-files (mylisp-org-roam-list-notes-by-tag "journal")))
    (setq org-agenda-files mylisp-org-roam-agenda-files-store)))

;;;###autoload
(defun mylisp-org-roam-toggle-agenda-files-mtime ()
  "Toggle org-agenda-files."
  (interactive)
  (unless mylisp-org-roam-agenda-files-store
    (setq mylisp-org-roam-agenda-files-store org-agenda-files))
  (if (eq mylisp-org-roam-agenda-files-store org-agenda-files)
      (setq org-agenda-files
	    (append org-agenda-files (mylisp-org-roam-list-notes-by-mtime)))
    (setq org-agenda-files mylisp-org-roam-agenda-files-store)))

;;;###autoload
(defun mylisp-myconfig ()
  "Invoke `find-file' init.el."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

;;;###autoload
(defun mylisp-switch-term-buffer ()
  "Toggle `ansi-term' buffer and the current buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (ansi-term "fish")
    (if (not (eq (current-buffer) (get-buffer "*ansi-term*")))
	(switch-to-buffer (get-buffer "*ansi-term*"))
      (switch-to-buffer (other-buffer)))))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode of BUFFER-OR-STRING"
  (with-current-buffer buffer-or-string
    major-mode))

;;;###autoload
(defun mylisp-switch-haskell-inferior-buffer ()
  "Toggle buffer and the current buffer."
  (interactive)
  (let* ((current-mode (buffer-mode (current-buffer)))
	 (haskell-buffer (if (string= "haskell-interactive-mode" current-mode)
			     (current-buffer))))
    (if (and (not (string= "haskell-mode" current-mode)) haskell-buffer)
	(switch-to-prev-buffer)
      (switch-to-buffer haskell-buffer))))

;;;###autoload
(defun mylisp-kindle-highlight-org-heading ()
  "Convert kindle highlight to org heading."
  (interactive)
  (save-excursion
    (progn
      (point-min)
      (while (search-forward "ハイライト" (point-max) t)
	(org-toggle-heading 2))
      (message "Done"))))

(defun mylisp-insert-new-todo-heading-with-page (headline counter)
  "Insert a new todo-heading with HEADLINE as a head line and with page COUNTER."
  (let (tmpheadline)
    (setq tmpheadline (concat headline " p" (number-to-string counter)))
    (org-insert-todo-heading '(16))
    (insert-before-markers tmpheadline)))

(defun mylisp-insert-new-todo-heading-with-page-count (arg)
  "Insert a new todo-heading which is read from minibuffer.
The number of todo item is determind by \\[universal-argument] number, ARG"
  (interactive "P")
  (let ((item (read-from-minibuffer "Headline: "))
	(count 1))
    (while (<= count arg)
      (mylisp-insert-new-todo-heading-with-page item count)
      (setq count (+ count 1)))))

(provide 'mylisp)
;;; mylisp.el ends here
