;;; mylisp-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from mylisp.el

(autoload 'mylisp-org-roam-refresh-agenda-list "mylisp" "\
Over write org-agenda-files to org-roam nodes." t)
(autoload 'mylisp-org-roam-refresh-agenda-list-by-all-roam-files "mylisp" "\
Over write org-agenda-files to org-roam nodes." t)
(autoload 'mylisp-org-roam-copy-note-to-daily "mylisp" "\
Refile entry with an inactive timestamp to a daily on that date." t)
(autoload 'mylisp-org-roam-toggle-agenda-files "mylisp" "\
Toggle org-agenda-files." t)
(autoload 'mylisp-org-roam-toggle-agenda-files-mtime "mylisp" "\
Toggle org-agenda-files." t)
(autoload 'mylisp-myconfig "mylisp" "\
Invoke `find-file' init.el." t)
(autoload 'mylisp-switch-term-buffer "mylisp" "\
Toggle `ansi-term' buffer and the current buffer." t)
(autoload 'mylisp-switch-haskell-inferior-buffer "mylisp" "\
Toggle buffer and the current buffer." t)
(autoload 'mylisp-kindle-highlight-org-heading "mylisp" "\
Convert kindle highlight to org heading." t)
(register-definition-prefixes "mylisp" '("mylisp-"))


;;; End of scraped data

(provide 'mylisp-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; mylisp-autoloads.el ends here
