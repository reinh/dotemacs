;(package-initialize)

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Load org-mode init file
(when (>= emacs-major-version 24)
  (require 'org)
  (org-babel-load-file
   (expand-file-name "conf/init.org" user-emacs-directory)))
