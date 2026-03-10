;; -*- lexical-binding: t; -*-

(setq debug-on-error t)
(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(require 'ewv)
;; https://github.com/prem-k-r/MaterialYouNewTab
(setq ewv-browser-default-url "extension://jjpokbgpiljgndebfoljdeihhkpcpfgl/index.html")

(add-hook 'emacs-startup-hook #'ewv-browser-reopen-all-pinned-urls)