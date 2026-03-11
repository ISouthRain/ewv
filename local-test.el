;; -*- lexical-binding: t; -*-

(setq debug-on-error t)
(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(require 'ewv)
;; https://github.com/prem-k-r/MaterialYouNewTab
(setq ewv-browser-default-url "extension://jjpokbgpiljgndebfoljdeihhkpcpfgl/index.html")

(setq ewv-browser-default-url "https://www.baidu.com")

; (setq ewv-browser-user-data-folder (expand-file-name "myuserdir" invocation-directory))

(add-hook 'emacs-startup-hook #'ewv-browser-reopen-all-pinned-tabs)