;; -*- lexical-binding: t; -*-

;;; rust debug env setup
;; does not work on Windows for dynamic module
;; (setenv "RUST_BACKTRACE" "1")
;; (setenv "WEBVIEW2_ADDITIONAL_BROWSER_ARGUMENTS" "--proxy-server=http://127.0.0.1:7887")
;; use set command
;; set RUST_BACKTRACE=1
;; set WEBVIEW2_ADDITIONAL_BROWSER_ARGUMENTS="--proxy-server=127.0.0.1:7897"

;; cargo build --lib && emacs  --debug-init -Q -L . -l ewv.el

(require 'cl-lib)

(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))

(require 'ewv-core)


(defun ewv--get-all-frames()
  (seq-filter (lambda (frame)
                (null (frame-parent frame)))
              (frame-list)))

(defun prepare-for-all-frames()
  (cl-loop for f in (ewv--get-all-frames)
           do
           (ewv-native-init-for-frame
            (ewv-get-frame-hwnd f))))

(prepare-for-all-frames)


(defun ewv--get-window-edges(&optional window)
  (window-body-pixel-edges window))



;; 配合 dynamic module 处理异步事件 https://nullprogram.com/blog/2017/02/14/
(define-key special-event-map [language-change]
  (lambda ()
    (interactive)
    (ewv-native-webview-process-events)))

(require 'ewv-browser)
;; (require 'ewv-mode-line)