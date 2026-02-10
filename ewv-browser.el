;; -*- lexical-binding: t; -*-

(require 'ewv-extension)

(cl-defstruct ewv-browser-webview
  "webview2 instance wrapper"
  id
  frame
  hwnd
  bounds
  url
  html-string
  buffer
  )

(defvar-local ewv--local-webview nil)
(defun ewv-browser--monitor-window-configuration-change()
  (save-excursion
    (save-window-excursion
      (dolist (wind (window-list))
        ;; (window-old-buffer) 在 window-state-change-hook 中总是返回跟 new-buf 一样的值
          (let ((old-buf (window-old-buffer wind))
                (new-buf (window-buffer wind)))
            (with-selected-window wind
              ;; (when (and (bufferp old-buf) (buffer-live-p old-buf) (not (eq old-buf new-buf)))
              ;;   (with-current-buffer old-buf
              ;;     (when ewv--local-webview
              ;;       (ewv-native-webview-set-visible (ewv-browser-webview-id ewv--local-webview) nil)
              ;;       )))
              (with-current-buffer new-buf
                (when ewv--local-webview
                  (let ((bounds (ewv--get-window-edges))
                        (id (ewv-browser-webview-id ewv--local-webview)))
                    (unless (equal (ewv-browser-webview-hwnd ewv--local-webview) (ewv-get-frame-hwnd))
                      (ewv-native-webview-reparent id (ewv-get-frame-hwnd))
                      (setf (ewv-browser-webview-hwnd ewv--local-webview) (ewv-get-frame-hwnd))
                      (setf (ewv-browser-webview-frame ewv--local-webview) (selected-frame))
                      )
                    (setf (ewv-browser-webview-bounds ewv--local-webview) bounds)
                    (ewv-native-webview-resize id bounds)
                    (ewv-native-webview-set-visible id t)
                    )))))
          )

      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and ewv--local-webview  (not (get-buffer-window buf t)))
            (if (frame-live-p (ewv-browser-webview-frame ewv--local-webview))
                (ewv-native-webview-set-visible (ewv-browser-webview-id ewv--local-webview) nil)
              )
            )
          )
        )
      )))

(add-hook 'window-configuration-change-hook #'ewv-browser--monitor-window-configuration-change)

;; ;; window-configuration-change-hook not work for window deletion
;; (define-advice delete-window (:before  (&optional window) ewv-before-delte-window)
;;   (message "running delte -window")
;;   (unless (and window (window-live-p window))
;;     (with-current-buffer (window-buffer window)
;;       (when ewv--local-webview
;;         (ewv-native-webview-set-visible (ewv-browser-webview-id ewv--local-webview) nil)
;;         ))))
(defun ewv--delete-frame-function(frame)
  (dolist (wind (window-list frame))
    (let ((buf (window-buffer wind)))
      (with-current-buffer buf
        (when ewv--local-webview
          (let* ((new-frame (cl-find-if-not (lambda (f) (eq f frame)) (frame-list)))
                 (new-hwnd (ewv-get-frame-hwnd new-frame)))
            (ewv-native-init-for-frame new-hwnd)
            (ewv-native-webview-reparent (ewv-browser-webview-id ewv--local-webview) new-hwnd)
            (ewv-native-webview-set-visible (ewv-browser-webview-id ewv--local-webview) nil)
            (setf (ewv-browser-webview-hwnd ewv--local-webview) new-hwnd)
            (setf (ewv-browser-webview-frame ewv--local-webview) new-frame)
            )
          )
        ))
    )
  )
;; window-configuration-change-hook 运行的时候 frame 已经删除了，此时再 reparent 会 panic
;; TODO 这个 hook 也不保证一定在 frame 删除之前调用
(add-hook 'delete-frame-functions #'ewv--delete-frame-function)
(defun ewv--buffer-kill-hook()
  (when ewv--local-webview
    (let ((id (ewv-browser-webview-id ewv--local-webview)))
      (ewv-native-webview-close id)
      )))

(add-hook 'kill-buffer-hook #'ewv--buffer-kill-hook)

(defun ewv--browser-normalize-url(url)
  (if (or (string-prefix-p "https://" url)
          (string-prefix-p "http://" url)
          (string-prefix-p "chrome://" url)
          (string-prefix-p "file://" url)
          (string-prefix-p "chrome-extension://" url)
          (string-prefix-p "extension://" url)
          (string-prefix-p "edge://" url)
          (string-prefix-p "about:blank" url)
          )
      url
    (if (file-exists-p url)
        (concat "file://" (expand-file-name url))
      (concat "https://" url)))
  )

;;; browser: attached to buffer + occupy entire window

(defun ewv-browser--load (ewv-id url buffer)
    (ewv-native-webview-load ewv-id
                             (ewv--browser-normalize-url url)
                             (lambda (title url)
                               (with-current-buffer buffer
                                   (rename-buffer (format "*ewv-buffer-%d-%s*" ewv-id title))
                                   )
                                (switch-to-buffer buffer)
                               ))
)
(defun ewv-browser-open-url (url)
  (interactive "sUrl[https://www.baidu.com]: ")
  (when (string-empty-p url)
    (setq url "https://www.baidu.com"))
  (setq url (ewv--browser-normalize-url url))
  (let* ((hwnd (ewv-get-frame-hwnd))
         (ewv-id (ewv-native-webview-new hwnd (list 0 0 1 1)))
         (ewv-buffer-name (format "*ewv-buffer-%d*" ewv-id))
         (ewv-buffer (get-buffer-create ewv-buffer-name))
         (ewv-obj))
    (setq ewv-obj (make-ewv-browser-webview :id ewv-id :buffer ewv-buffer :hwnd hwnd :frame (selected-frame)))
    (with-current-buffer ewv-buffer
      ;; quick minor mode
      (setq ewv--local-webview ewv-obj)
      (keymap-local-set "i" (lambda () (interactive) (ewv-native-webview-focus ewv-id)(message "focus webview")))
      (keymap-local-set "t" (lambda () (interactive) (ewv-native-webview-set-visible ewv-id (not (ewv-native-webview-is-visible ewv-id)))(message "toggle")))
      (keymap-local-set "e" (lambda (url) (interactive "sNew Url: ") (ewv-browser--load ewv-id url ewv-buffer)))
      (keymap-local-set "f" (lambda (url) (interactive "fFile: ")    (ewv-browser--load ewv-id url ewv-buffer)))
      )
    (ewv--extension-load ewv-id)
    (ewv-browser--load ewv-id url ewv-buffer)
    )
  )
(defun ewv-browser-open-file (file)
  (interactive "fFile: ")
  (ewv-browser-open-url (ewv--browser-normalize-url file))
  )
(provide 'ewv-browser)
