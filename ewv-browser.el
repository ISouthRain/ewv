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

(defvar-local ewv-browser--local-webview nil)
(defvar-local ewv-browser--on-navigation-starting nil)
(defvar-local ewv-browser--registered nil)

(defvar ewv-browser--active-count 0)


(defun ewv-browser-focus-webview ()
  (interactive)
  (when ewv-browser--local-webview
    (ewv-native-webview-focus (ewv-browser-webview-id ewv-browser--local-webview))
    (message "focus webview")))

(defun ewv-browser-toggle-visibility ()
  (interactive)
  (when ewv-browser--local-webview
    (let ((id (ewv-browser-webview-id ewv-browser--local-webview)))
      (ewv-native-webview-set-visible id (not (ewv-native-webview-is-visible id)))
      (message "toggle"))))

(defun ewv-browser-load-url (url)
  (interactive "sNew Url: ")
  (when ewv-browser--local-webview
    (ewv-browser--load (ewv-browser-webview-id ewv-browser--local-webview) url (ewv-browser-webview-buffer ewv-browser--local-webview))))

(defun ewv-browser-load-file (file)
  (interactive "fFile: ")
  (when ewv-browser--local-webview
    (ewv-browser--load (ewv-browser-webview-id ewv-browser--local-webview) file (ewv-browser-webview-buffer ewv-browser--local-webview))))

(defun ewv-browser-set-on-navigation-starting (callback)
  (setq ewv-browser--on-navigation-starting callback))

(defun ewv-browser--enable-global-hooks ()
  (add-hook 'window-configuration-change-hook #'ewv-browser--monitor-window-configuration-change)
  (add-hook 'delete-frame-functions #'ewv--delete-frame-function))

(defun ewv-browser--disable-global-hooks ()
  (remove-hook 'window-configuration-change-hook #'ewv-browser--monitor-window-configuration-change)
  (remove-hook 'delete-frame-functions #'ewv--delete-frame-function))

(defun ewv-browser--register-buffer ()
  (unless ewv-browser--registered
    (setq ewv-browser--registered t)
    (cl-incf ewv-browser--active-count)
    (when (= ewv-browser--active-count 1)
      (ewv-browser--enable-global-hooks))))

(defun ewv-browser--buffer-exited ()
  (when ewv-browser--registered
    (setq ewv-browser--registered nil)
    (cl-decf ewv-browser--active-count)
    (when (<= ewv-browser--active-count 0)
      (setq ewv-browser--active-count 0)
      (ewv-browser--disable-global-hooks))))

(defvar ewv-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'ewv-browser-focus-webview)
    (define-key map (kbd "t") #'ewv-browser-toggle-visibility)
    (define-key map (kbd "e") #'ewv-browser-load-url)
    (define-key map (kbd "f") #'ewv-browser-load-file)
    map))

(define-derived-mode ewv-browser-mode special-mode "EWV-Browser"
  (use-local-map ewv-browser-mode-map)
  (add-hook 'kill-buffer-hook #'ewv--buffer-kill-hook nil t)
  (add-hook 'kill-buffer-hook #'ewv-browser--buffer-exited nil t)
  (add-hook 'change-major-mode-hook #'ewv-browser--buffer-exited nil t)
  (ewv-browser--register-buffer))
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
            ;;     (when ewv-browser--local-webview
            ;;       (ewv-native-webview-set-visible (ewv-browser-webview-id ewv-browser--local-webview) nil)
            ;;       )))
            (with-current-buffer new-buf
              (when ewv-browser--local-webview
                (let ((bounds (ewv--get-window-edges))
                      (id (ewv-browser-webview-id ewv-browser--local-webview)))
                  (unless (equal (ewv-browser-webview-hwnd ewv-browser--local-webview) (ewv-get-frame-hwnd))
                    (ewv-native-webview-reparent id (ewv-get-frame-hwnd))
                    (setf (ewv-browser-webview-hwnd ewv-browser--local-webview) (ewv-get-frame-hwnd))
                    (setf (ewv-browser-webview-frame ewv-browser--local-webview) (selected-frame))
                    )
                  (setf (ewv-browser-webview-bounds ewv-browser--local-webview) bounds)
                  (ewv-native-webview-resize id bounds)
                  (ewv-native-webview-set-visible id t)
                  )))))
        )

      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and ewv-browser--local-webview  (not (get-buffer-window buf t)))
            (if (frame-live-p (ewv-browser-webview-frame ewv-browser--local-webview))
                (ewv-native-webview-set-visible (ewv-browser-webview-id ewv-browser--local-webview) nil)
              )
            )
          )
        )
      )))


;; ;; window-configuration-change-hook not work for window deletion
;; (define-advice delete-window (:before  (&optional window) ewv-before-delte-window)
;;   (message "running delte -window")
;;   (unless (and window (window-live-p window))
;;     (with-current-buffer (window-buffer window)
;;       (when ewv-browser--local-webview
;;         (ewv-native-webview-set-visible (ewv-browser-webview-id ewv-browser--local-webview) nil)
;;         ))))
(defun ewv--delete-frame-function(frame)
  (dolist (wind (window-list frame))
    (let ((buf (window-buffer wind)))
      (with-current-buffer buf
        (when ewv-browser--local-webview
          (let* ((new-frame (cl-find-if-not (lambda (f) (eq f frame)) (frame-list)))
                 (new-hwnd (ewv-get-frame-hwnd new-frame)))
            (ewv-native-init-for-frame new-hwnd)
            (ewv-native-webview-reparent (ewv-browser-webview-id ewv-browser--local-webview) new-hwnd)
            (ewv-native-webview-set-visible (ewv-browser-webview-id ewv-browser--local-webview) nil)
            (setf (ewv-browser-webview-hwnd ewv-browser--local-webview) new-hwnd)
            (setf (ewv-browser-webview-frame ewv-browser--local-webview) new-frame)
            )
          )
        ))
    )
  )
;; window-configuration-change-hook 运行的时候 frame 已经删除了，此时再 reparent 会 panic
;; TODO 这个 hook 也不保证一定在 frame 删除之前调用
(defun ewv--buffer-kill-hook()
  (when ewv-browser--local-webview
    (let ((id (ewv-browser-webview-id ewv-browser--local-webview)))
      (ewv-native-webview-close id)
      )))


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
    (ewv-native-webview-set-on-new-window-requested ewv-id (lambda (url)
                                                             (ewv-browser-open-url url)
                                                             ;; (ewv-native-webview-load ewv-id
                                                             ;;                          (ewv--browser-normalize-url url) #'ignore)
                                                             t
                                                             ))
    (ewv-native-webview-set-on-focus ewv-id (lambda () (select-window (get-buffer-window ewv-buffer))))
    (ewv-native-webview-set-on-navigation-starting ewv-id
                                                   (lambda (url)
                                                     (with-current-buffer ewv-buffer
                                                       (when ewv-browser--local-webview
                                                         (let ((bounds (ewv--get-window-edges))
                                                               (id (ewv-browser-webview-id ewv-browser--local-webview)))
                                                           (unless (equal (ewv-browser-webview-hwnd ewv-browser--local-webview) (ewv-get-frame-hwnd))
                                                             (ewv-native-webview-reparent id (ewv-get-frame-hwnd))
                                                             (setf (ewv-browser-webview-hwnd ewv-browser--local-webview) (ewv-get-frame-hwnd))
                                                             (setf (ewv-browser-webview-frame ewv-browser--local-webview) (selected-frame))
                                                             )
                                                           (setf (ewv-browser-webview-bounds ewv-browser--local-webview) bounds)
                                                           (ewv-native-webview-resize id bounds)
                                                           (ewv-native-webview-set-visible id t)
                                                           ))
                                                       (when ewv-browser--on-navigation-starting
                                                         (funcall ewv-browser--on-navigation-starting url)))))
    (with-current-buffer ewv-buffer
      (ewv-browser-mode)
      (setq ewv-browser--local-webview ewv-obj)
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
