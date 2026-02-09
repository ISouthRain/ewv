;; -*- lexical-binding: t; -*-
;;; debug
;; (set-face-attribute 'mode-line nil :height 140)
;; (set-face-attribute 'mode-line-inactive nil :height 140)

(defvar ewv-mode-line--webview-list nil)

(defvar ewv-mode-line-file
  (expand-file-name "modeline.html" (file-name-directory (or load-file-name (buffer-file-name)))))
(setq ewv-mode-line--webview-list nil)
(cl-defstruct ewv-mode-line-webview
  ""
  w
  id
  )
(defun ewv-mode-line--get-bounds(&optional window)
  (let ((b1 (window-pixel-edges window))
        (b2 (window-body-pixel-edges window)))
    (list (nth 0 b1) (nth 3 b2) (nth 2 b1) (nth 3 b1))))

(defun ewv-mode-line--monitor-window-mode-line()
  (save-excursion
    (save-window-excursion
      (let* ((division (seq-group-by (lambda (e) (window-live-p (ewv-mode-line-webview-w e))) ewv-mode-line--webview-list))
             (live-list (alist-get t division))
             (dead-list (alist-get nil division))
             )
        (when dead-list
          (cl-loop for w in dead-list
                   do (when (ewv-mode-line-webview-id w)
                        (ewv-native-webview-close (ewv-mode-line-webview-id w))
                        )
                   )
          )
        (when live-list
          (cl-loop for w in live-list
                   do (when (ewv-mode-line-webview-id w)
                        (ewv-native-webview-resize (ewv-mode-line-webview-id w) (ewv-mode-line--get-bounds (ewv-mode-line-webview-w w)))
                        )
                   )
          )
        (setq ewv-mode-line--webview-list live-list)
        (dolist (w (seq-filter (lambda (e) (not (window-minibuffer-p e))) (window-list)))
          (unless (seq-find (lambda (e) (eq w (ewv-mode-line-webview-w e)))
                            live-list)
            (ewv--print "new window %S " w)
            (let ((new-id (ewv-native-webview-new (ewv-get-frame-hwnd) (ewv-mode-line--get-bounds w))))
              (ewv-native-webview-load new-id ewv-mode-line-file)
              (push (make-ewv-mode-line-webview :w w :id new-id) ewv-mode-line--webview-list)
              )
            )
          )
        (dolist (wv ewv-mode-line--webview-list)
          (ewv-with-struct-slots (w id) ewv-mode-line-webview wv
                                 (with-current-buffer (window-buffer w)
                                   (ewv-native-webview-eval-js id (format "updateModeLine('%s', '%s')"
                                                                          (buffer-name)
                                                                          (eq (selected-window) w)
                                                                          ) #'identity)
                                   )
                                 )
          )
        )
      )
    )
  )
(add-hook 'window-state-change-hook #'ewv-mode-line--monitor-window-mode-line)

(provide 'ewv-mode-line)