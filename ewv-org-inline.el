;; -*- lexical-binding: t; -*-

(require 'org)
(defvar-local ewv-show-links nil)
(defvar-local ewv-scroll-hook-running nil)
(org-add-link-type "ewv")

(defun ewv-get-img-bounds()
  (let (bounds
        cinfo

        (wind-bounds (window-body-pixel-edges))
        left top right bottom
        )
    ;; (setq cinfo (cl-coerce (cl-subseq (window-cursor-info) 1 5) 'list))
    (setq cinfo (posn-x-y (posn-at-point)))
    (setq cinfo (flatten-list (list (posn-x-y (posn-at-point))
                                    (posn-object-width-height (posn-at-point)))))

    (setq left (+ (nth 0 cinfo) (nth 0 wind-bounds)))
    (setq top (+ (nth 1 cinfo) (nth 1 wind-bounds)))
    (setq right (min (+  (nth 2 cinfo) left) (nth 2 wind-bounds)))
    (setq bottom (min (+ (nth 3 cinfo) top) (nth 3 wind-bounds)))
    (setq bounds (list left top right bottom))
    (posn-x-y (posn-at-point))
    (posn-object-width-height (posn-at-point))
    (posn-at-point)
    bounds
    )
  )

;;; 使用这个来跟踪一个 image 来显示 webview
(defun ewv-window-scroll-hook (window start)
  ;; (run-with-timer 0.001 nil (lambda ()
                              (save-excursion
                                (with-selected-window window
                                  (with-current-buffer (window-buffer window)
                                    (unless ewv-scroll-hook-running
                                      (setq ewv-scroll-hook-running t)
                                      (goto-char (point-min))
                                      (let (pt
                                            rt
                                            ewv-id
                                            bounds
                                            begin
                                            end
                                            (win-start (window-start window))
                                            (win-end (window-end window t)))
                                        (while (setq pt (text-property-search-forward 'ewv-id))
                                          ;; (message "%S" (list :pt pt :point (point) :win-start win-start :win-end win-end))
                                          (setq begin (prop-match-beginning pt))
                                          (setq end (prop-match-end pt))
                                          (setq ewv-id (prop-match-value pt))
                                          (if (and (>= begin win-start) (<= end win-end))
                                              (save-excursion
                                                (goto-char (prop-match-beginning pt))
                                                (sit-for 0.01)
                                                (push (list :cursor-info (window-cursor-info) :ewv-id (prop-match-value pt) :point (point)) rt)
                                                (setq bounds (ewv-get-img-bounds))
                                                (ewv-native-webview-resize ewv-id bounds)
                                                (ewv-native-webview-set-visible ewv-id t)
                                                )
                                            (ewv-native-webview-set-visible ewv-id nil)
                                            )
                                          )
                                        rt
                                        )
                                      (setq ewv-scroll-hook-running nil)
                                      )
                                    ))
                                )
                              ;; ))
  )

(defun ewv-parse-org-links (link)
  (let* ((begin (org-element-begin link))
         (end (org-element-end link))
         (data (read (org-element-property :path link)))
         (width (plist-get data :width))
         (height (plist-get data :height))
         (url (plist-get data :url))
         (svg (svg-create width height))
         ewv-wv
         img
         bounds
         )
    (svg-rectangle svg 0 0 "100%" "100%" :fill-color "transparent" :stroke-width 0 :stroke-color "transparent")
    ;; (svg-rectangle svg 0 0 "100%" "100%" :fill-color "green")
    ;; (svg-rectangle svg 0 0 "100%" "100%" :fill-color "white")
    (setq img (svg-image svg))
    (list begin end data)
    (org-with-silent-modifications
     (put-text-property begin end 'display img)
     )
    (unless (get-text-property begin 'ewv-id)
      (save-excursion (goto-char begin)
                      (sit-for 0.001)
                      (setq bounds (ewv-get-img-bounds))
                      (setq ewv-wv (ewv-native-webview-new (ewv-get-frame-hwnd) bounds))
                      (ewv-native-webview-load ewv-wv url)
                      ;; (ewv-native-webview-resize ewv-wv width height)
                      (org-with-silent-modifications
                       (put-text-property begin end 'ewv-id ewv-wv)
                       (put-text-property begin end 'ewv-bounds bounds)
                       )
                      )
      )
    (setq ewv-wv (get-text-property begin 'ewv-id))
    (save-excursion (goto-char begin)
                    (sit-for 0.001)
                    (setq bounds (ewv-get-img-bounds))
                    (ewv-native-webview-resize ewv-wv bounds)
                    )

    ))

;; (defun test-ewv-links()
;;   (with-current-buffer "test.org"
;;     (with-selected-window (get-buffer-window (current-buffer))
;;       (org-element-map (org-element-parse-buffer) 'link
;;         #'ewv-parse-org-links
;;         )
;;       ))
;;   )
(defun test-ewv-links()
  (interactive)
  (setq-local ewv-show-links t)
  ;; (remove-hook 'window-scroll-functions #'ewv-window-scroll-hook)

  (org-element-map (org-element-parse-buffer) 'link
    #'ewv-parse-org-links
    )
  )

(add-hook 'window-scroll-functions #'ewv-window-scroll-hook)