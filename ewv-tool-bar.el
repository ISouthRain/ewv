(defvar eb//address-bar-id nil)

(defvar eb//address-bar-file
  (expand-file-name "address-bar.html" (file-name-directory (or load-file-name (buffer-file-name)))))

(unless eb//address-bar-id
  (setq eb//address-bar-id (eb//webview-new (selected-frame)))
  (ent/webview-resize eb//address-bar-id (list 0 0 (frame-inner-width) (tool-bar-height nil t)) )

  (ent/webview-load-sync eb//address-bar-id eb//address-bar-file)
  (ent/webview-set-visible eb//address-bar-id t)
  )

(defun eb//monitor-address-bar()
  (ent/webview-set-visible eb//address-bar-id (bound-and-true-p eb//local-id))
  (ent/webview-resize eb//address-bar-id (list 0 0 (frame-inner-width) (tool-bar-height nil t)))
  )
(add-hook 'window-state-change-hook #'eb//monitor-address-bar)