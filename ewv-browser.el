;; -*- lexical-binding: t; -*-

(defgroup ewv-browser nil
  "Browser based on ewv"
  :link '(info-link :tag "Info Manual" "(ewv)")
  :link '(url-link :tag "Website" "https://github.com/heartnheart/ewv")
  :group 'tools
  :prefix "ewv-browser")

(require 'ewv-core)


(defcustom eb/default-url "https://www.baidu.com" "ewv-browser-open-url 默认打开的 url")

(defcustom eb/browser-executable-folder nil
  "传递给 CreateCoreWebView2EnvironmentWithOptions 的参数。
See https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/webview2-idl?view=webview2-1.0.3800.47#createcorewebview2environmentwithoptions")

(defcustom eb/user-data-folder nil
  "传递给 CreateCoreWebView2EnvironmentWithOptions 的参数。
See https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/webview2-idl?view=webview2-1.0.3800.47#createcorewebview2environmentwithoptions")

(defcustom eb/are-browser-extensions-enabled t
  "传递给 CreateCoreWebView2EnvironmentWithOptions 的参数。
See https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/webview2-idl?view=webview2-1.0.3800.47#createcorewebview2environmentwithoptions")

;; 即使是 64 位 edge.exe 也会安装到 Program Files (x86)
(defcustom eb/edge-path "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe"
  "使用 edge 来编辑 ewv-browser 的 user data folder。注意 edge 的版本需要跟 webview2 runtime 的版本匹配。")

(defvar-local eb//local-id nil "底层 webview 对应的 id")
(defvar-local eb//local-frame nil "底层 webview 所属 parent HWND 对应的 emacs frame")

(defvar eb//all-buffers nil "所有 ewv-browser buffer")
(defvar eb//session-urls nil "记录当前 session ewv-browser buffer")


(defvar eb//all-pinned-urls nil "记录所有被 pin 的 urls")

(defun eb//get-user-data-dir()
  (or (and eb/user-data-folder (expand-file-name "EBWebView"  eb/user-data-folder))
      (expand-file-name "emacs.exe.WebView2\\EBWebView" invocation-directory)))

(defvar eb//pin-file (expand-file-name "ewv-browser-pin.el" (eb//get-user-data-dir)))

(defun eb//read-pin-file()
  (when (file-exists-p eb//pin-file)
    (with-temp-buffer
      (insert-file-contents eb//pin-file)
      (eval-buffer))))

(defun eb//save-pin-file()
  (with-temp-file eb//pin-file
    (insert ";; -*- lexical-binding: t; -*-\n")
    (insert (format "(setq ewv-browser--all-pinned-urls '%S)" eb//all-pinned-urls))))

(eb//read-pin-file)


(defun eb/reopen-all-pinned-urls()
  (interactive)
  (dolist (url eb//all-pinned-urls)
    (eb/open-url url)))

(defvar eb//environment nil "假设 ewv-browser 共享一个 webview environment")


;; TODO emacs-module-rs 目前不支持接受变长参数
(defun ent/environment-new (&rest args)
  (ent/environment-new1 args))

(defun eb//ensure-environment()
  (setq eb//environment (or eb//environment (ent/environment-new
                                             :browser-executable-folder eb/browser-executable-folder
                                             :user-data-folder eb/user-data-folder
                                             :are-browser-extensions-enabled eb/are-browser-extensions-enabled))))

(defun eb//webview-new(frame)
  (eb//ensure-environment)
  (ent/webview-new frame eb//environment))

(defun eb/focus-webpage ()
  (interactive)
  (when eb//local-id
    (ent/webview-focus eb//local-id)
    (message "Focus moved to webpage")))

(defun eb/toggle-visibility ()
  (interactive)
  (ent/webview-set-visible eb//local-id
                           (not (ent/webview-is-visible eb//local-id))))

(defun eb/load-new-url (url)
  (interactive "sEdit Url: ")
  (eb//load eb//local-id url (current-buffer)))

(defun eb/load-new-file (file)
  (interactive "fFile: ")
  (eb//load eb//local-id file (current-buffer)))


(defun eb//enable-global-hooks ()
  (add-hook 'window-configuration-change-hook #'eb//monitor-window-configuration-change)
  (add-hook 'delete-frame-functions #'eb//delete-frame-function))

(defun eb//disable-global-hooks ()
  (remove-hook 'window-configuration-change-hook #'eb//monitor-window-configuration-change)
  (remove-hook 'delete-frame-functions #'eb//delete-frame-function))

(defun eb//register-buffer (buffer)
  (cl-pushnew buffer eb//all-buffers)
  ;; (ec//print "register buffer len = %S" eb//all-buffers)
  (when (= (length eb//all-buffers) 1)
    (eb//enable-global-hooks)))

(defun eb//unregister-buffer ()
  (ent/webview-close eb//local-id)
  (setq eb//all-buffers (cl-remove (current-buffer) eb//all-buffers))
  ;; (ec//print "unregister buffer len = %S" eb//all-buffers)
  (when (<= (length eb//all-buffers) 0)
    (eb//disable-global-hooks)
    (setq eb//environment nil)))

(defun eb//monitor-window-configuration-change1()
  (dolist (wind (window-list))
    ;; NOTE (window-old-buffer) 在 window-state-change-hook 中总是返回跟 new-buf 一样的值, 所以只能用 window-configuration-change-hook
    (with-current-buffer (window-buffer wind)
      ;; (ec//print "monitor wind conf change in buffer %S" (current-buffer))
      (when-let* ((bounds (ec//get-window-edges wind))
                  (id eb//local-id)
                  (frame (window-frame wind)))
        (ent/webview-reparent id frame)
        (setq-local eb//local-frame frame)
        (ent/webview-resize id bounds)
        (ent/webview-set-visible id t))))

  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and eb//local-id  (not (get-buffer-window buf t)))
        (ent/webview-set-visible eb//local-id nil)))))

(defvar eb//monitor-window-configuration-change-timer nil)
(defun eb//monitor-window-configuration-change()
  (when eb//monitor-window-configuration-change-timer
    ;; cancel 一个已经执行过的好像也没错
    (cancel-timer eb//monitor-window-configuration-change-timer))
  (setq eb//monitor-window-configuration-change-timer
        (run-with-idle-timer 0 nil #'eb//monitor-window-configuration-change1)))

;; NOTE window-configuration-change-hook NOT work for window deletion
(defun eb//delete-frame-function(frame)
  (when-let* ((safe-frame (car-safe (remove frame (frame-list)))))
    (dolist (buf eb//all-buffers)
      (with-current-buffer buf
        (when (eq frame eb//local-frame)
          (ent/webview-reparent eb//local-id safe-frame)
          (ent/webview-set-visible eb//local-id nil)
          (setq-local eb//local-frame safe-frame))))))

(defun eb//normalize-url(url)
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
      (concat "https://" url))))

(defun eb//format-title(id title)
  (format "*ewv-buffer-%d-%s*" id title))

(defun eb//load (id url buffer)
  (ent/webview-load id
                    (eb//normalize-url url)
                    (lambda (title url)
                      (with-current-buffer buffer
                        (rename-buffer (eb//format-title id title)))
                      (switch-to-buffer buffer))))

(defun eb//on-new-window-requested(url features)
  (ec//print "eb new-window-requrest url = %S" url)
  (ec//print "                       features = %S" features)
  (cond
   ;; 有些 link 如百度新闻里的会要求访问 about:blank
   ((string= url "about:blank") t)
   ;; 可能是 popup
   ((plist-get features :has-position) nil)
   (t (eb/open-url url) t)
   ))

(defun eb//on-focus-change(focused buffer)
  (when focused
    (let (w (get-buffer-window))
      (and (window-live-p w)
           (select-window w)))))

(defun eb/open-url (url)
  "在当前 frame 新建一个 buffer 打开指定的 url。"
  (interactive (list (read-string (format "URL [%s]: " eb/default-url))))
  (when (and (file-exists-p (expand-file-name "lockfile" (eb//get-user-data-dir)))
             (length= eb//all-buffers 0))
    (user-error "User Data Dir %s is locked" (eb//get-user-data-dir)))
  (when (string-empty-p url)
    (setq url eb/default-url))
  (setq url (eb//normalize-url url))

  (let* ((frame (selected-frame))
         (eb-id (eb//webview-new frame))
         (eb-buffer-name (format "*ewv-browser-%d*" eb-id))
         (eb-buffer (get-buffer-create eb-buffer-name)))

    (ent/webview-set-on-new-window-requested eb-id #'eb//on-new-window-requested)
    ;; update tab-bar
    (ent/webview-set-on-history-changed eb-id #'(lambda () (with-current-buffer eb-buffer (force-mode-line-update) (rename-buffer (eb//format-title eb//local-id (ent/webview-get-document-title eb//local-id))))))
    (ent/webview-set-on-focus eb-id (lambda (focused) (eb//on-focus-change focused eb-buffer)))
    (with-current-buffer eb-buffer
      (eb/mode)
      (setq-local eb//local-id eb-id)
      (setq-local eb//local-frame frame)
      (eb//load eb//local-id url eb-buffer))))

(defun eb/open-file (file)
  (interactive "fFile: ")
  (eb/open-url (eb//normalize-url file)))

(defun eb/kill-all()
  "关闭 ewv-browser 所有 buffer"
  (interactive)
  (dolist (buf eb//all-buffers)
    (kill-buffer buf)))


(defun eb/open-session-in-edge()
  (interactive)
  (when (file-executable-p eb/edge-path)
    (setq eb//session-urls (mapcar (lambda (buf) (ent/webview-get-url (buffer-local-value 'eb//local-id buf)))
                                   eb//all-buffers))
    (eb/kill-all)
    (let ((user-data-dir (eb//get-user-data-dir)))
      ;; webview2 退出之后还需要一段时间才会完整退出
      (while (file-exists-p (expand-file-name "lockfile" user-data-dir))
        (sit-for 0.5))
      (set-process-sentinel
       (apply #'start-process "*Edge*" nil eb/edge-path
                      (format "--user-data-dir=%s" user-data-dir)
                      ;; (car-safe eb//session-urls)
                      eb//session-urls
                      )
       (lambda (proc event)
         ;; edge 退出之后还需要一段时间才会完整退出
         (while (file-exists-p (expand-file-name "lockfile" user-data-dir))
           (sit-for 0.5))
         (dolist (url eb//session-urls)
           (eb/open-url url))
         )
       ))))

(defvar eb/tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    ;; Add specific items to this new map
    tool-bar-map)
  "Custom tool bar map for Lisp mode.")



(defun eb/go-forward()
  (interactive)
  (ent/webview-go-forward eb//local-id))

(defun eb/go-back()
  (interactive)
  (ent/webview-go-back eb//local-id))

(defun eb/open-history ()
  (interactive)
  (eb/open-url "edge://history"))

(defun eb/pin-url ()
  (interactive)
  (cl-pushnew (ent/webview-get-url eb//local-id) eb//all-pinned-urls :test #'string=)
  (force-mode-line-update)
  (eb//save-pin-file))

(defun eb/unpin-url ()
  (interactive)
  (setq eb//all-pinned-urls (cl-remove (ent/webview-get-url eb//local-id) eb//all-pinned-urls :test #'string=))
  (force-mode-line-update)
  (eb//save-pin-file))

(tool-bar-local-item "left-arrow" 'eb/go-back 'eb/go-back eb/tool-bar-map :enable '(ent/webview-can-go-back eb//local-id))

(tool-bar-local-item "right-arrow" 'eb/go-forward 'eb/go-forward eb/tool-bar-map :enable '(ent/webview-can-go-forward eb//local-id))

(define-key-after eb/tool-bar-map [eb/open-session-in-edge-local]
  `(menu-item "Open url" eb//open-default-url
              :image (image :type svg :file ,(expand-file-name "ewv.svg" ec//src-dir) :height ,(tool-bar-height nil t) :scale 0.9)
              :help "Open default url in in new buffer"))

(define-key-after eb/tool-bar-map [eb/open-history]
  `(menu-item "Open history" eb/open-history
              :image (image :type svg :file ,(expand-file-name "history.svg" ec//src-dir) :height ,(tool-bar-height nil t) :scale 0.9)
              :help "Open history in new buffer"))

(define-key-after eb/tool-bar-map [eb/open-session-in-edge]
  `(menu-item "Open in Edge" eb/open-session-in-edge
              :image (image :type svg :file ,(expand-file-name "edge.svg" ec//src-dir) :height ,(tool-bar-height nil t) :scale 0.7)
              :help "Open all webpages in Edge"))

(define-key-after eb/tool-bar-map [eb/pin-url]
  `(menu-item "Open in Edge" eb/pin-url
              :image (image :type svg :file ,(expand-file-name "pin.svg" ec//src-dir) :height ,(tool-bar-height nil t) :scale 0.7)
              :visible (not (cl-find (ent/webview-get-url eb//local-id) eb//all-pinned-urls :test #'string=))
              :help "Pin current url"))

(define-key-after eb/tool-bar-map [eb/unpin-url]
  `(menu-item "Open in Edge" eb/unpin-url
              :image (image :type svg :file ,(expand-file-name "unpin.svg" ec//src-dir) :height ,(tool-bar-height nil t) :scale 0.7)
              :visible (cl-find (ent/webview-get-url eb//local-id) eb//all-pinned-urls :test #'string=)
              :help "Unpin current url"))

(defvar eb/mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'eb/focus-webpage)
    (define-key map (kbd "t") #'eb/toggle-visibility)
    (define-key map (kbd "e") #'eb/load-new-url)
    (define-key map (kbd "f") #'eb/load-new-file)
    map))

(define-derived-mode eb/mode special-mode "EWV-Browser"
  (use-local-map eb/mode-map)
  ;; window-configuration-change-hook 运行的时候 frame 已经删除了，此时再 reparent 会 panic
  ;; TODO 这个 hook 也不保证一定在 frame 删除之前调用
  (add-hook 'kill-buffer-hook #'eb//unregister-buffer nil t)
  (add-hook 'change-major-mode-hook #'eb//unregister-buffer nil t)
  (setq-local tool-bar-map eb/tool-bar-map)
  (eb//register-buffer (current-buffer)))

(defun eb//open-default-url ()
  (interactive)
  (eb/open-url eb/default-url))

(define-key-after global-map [tool-bar eb/open-default-url]
  `(menu-item "Open url" eb//open-default-url
              :image (image :type svg :file ,(expand-file-name "ewv.svg" ec//src-dir) :height ,(tool-bar-height nil t) :scale 0.9)
              :help "Open default url in in buffer"))


(provide 'ewv-browser)

;; Local Variables:
;; read-symbol-shorthands: (("eb/" . "ewv-browser-")
;;                          ("eb//" . "ewv-browser--")
;;                          ("ent/" . "ewv-native-")
;;                          ("ec//" . "ewv-core--")
;;                          )
;; coding: utf-8-unix
;; End:
