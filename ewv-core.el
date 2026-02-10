;; -*- lexical-binding: t; -*-


;;; load ewv.dll
;; 先在根目录放一个 debug 版本的 ewv.dll, 方便其他人使用
;; TODO 应该分发 release dll, 特别是后面增加 make-pipe-process 之后还需要针对 ucrt 和 gnu 分发两个不同的 dll
(defvar ewv--current-dir (file-name-directory (or load-file-name (buffer-file-name))))
(defvar ewv--dll-name (expand-file-name "target/debug/ewv.dll" ewv--current-dir))
(unless (file-exists-p ewv--dll-name)
  (setq ewv--dll-name (expand-file-name "ewv.dll" ewv--current-dir)))

(load ewv--dll-name)




;;; core
(defun ewv-get-frame-hwnd (&optional frame)
  "Emasc frame to win32 HWND"
  (string-to-number (frame-parameter (or frame (selected-frame))
                                     'window-id)
                    10))


(defun ewv--eval-string (string)
  "Called from js. Always convert result to string"
  (condition-case err

      (format "%s" (eval (car (read-from-string (format "(progn %s)" string)))))
    (error (ewv--print "ewv--eval-string error: %S" err))
    ))

(cl-defstruct ewv-webview
  "webview2 instance wrapper"
  id
  hwnd
  bounds
  url
  html-string
  )


;;; debug
(defun ewv-debug-open-task-manager()
  (interactive)
  (ewv-native-webview-open-task-manager)
  )
(defun ewv--print(format-exp &rest args)
  (ewv-native-print (apply #'format format-exp args))
  )

;; https://www.reddit.com/r/emacs/comments/8pbbpe/comment/e0an4xy/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(defmacro ewv-with-struct-slots (spec-list type inst &rest body)
  (declare (indent 3) (debug (sexp sexp def-body)))
  ;; Transform the spec-list into a cl-symbol-macrolet spec-list.
  (macroexp-let2 nil inst inst
    `(cl-symbol-macrolet
         ,(mapcar (lambda (entry)
                    (let* ((slot-var  (if (listp entry) (car entry) entry))
			   (slot (if (listp entry) (cadr entry) entry))
			   (idx (cl-struct-slot-offset type slot)))
                      (list slot-var `(aref ,inst ,idx))))
                  spec-list)

       (unless (cl-typep ,inst ',type)
	      (error "%s is not a %s" ',inst ',type))

       ,@body)))


;;; end
(provide 'ewv-core)