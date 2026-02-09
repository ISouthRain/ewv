;; -*- lexical-binding: t; -*-


;;; load ewv.dll
(load  (expand-file-name "target/debug/ewv.dll"
                            (file-name-directory(or load-file-name (buffer-file-name)))))




;;; core
(defun ewv-get-frame-hwnd (&optional frame)
  "Emasc frame to win32 HWND"
  (string-to-number (frame-parameter (or frame (selected-frame))
                                     'window-id)
                    10))


(defun ewv--eval-string (string)
  "Called from js. Always convert result to string"
  (format "%s" (eval (car (read-from-string (format "(progn %s)" string))))))

(cl-defstruct ewv-webview
  "webview2 instance wrapper"
  id
  hwnd
  bounds
  url
  html-string
  )


(defun ewv-navigate-to (webview url)
  (interactive)
  (ewv-native-webview-load (ewv-webview-id webview) url))


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