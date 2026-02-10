;; -*- lexical-binding: t; -*-

(require 'url)

(defcustom ewv-extensions-dir
  (expand-file-name "extensions/" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory where EWV extension files are stored.
By default, this is the `extensions/' subdirectory located next to
the ewv.el file."
  :type 'directory
  :group 'ewv)

(defcustom ewv-extensions-alist
  '(("kgnghhfkloifoabeaobjkgagcecbnppg" . "Sufingkeys"))
  "Alist mapping EWV extension IDs to descriptions.
The car of each element is the extension ID string.
The cdr is a human-readable description.

Example Sufingkeys extension html:
https://microsoftedge.microsoft.com/addons/detail/surfingkeys/kgnghhfkloifoabeaobjkgagcecbnppg
the extension id: kgnghhfkloifoabeaobjkgagcecbnppg"
  :type '(alist :key-type string :value-type string)
  :group 'ewv)

(defun ewv--extension--download-url (id)
  "Return Edge Web Store download URL for extension ID."
  (format
   "https://edge.microsoft.com/extensionwebstorebase/v1/crx?x=id%%3D%s%%26installsource%%3Dondemand&response=redirect"
   id))

(defun ewv--extension--installed-p (id)
  "Return non-nil if extension ID directory already exists."
  (file-directory-p
   (expand-file-name id ewv-extensions-dir)))

(defun ewv--extension--install (id)
  "Download and install extension ID into `ewv-extensions-dir'."
  (let* ((target-dir (expand-file-name id ewv-extensions-dir))
         (tmp-crx (make-temp-file (format "ewv-%s-" id) nil ".crx"))
         (url (ewv--extension--download-url id)))
    (message "[ewv] Installing extension %s…" id)
    (condition-case err
        (progn
          ;; 确保 extensions 目录存在
          (make-directory ewv-extensions-dir t)
          ;; 下载 crx
          (url-copy-file url tmp-crx t)
          ;; 创建目标目录
          (make-directory target-dir t)
          ;; 解压
          (let ((exit-code
                 (call-process
                  "unzip" nil "*ewv-unzip*" t
                  "-oq" tmp-crx "-d" target-dir)))
            ;; TODO: Exit code is 1320
            ;; (unless (eq exit-code 0)
            ;;   (error "unzip failed with exit code %s" exit-code))
            )

          (message "[ewv] Extension %s installed successfully" id))
      (error
       (warn "[ewv] Failed to install extension %s: %s"
             id (error-message-string err))))
    (ignore-errors (delete-file tmp-crx))))

(defun ewv--extension-load (ew-process-id)
  "Load `ewv-extensions-alist' and auto-install missing extensions."
  (dolist (entry ewv-extensions-alist)
    (let ((id (car entry))
          (desc (cdr entry)))
      (unless (ewv--extension--installed-p id)
        (message "[ewv] Extension missing: %s (%s)" id desc)
        (ewv--extension--install id))
      (ewv-native-webview-add-extension ew-process-id (expand-file-name (concat ewv-extensions-dir id))))))

(provide 'ewv-extension)
