
(defun locate-latex-document-root(f)
  (if f
    (with-temp-buffer
      (insert-file-contents-literally f)
      (setq-local found (re-search-forward "@default_files\\s-*=\\s-*(\\([^,)]+\\))" nil t))
      (if found
          (replace-regexp-in-string "'" "" (match-string 1))
        nil))
    nil
    )
  )

(defun latexmk-utils-get-root-uri()
  "Get root document URI."
  (concat "file://" (file-truename latexmkrc-root-document))
  )

(defun lsp-latex-root-build (&optional sync)
  "Build current tex file with latexmk, through texlab.
Build synchronously if SYNC is non-nil."
  (interactive "P")
  (if sync
      (lsp-latex--message-result-build
       (lsp-request
        "textDocument/build"
        (list :textDocument (list :uri (latexmk-utils-get-root-uri)))))
    (lsp-request-async
     "textDocument/build"
     (list :textDocument (list :uri (latexmk-utils-get-root-uri)))
     #'lsp-latex--message-result-build)))

(defun latexmk-utils-root-relative()
  "Find relative path to root document."
  (file-relative-name (file-name-directory latexmkrc-root-document)
                      (file-name-directory (buffer-file-name)))
  )

(defun latexmk-utils-master(&optional ext)
  (if (eq ext t)
      (setq ext TeX-default-extension))
  (if latexmkrc-root-document-file
      (if ext
          (concat (latexmk-utils-root-relative) (file-name-sans-extension latexmkrc-root-document-file) "." ext)
        (concat (latexmk-utils-root-relative) (file-name-sans-extension latexmkrc-root-document-file)))
    nil))


;;;###autoload
(define-minor-mode latexmk-compile-mode
  "Use latexmk from anywhere"
  :lighter nil
  :keymap nil
  (setq-local latexmkrc-directory (locate-dominating-file buffer-file-name ".latexmkrc"))
  (setq-local latexmkrc-file
              (if latexmkrc-directory
                  (concat latexmkrc-directory ".latexmkrc")
                nil))
  (setq-local latexmkrc-root-document-file
              (locate-latex-document-root latexmkrc-file))
  (setq-local latexmkrc-root-document (if latexmkrc-root-document-file
                                          (concat latexmkrc-directory latexmkrc-root-document-file)
                                        nil))
  (when latexmkrc-directory
    (setq-local lsp-latex-root-directory latexmkrc-directory))
  (when latexmkrc-root-document-file
    (setq-local TeX-master (latexmk-utils-master)))
  )

;;;###autoload
(add-hook 'latex-mode-hook 'latexmk-compile-mode)

(provide 'latexmk-compile-mode)