
(defun locate-latex-document-root(f)
  (interactive)
  (if f
    (with-temp-buffer
      (insert-file-contents-literally f)
      (setq-local found (re-search-forward "@default_files\\s-*=\\s-*(\\([^,)]+\\))" nil t))
      (setq-local latexmkrc-root-document
                  (if found
                      (replace-regexp-in-string "'" "" (match-string 1))
                    nil))
      (message latexmkrc-root-document)
      )
    (setq-local latexmkrc-root-document nil)
    )
  (message latexmkrc-root-document)
  latexmkrc-root-document
  )

(defun lsp-latex-root-build (&optional sync)
  "Build current tex file with latexmk, through texlab.
Build synchronously if SYNC is non-nil."
  (interactive "P")
  (if sync
      (lsp-latex--message-result-build
       (lsp-request
        "textDocument/build"
        (list :textDocument (list :uri (file-relative-name
                                        latexmkrc-root-document
                                        (lsp--calculate-root lsp--session latexmkrc-root-document))))))
    (lsp-request-async
     "textDocument/build"
     (list :textDocument (list :uri (file-relative-name
                                     latexmkrc-root-document
                                     (lsp--calculate-root lsp--session latexmkrc-root-document))))
     #'lsp-latex--message-result-build)))

;;;###autoload
(define-minor-mode latexmk-utils
  "Use latexmk from anywhere"
  :lighter nil
  :keymap nil
  (setq-local latexmkrc-directory (locate-dominating-file buffer-file-name ".latexmkrc"))
  (setq-local latexmkrc-file
              (if latexmkrc-directory
                  (concat latexmkrc-directory ".latexmkrc")
                nil))
  (setq-local latexmkrc-root-document
              (locate-latex-document-root latexmkrc-file))
  (when latexmkrc-directory
    (setq-local lsp-latex-root-directory latexmkrc-directory))
  )

;;;###autoload
(add-hook 'latex-mode-hook 'latexmk-utils)

(provide latexmk-utils)
