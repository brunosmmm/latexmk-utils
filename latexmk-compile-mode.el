;;; latexmk-compile-mode.el --- Compile from anywhere in project using latexmk
;; Author: Bruno Morais <brunosmmm@gmail.com>

;;; Code:
(defun latexmk-utils-locate-latex-document-root(f)
  (if f
    (with-temp-buffer
      (insert-file-contents-literally f)
      (let ((found (re-search-forward "@default_files\\s-*=\\s-*(\\([^,)]+\\))" nil t)))
        (if found
            (replace-regexp-in-string "'" "" (match-string 1))
          nil)))
    nil))

(defun latexmk-utils-root-relative(root-document)
  "Find relative path to root document."
  (file-relative-name (file-name-directory root-document)
                      (file-name-directory (buffer-file-name))))

(defun latexmk-utils-master(root-document &optional ext)
  (if (eq ext t)
      (setq ext TeX-default-extension))
  (if latexmkrc-root-document-file
      (if ext
          (concat (latexmk-utils-root-relative root-document)
                  (file-name-sans-extension (file-name-nondirectory root-document)) "." ext)
        (concat (latexmk-utils-root-relative root-document)
                (file-name-sans-extension (file-name-nondirectory root-document))))
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
              (latexmk-utils-locate-latex-document-root latexmkrc-file))
  (setq-local latexmkrc-root-document (if latexmkrc-root-document-file
                                          (concat latexmkrc-directory latexmkrc-root-document-file)
                                        nil))
  (when latexmkrc-directory
    (setq-local lsp-latex-root-directory latexmkrc-directory))
  (when latexmkrc-root-document-file
    (setq-local TeX-master (latexmk-utils-master latexmkrc-root-document))))

;;;###autoload
(add-hook 'LaTeX-mode-hook #'latexmk-compile-mode)
;;;###autoload
(add-hook 'TeX-mode-hook #'latexmk-compile-mode)

(provide 'latexmk-compile-mode)
