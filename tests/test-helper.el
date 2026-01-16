;;; test-helper.el --- Test helper for nnttrss tests -*- lexical-binding: t -*-

;;; Commentary:

;; Test setup for nnttrss tests.
;; For coverage, use: eask eval "(load \"scripts/run-coverage.el\")"

;;; Code:

;; Add parent directory to load path
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(provide 'test-helper)
;;; test-helper.el ends here
