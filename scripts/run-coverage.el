;;; run-coverage.el --- Run tests with coverage -*- lexical-binding: t -*-
;;; Commentary:
;; Run with: eask eval "(load \"scripts/run-coverage.el\")"
;;; Code:

(require 'undercover)
(setq undercover-force-coverage t)

;; Configure undercover - use lcov for CI, text for local
(if (getenv "CI")
    (undercover "nnttrss.el" "nnttrss-storage.el" "nnttrss-sync.el"
                (:report-format 'lcov)
                (:report-file "coverage.lcov")
                (:send-report nil))
  (undercover "nnttrss.el" "nnttrss-storage.el" "nnttrss-sync.el"
              (:report-format 'text)
              (:send-report nil)))

(require 'ert)
(add-to-list 'load-path (expand-file-name "tests"))
(load "test-storage")
(load "test-ttrss")
(load "test-nnttrss")
(ert-run-tests-batch-and-exit)
;;; run-coverage.el ends here
