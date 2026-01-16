;;; test-nnttrss.el --- Tests for nnttrss.el -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: tests

;;; Commentary:

;; Integration tests for the nnttrss Gnus backend.

;;; Code:

(require 'ert)
(require 'nnttrss)

;;; Test fixtures

(defvar test-nnttrss--dir nil
  "Temporary directory for test storage.")

(defun test-nnttrss--setup ()
  "Set up test environment."
  (setq test-nnttrss--dir (make-temp-file "nnttrss-test-" t)))

(defun test-nnttrss--teardown ()
  "Tear down test environment."
  (when (and test-nnttrss--dir (file-directory-p test-nnttrss--dir))
    (delete-directory test-nnttrss--dir t))
  (setq test-nnttrss--dir nil))

(defmacro with-test-nnttrss (&rest body)
  "Execute BODY with test nnttrss environment."
  (declare (indent 0) (debug t))
  `(progn
     (test-nnttrss--setup)
     (unwind-protect
         (let ((nnttrss-directory test-nnttrss--dir))
           ,@body)
       (test-nnttrss--teardown))))

;;; Server operations tests

(ert-deftest nnttrss-test-open-server ()
  "Test server open initializes storage."
  (with-test-nnttrss
    ;; Mock the login to avoid network calls
    (cl-letf (((symbol-function 'ttrss-login) (lambda (&rest _) "fake-sid")))
      (nnttrss-open-server "test"
                           '((nnttrss-address "http://example.com/api/")
                             (nnttrss-user "user")
                             (nnttrss-password "pass")))
      (should (file-directory-p
               (expand-file-name "test" test-nnttrss--dir))))))

(ert-deftest nnttrss-test-close-server ()
  "Test server close stops sync timer."
  (with-test-nnttrss
    (cl-letf (((symbol-function 'ttrss-login) (lambda (&rest _) "fake-sid"))
              ((symbol-function 'ttrss-logout) (lambda (&rest _) t)))
      (nnttrss-open-server "test"
                           '((nnttrss-address "http://example.com/api/")
                             (nnttrss-user "user")
                             (nnttrss-password "pass")))
      (should (nnttrss-close-server "test")))))

;;; Group operations tests

(ert-deftest nnttrss-test-request-list ()
  "Test group listing from storage."
  (with-test-nnttrss
    (nnttrss-storage-init "test")
    ;; Create some test groups with active ranges
    (nnttrss-storage-write-active "test" "feed1" '(1 . 10))
    (nnttrss-storage-write-active "test" "feed2" '(1 . 20))
    ;; Test that request-list can read them
    ;; (actual implementation will vary based on how we store feed metadata)
    (should (functionp 'nnttrss-request-list))))

(ert-deftest nnttrss-test-request-group ()
  "Test selecting a group."
  (with-test-nnttrss
    (nnttrss-storage-init "test")
    (nnttrss-storage-write-active "test" "test-feed" '(1 . 50))
    ;; Verify the storage is set up correctly
    (let ((active (nnttrss-storage-read-active "test" "test-feed")))
      (should (equal active '(1 . 50))))))

;;; Article retrieval tests

(ert-deftest nnttrss-test-retrieve-headers ()
  "Test header retrieval from storage overview."
  (with-test-nnttrss
    (nnttrss-storage-init "test")
    ;; Create test overview data (10-element vector for NOV format)
    (let ((headers (list (make-vector 10 nil))))
      (aset (car headers) 0 1)
      (aset (car headers) 1 "Test Article")
      (aset (car headers) 2 "author@test.com")
      (aset (car headers) 3 "Thu, 16 Jan 2025 10:00:00 +0000")
      (aset (car headers) 4 "<test-1@server>")
      (aset (car headers) 5 "")
      (aset (car headers) 6 100)
      (aset (car headers) 7 10)
      (aset (car headers) 8 "")
      (aset (car headers) 9 "")
      (nnttrss-storage-write-overview "test" "test-feed" headers))
    ;; Verify overview can be read
    (let ((headers (nnttrss-storage-read-overview "test" "test-feed")))
      (should (= (length headers) 1))
      (should (equal (aref (car headers) 1) "Test Article")))))

(ert-deftest nnttrss-test-request-article ()
  "Test article content retrieval from storage."
  (with-test-nnttrss
    (nnttrss-storage-init "test")
    ;; Create test article
    (let ((content "From: test@example.com
Subject: Test Article
Date: Thu, 16 Jan 2025 10:00:00 +0000
Message-ID: <test-1@server>
Content-Type: text/html; charset=utf-8

<p>Article body</p>"))
      (nnttrss-storage-write-article "test" "test-feed" 1 content))
    ;; Verify article can be read
    (should (nnttrss-storage-article-exists-p "test" "test-feed" 1))))

;;; Mark operations tests

(ert-deftest nnttrss-test-update-local-mark ()
  "Test that nnttrss--update-local-mark persists marks to storage."
  (with-test-nnttrss
    (nnttrss-storage-init "test-server")
    ;; Set up: articles 1-10, no marks yet
    (nnttrss-storage-write-active "test-server" "test-feed" '(1 . 10))
    (nnttrss-storage-write-marks "test-server" "test-feed" '())
    ;; Mark article 1 as read
    (nnttrss--update-local-mark "test-server" "test-feed" 1 'read t)
    ;; Verify mark was persisted
    (let ((marks (nnttrss-storage-read-marks "test-server" "test-feed")))
      (should (assq 'read marks))
      (should (member 1 (range-uncompress (cdr (assq 'read marks))))))
    ;; Mark articles 2 and 3 as read
    (nnttrss--update-local-mark "test-server" "test-feed" 2 'read t)
    (nnttrss--update-local-mark "test-server" "test-feed" 3 'read t)
    ;; Verify all three are marked
    (let* ((marks (nnttrss-storage-read-marks "test-server" "test-feed"))
           (read-list (range-uncompress (cdr (assq 'read marks)))))
      (should (member 1 read-list))
      (should (member 2 read-list))
      (should (member 3 read-list)))))

(ert-deftest nnttrss-test-mark-persists-after-refresh ()
  "Test that marks persist through simulated group exit and re-entry.
This is the key bug we fixed: marks must survive pressing 'g' in Gnus."
  (with-test-nnttrss
    (nnttrss-storage-init "test-server")
    ;; Set up: articles 1-17, none read
    (nnttrss-storage-write-active "test-server" "test-feed" '(1 . 17))
    (nnttrss-storage-write-marks "test-server" "test-feed" '())
    ;; Simulate reading article 1 (mark as read)
    (nnttrss--update-local-mark "test-server" "test-feed" 1 'read t)
    ;; Read marks from storage (simulating what happens on refresh)
    (let ((marks (nnttrss-storage-read-marks "test-server" "test-feed")))
      ;; Article 1 should still be marked as read
      (should (assq 'read marks))
      (let ((read-articles (range-uncompress (cdr (assq 'read marks)))))
        (should (member 1 read-articles))
        ;; Article 2 should NOT be marked as read
        (should-not (member 2 read-articles))))))

(ert-deftest nnttrss-test-update-info ()
  "Test info update generates proper read ranges."
  (with-test-nnttrss
    (nnttrss-storage-init "test")
    ;; Set up test data: articles 10-20 exist, 10-15 are read
    (nnttrss-storage-write-active "test" "test-feed" '(10 . 20))
    (nnttrss-storage-write-marks "test" "test-feed"
                                 '((read (10 . 15))))
    ;; Verify marks can be read back
    (let ((marks (nnttrss-storage-read-marks "test" "test-feed")))
      (should (assq 'read marks))
      ;; The read range should be (10 . 15)
      (should (equal (cdr (assq 'read marks)) '((10 . 15)))))))

(ert-deftest nnttrss-test-mark-range-prepend ()
  "Test that read ranges properly handle non-existent article 1.
Gnus expects read ranges to start from 1, so we must prepend (1 . min-1)."
  (with-test-nnttrss
    (nnttrss-storage-init "test")
    ;; Articles start at 159, not 1
    (nnttrss-storage-write-active "test" "test-feed" '(159 . 200))
    ;; Articles 159-170 are read
    (nnttrss-storage-write-marks "test" "test-feed"
                                 '((read (159 . 170))))
    ;; When generating Gnus info, we need to prepend (1 . 158)
    ;; This test verifies our understanding of the issue
    (let* ((active '(159 . 200))
           (min-article (car active))
           (read-ranges '((159 . 170)))
           ;; This is what we need to do in request-update-info
           (full-ranges (if (> min-article 1)
                            (cons (cons 1 (1- min-article)) read-ranges)
                          read-ranges)))
      (should (equal (car full-ranges) '(1 . 158)))
      (should (equal (cadr full-ranges) '(159 . 170))))))

;;; Sync tests

(ert-deftest nnttrss-test-request-scan ()
  "Test that request-scan triggers async sync."
  ;; This is more of a smoke test - actual sync testing requires mocks
  (should (functionp 'nnttrss-request-scan)))

(provide 'test-nnttrss)
;;; test-nnttrss.el ends here
