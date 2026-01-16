;;; test-storage.el --- Tests for nnttrss-storage.el -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: tests

;;; Commentary:

;; Unit tests for the nnttrss storage layer.

;;; Code:

(require 'ert)
(require 'nnttrss-storage)

;;; Test fixtures

(defvar test-storage--dir nil
  "Temporary directory for test storage.")

(defun test-storage--setup ()
  "Set up test environment."
  (setq test-storage--dir (make-temp-file "nnttrss-test-" t)))

(defun test-storage--teardown ()
  "Tear down test environment."
  (when (and test-storage--dir (file-directory-p test-storage--dir))
    (delete-directory test-storage--dir t))
  (setq test-storage--dir nil))

(defmacro with-test-storage (&rest body)
  "Execute BODY with temporary storage directory."
  (declare (indent 0) (debug t))
  `(progn
     (test-storage--setup)
     (unwind-protect
         (let ((nnttrss-directory test-storage--dir))
           ,@body)
       (test-storage--teardown))))

;;; Directory management tests

(ert-deftest storage-test-init ()
  "Test storage initialization creates directory structure."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (should (file-directory-p
             (expand-file-name "test-server" test-storage--dir)))))

(ert-deftest storage-test-group-dir ()
  "Test group directory path generation."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((dir (nnttrss-storage-group-dir "test-server" "test-feed")))
      (should (string-match-p "test-server/test-feed$" dir)))))

(ert-deftest storage-test-sanitize-name ()
  "Test name sanitization for filesystem."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    ;; Names with special chars should be sanitized
    (let ((dir (nnttrss-storage-group-dir "test-server" "feed/with:special*chars")))
      (should-not (string-match-p "[/:*]" (file-name-nondirectory dir))))))

;;; Overview tests

(ert-deftest storage-test-overview-roundtrip ()
  "Test overview write and read."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((headers (list (make-vector 10 nil))))
      ;; Set header fields: [num subject from date msgid refs chars lines xref extra]
      (aset (car headers) 0 1)
      (aset (car headers) 1 "Test Subject")
      (aset (car headers) 2 "author@example.com")
      (aset (car headers) 3 "Thu, 16 Jan 2025 10:00:00 +0000")
      (aset (car headers) 4 "<test-1@server>")
      (aset (car headers) 5 "")
      (aset (car headers) 6 100)
      (aset (car headers) 7 10)
      (aset (car headers) 8 "")
      (aset (car headers) 9 "")
      (nnttrss-storage-write-overview "test-server" "test-feed" headers)
      (let ((read-headers (nnttrss-storage-read-overview "test-server" "test-feed")))
        (should (= (length read-headers) 1))
        (should (equal (aref (car read-headers) 1) "Test Subject"))
        (should (= (aref (car read-headers) 0) 1))))))

(ert-deftest storage-test-overview-append ()
  "Test appending to overview."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((h1 (make-vector 10 nil))
          (h2 (make-vector 10 nil)))
      (aset h1 0 1)
      (aset h1 1 "First")
      (aset h2 0 2)
      (aset h2 1 "Second")
      (nnttrss-storage-write-overview "test-server" "test-feed" (list h1))
      (nnttrss-storage-append-overview "test-server" "test-feed" (list h2))
      (let ((headers (nnttrss-storage-read-overview "test-server" "test-feed")))
        (should (= (length headers) 2))))))

;;; Article tests

(ert-deftest storage-test-article-roundtrip ()
  "Test article write and read."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((content "From: test@example.com\nSubject: Test\n\nBody here"))
      (nnttrss-storage-write-article "test-server" "test-feed" 1 content)
      (should (nnttrss-storage-article-exists-p "test-server" "test-feed" 1))
      (with-temp-buffer
        (nnttrss-storage-read-article "test-server" "test-feed" 1 (current-buffer))
        (should (string-match-p "Subject: Test" (buffer-string)))))))

(ert-deftest storage-test-article-delete ()
  "Test article deletion."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (nnttrss-storage-write-article "test-server" "test-feed" 1 "content")
    (should (nnttrss-storage-article-exists-p "test-server" "test-feed" 1))
    (nnttrss-storage-delete-article "test-server" "test-feed" 1)
    (should-not (nnttrss-storage-article-exists-p "test-server" "test-feed" 1))))

(ert-deftest storage-test-article-nonexistent ()
  "Test reading nonexistent article returns nil."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (should-not (nnttrss-storage-article-exists-p "test-server" "test-feed" 999))))

;;; Active range tests

(ert-deftest storage-test-active-roundtrip ()
  "Test active range write and read."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (nnttrss-storage-write-active "test-server" "test-feed" '(1 . 100))
    (let ((active (nnttrss-storage-read-active "test-server" "test-feed")))
      (should (equal active '(1 . 100))))))

(ert-deftest storage-test-active-nil-for-new-group ()
  "Test active is nil for group without articles."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (should-not (nnttrss-storage-read-active "test-server" "nonexistent"))))

;;; Marks tests

(ert-deftest storage-test-marks-roundtrip ()
  "Test marks write and read."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((marks '((read (1 . 50) 55 60)
                   (tick 10 20 30))))
      (nnttrss-storage-write-marks "test-server" "test-feed" marks)
      (let ((read-marks (nnttrss-storage-read-marks "test-server" "test-feed")))
        (should (equal read-marks marks))))))

(ert-deftest storage-test-marks-empty ()
  "Test empty marks."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (nnttrss-storage-write-marks "test-server" "test-feed" '())
    (let ((marks (nnttrss-storage-read-marks "test-server" "test-feed")))
      (should (null marks)))))

;;; ID mapping tests

(ert-deftest storage-test-id-map-roundtrip ()
  "Test ID mapping write and read."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((map (make-hash-table :test 'equal)))
      (puthash 139328 1 map)
      (puthash 139329 2 map)
      (puthash 139330 3 map)
      (nnttrss-storage-write-id-map "test-server" "test-feed" map)
      (let ((read-map (nnttrss-storage-read-id-map "test-server" "test-feed")))
        (should (= (gethash 139328 read-map) 1))
        (should (= (gethash 139329 read-map) 2))
        (should (= (gethash 139330 read-map) 3))))))

(ert-deftest storage-test-id-conversion ()
  "Test ID to number and number to ID conversion."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((map (make-hash-table :test 'equal)))
      (puthash 139328 1 map)
      (puthash 139329 2 map)
      (nnttrss-storage-write-id-map "test-server" "test-feed" map)
      (should (= (nnttrss-storage-id-to-num "test-server" "test-feed" 139328) 1))
      (should (= (nnttrss-storage-num-to-id "test-server" "test-feed" 1) 139328)))))

(ert-deftest storage-test-register-id ()
  "Test registering new ID mapping."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (nnttrss-storage-register-id "test-server" "test-feed" 12345 1)
    (should (= (nnttrss-storage-id-to-num "test-server" "test-feed" 12345) 1))))

(ert-deftest storage-test-next-article-num ()
  "Test getting next article number."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    ;; No articles yet
    (should (= (nnttrss-storage-next-article-num "test-server" "test-feed") 1))
    ;; After adding active range
    (nnttrss-storage-write-active "test-server" "test-feed" '(1 . 10))
    (should (= (nnttrss-storage-next-article-num "test-server" "test-feed") 11))))

;;; Feeds tests

(ert-deftest storage-test-feeds-roundtrip ()
  "Test feeds metadata write and read."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (let ((feeds '(("Feed One" :id 1 :title "Feed One")
                   ("Feed Two" :id 2 :title "Feed Two"))))
      (nnttrss-storage-write-feeds "test-server" feeds)
      (let ((read-feeds (nnttrss-storage-read-feeds "test-server")))
        (should (= (length read-feeds) 2))
        (should (equal (car (car read-feeds)) "Feed One"))))))

;;; Cleanup tests

(ert-deftest storage-test-clear-group ()
  "Test clearing a single group."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (nnttrss-storage-write-article "test-server" "test-feed" 1 "content")
    (nnttrss-storage-clear "test-server" "test-feed")
    (should-not (file-directory-p
                 (nnttrss-storage-group-dir "test-server" "test-feed")))))

(ert-deftest storage-test-clear-server ()
  "Test clearing entire server storage."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (nnttrss-storage-write-article "test-server" "feed1" 1 "content")
    (nnttrss-storage-write-article "test-server" "feed2" 1 "content")
    (nnttrss-storage-clear "test-server")
    (should-not (file-directory-p
                 (expand-file-name "test-server" test-storage--dir)))))

(ert-deftest storage-test-list-groups ()
  "Test listing groups."
  (with-test-storage
    (nnttrss-storage-init "test-server")
    (nnttrss-storage-write-article "test-server" "feed1" 1 "content")
    (nnttrss-storage-write-article "test-server" "feed2" 1 "content")
    (let ((groups (nnttrss-storage-list-groups "test-server")))
      (should (= (length groups) 2))
      (should (member "feed1" groups))
      (should (member "feed2" groups)))))

(provide 'test-storage)
;;; test-storage.el ends here
