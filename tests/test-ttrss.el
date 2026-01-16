;;; test-ttrss.el --- Tests for ttrss.el -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: tests

;;; Commentary:

;; Unit tests for the TT-RSS API client.

;;; Code:

(require 'ert)
(require 'ttrss)

;;; Helper to extract content from response

(defun ttrss--extract-content (response)
  "Extract content from RESPONSE."
  (cdr (assq 'content response)))

(defun ttrss--build-request (&rest args)
  "Build request from ARGS."
  (let ((result '()))
    (while args
      (let ((key (pop args))
            (val (pop args)))
        (when key
          (push (cons (intern (substring (symbol-name key) 1)) val) result))))
    (nreverse result)))

;;; JSON parsing tests

(ert-deftest ttrss-test-parse-json-success ()
  "Test successful JSON response parsing."
  (let ((response '((status . 0)
                    (content . ((id . 123)
                                (name . "test"))))))
    (should (equal (ttrss--extract-content response)
                   '((id . 123) (name . "test"))))))

(ert-deftest ttrss-test-parse-json-error ()
  "Test error JSON response parsing."
  (let ((response '((status . 1)
                    (content . ((error . "NOT_LOGGED_IN"))))))
    (should (equal (cdr (assq 'error (ttrss--extract-content response)))
                   "NOT_LOGGED_IN"))))

;;; Request building tests

(ert-deftest ttrss-test-build-request-basic ()
  "Test basic request building."
  (let ((req (ttrss--build-request :op "login" :user "test" :password "pass")))
    (should (equal (cdr (assq 'op req)) "login"))
    (should (equal (cdr (assq 'user req)) "test"))
    (should (equal (cdr (assq 'password req)) "pass"))))

(ert-deftest ttrss-test-build-request-with-sid ()
  "Test request building with session ID."
  (let ((req (ttrss--build-request :op "getFeeds" :sid "abc123")))
    (should (equal (cdr (assq 'op req)) "getFeeds"))
    (should (equal (cdr (assq 'sid req)) "abc123"))))

;;; Helper function tests

(ert-deftest ttrss-test-functions-exist ()
  "Test that expected functions are defined."
  (should (functionp 'ttrss-login))
  (should (functionp 'ttrss-logout))
  (should (functionp 'ttrss-logged-in-p))
  (should (functionp 'ttrss-get-feeds))
  (should (functionp 'ttrss-get-headlines))
  (should (functionp 'ttrss-get-article))
  (should (functionp 'ttrss-get-feeds-async))
  (should (functionp 'ttrss-get-headlines-async))
  (should (functionp 'ttrss-get-article-async)))

(provide 'test-ttrss)
;;; test-ttrss.el ends here
