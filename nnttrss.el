;;; nnttrss.el --- Gnus backend for Tiny Tiny RSS  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Pedro Silva
;; Copyright (C) 2023-2025 Slava Barinov

;; Author: Pedro Silva <psilva+git@pedrosilva.pt>
;;         Slava Barinov <rayslava@rayslava.com>
;; Created: 01 April 2013
;; Updated: 16 January 2025
;; Version: 0.1.0
;; Keywords: news, rss, gnus
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/rayslava/ttrss.el

;; This file is not part of GNU Emacs.

;; nnttrss is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; nnttrss is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnttrss.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a Gnus backend for reading feeds from a
;; Tiny Tiny RSS server.  It uses file-per-article storage similar
;; to Gnus Agent and performs background async synchronization.
;;
;; Usage:
;;
;;   (setq gnus-select-method
;;         '(nnttrss "default"
;;           (nnttrss-address "https://example.com/tt-rss/api/")
;;           (nnttrss-user "username")
;;           (nnttrss-password "password")))
;;
;; Or add as a secondary method:
;;
;;   (add-to-list 'gnus-secondary-select-methods
;;                '(nnttrss "myserver"
;;                  (nnttrss-address "https://example.com/tt-rss/api/")
;;                  (nnttrss-user "username")
;;                  (nnttrss-password "password")))

;;; Code:

(require 'cl-lib)
(require 'range)
(require 'ttrss)
(require 'nnttrss-storage)
(require 'nnttrss-sync)
(require 'gnus)
(require 'nnoo)
(require 'nnheader)

;; Declare dynamic variables from gnus
(defvar gnus-read-mark)
(defvar gnus-del-mark)
(defvar gnus-catchup-mark)
(defvar gnus-unread-mark)
(defvar gnus-ticked-mark)

;;; Backend declaration

(nnoo-declare nnttrss)
(nnoo-define-basics nnttrss)
(gnus-declare-backend "nnttrss" 'news 'address 'agent)

;;; Server variables

(defvoo nnttrss-address nil
  "Address of the TT-RSS API endpoint.")

(defvoo nnttrss-user nil
  "Username for TT-RSS authentication.")

(defvoo nnttrss-password nil
  "Password for TT-RSS authentication.")

(defvoo nnttrss-status-string ""
  "Last status message.")

;;; Internal state (per-server)

(defvar nnttrss--sessions (make-hash-table :test 'equal)
  "Hash table of server -> session info plist.
Each plist contains :sid, :address, :user.")

;;; Helper functions

(defun nnttrss--server-key (server)
  "Return normalized key for SERVER."
  (or server "default"))

(defun nnttrss--get-session (server)
  "Get session info for SERVER."
  (gethash (nnttrss--server-key server) nnttrss--sessions))

(defun nnttrss--set-session (server info)
  "Set session INFO for SERVER."
  (puthash (nnttrss--server-key server) info nnttrss--sessions))

(defun nnttrss--clear-session (server)
  "Clear session for SERVER."
  (remhash (nnttrss--server-key server) nnttrss--sessions))

(defun nnttrss-decode-gnus-group (group)
  "Decode GROUP name from Gnus internal encoding."
  (decode-coding-string group 'utf-8))

(defun nnttrss-encode-gnus-group (group)
  "Encode GROUP name for Gnus internal use."
  (encode-coding-string group 'utf-8))

;;; Server operations

(deffoo nnttrss-open-server (server &optional defs)
  "Open connection to TT-RSS SERVER with DEFS configuration."
  (unless (nnttrss-server-opened server)
    ;; Extract configuration from defs
    (dolist (def '(nnttrss-address nnttrss-user nnttrss-password))
      (unless (assq def defs)
        (setq defs (append defs (list (list def server)))))
      (setf (symbol-value def) (cadr (assq def defs))))
    ;; Login to server
    (condition-case err
        (let ((sid (ttrss-login nnttrss-address nnttrss-user nnttrss-password)))
          (nnttrss--set-session server
                                (list :sid sid
                                      :address nnttrss-address
                                      :user nnttrss-user))
          ;; Initialize storage
          (nnttrss-storage-init (nnttrss--server-key server))
          ;; Start sync timer and trigger initial sync
          (nnttrss-sync-start-timer (nnttrss--server-key server)
                                    nnttrss-address sid)
          (when nnttrss-sync-on-open
            (nnttrss-sync (nnttrss--server-key server) nnttrss-address sid)))
      (error
       (setq nnttrss-status-string (error-message-string err))
       nil)))
  (nnoo-change-server 'nnttrss server defs))

(deffoo nnttrss-close-server (&optional server defs)
  "Close connection to SERVER."
  (ignore defs)
  (when-let ((session (nnttrss--get-session server)))
    (let ((sid (plist-get session :sid))
          (address (plist-get session :address)))
      ;; Stop sync timer
      (nnttrss-sync-stop-timer (nnttrss--server-key server))
      ;; Logout
      (condition-case nil
          (ttrss-logout address sid)
        (error nil))
      (nnttrss--clear-session server)))
  t)

(deffoo nnttrss-request-close ()
  "Close all nnttrss connections."
  t)

(deffoo nnttrss-server-opened (&optional server)
  "Check if SERVER is opened (has valid session)."
  (and (nnttrss--get-session server) t))

(deffoo nnttrss-status-message (&optional server)
  "Return status message for SERVER."
  (ignore server)
  nnttrss-status-string)

;;; Group operations

(deffoo nnttrss-request-list (&optional server)
  "List all feeds from storage for SERVER."
  (let* ((srv (nnttrss--server-key server))
         (feeds (nnttrss-storage-read-feeds srv)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (feed-entry feeds)
        (let* ((group (car feed-entry))
               (active (nnttrss-storage-read-active srv group)))
          (if active
              (insert (format "\"%s\" %d %d y\n"
                              group (cdr active) (car active)))
            (insert (format "\"%s\" 0 1 y\n" group))))))
    t))

(deffoo nnttrss-request-group (group &optional server fast info)
  "Select GROUP on SERVER.
If FAST is non-nil, just confirm group exists.
INFO is the group info structure."
  (ignore info)
  (setq group (nnttrss-decode-gnus-group group))
  (let* ((srv (nnttrss--server-key server))
         (active (nnttrss-storage-read-active srv group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (if fast
          t
        (if active
            (let* ((min-art (car active))
                   (max-art (cdr active))
                   (count (1+ (- max-art min-art))))
              (insert (format "211 %d %d %d \"%s\"\n"
                              count min-art max-art
                              (nnttrss-encode-gnus-group group))))
          (insert (format "211 0 1 0 \"%s\"\n"
                          (nnttrss-encode-gnus-group group))))
        t))))

(deffoo nnttrss-close-group (group &optional server)
  "Close GROUP on SERVER."
  (ignore group server)
  t)

(deffoo nnttrss-request-scan (&optional group server)
  "Trigger async sync for GROUP on SERVER."
  (ignore group)
  (when-let ((session (nnttrss--get-session server)))
    (let ((address (plist-get session :address))
          (sid (plist-get session :sid)))
      (nnttrss-sync (nnttrss--server-key server) address sid)))
  t)

;;; Article retrieval

(deffoo nnttrss-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve headers for ARTICLES in GROUP on SERVER.
FETCH-OLD is ignored (we read from cache only)."
  (ignore fetch-old)
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let* ((srv (nnttrss--server-key server))
         (headers (nnttrss-storage-read-overview srv group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (article articles)
        (when-let ((header (cl-find article headers
                                    :key (lambda (h) (aref h 0)))))
          (insert (format "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%s\t%s\n"
                          (aref header 0)   ; num
                          (aref header 1)   ; subject
                          (aref header 2)   ; from
                          (aref header 3)   ; date
                          (aref header 4)   ; msgid
                          (aref header 5)   ; refs
                          (aref header 6)   ; chars
                          (aref header 7)   ; lines
                          (aref header 8)   ; xref
                          (aref header 9))) ; extra
          ))))
  'nov)

(deffoo nnttrss-request-article (article &optional group server to-buffer)
  "Retrieve ARTICLE from GROUP on SERVER into TO-BUFFER."
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let* ((srv (nnttrss--server-key server))
         (destination (or to-buffer nntp-server-buffer))
         (num (if (numberp article) article
                ;; Handle message-id lookup
                (when (string-match "<\\([0-9]+\\)@" article)
                  (string-to-number (match-string 1 article))))))
    (when num
      (with-current-buffer destination
        (erase-buffer)
        (if (nnttrss-storage-read-article srv group num)
            (cons group num)
          nil)))))

;;; Mark operations

(deffoo nnttrss-request-update-mark (group article mark)
  "Update MARK for ARTICLE in GROUP.
Queues mark changes for background sync."
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let* ((srv (nnttrss--server-key (nnoo-current-server 'nnttrss)))
         (sql-id (nnttrss-storage-num-to-id srv group article)))
    (when sql-id
      (cond
       ;; Mark as read
       ((or (= mark gnus-read-mark)
            (= mark gnus-del-mark)
            (= mark gnus-catchup-mark))
        (nnttrss-sync-queue-mark srv sql-id 2 0)
        (nnttrss--update-local-mark srv group article 'read t))
       ;; Mark as unread
       ((= mark gnus-unread-mark)
        (nnttrss-sync-queue-mark srv sql-id 2 1)
        (nnttrss-sync-queue-mark srv sql-id 0 0)  ; Also unstar
        (nnttrss--update-local-mark srv group article 'read nil)
        (nnttrss--update-local-mark srv group article 'tick nil))
       ;; Mark as ticked (starred)
       ((= mark gnus-ticked-mark)
        (nnttrss-sync-queue-mark srv sql-id 0 1)
        (nnttrss--update-local-mark srv group article 'tick t)))))
  mark)

(deffoo nnttrss-request-set-mark (group actions &optional server)
  "Handle mark ACTIONS for GROUP on SERVER."
  (ignore server)
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let ((srv (nnttrss--server-key server)))
    (dolist (action actions)
      (cl-destructuring-bind (range action-type marks) action
        (let ((unread-mode (if (eq action-type 'add) 0 1))
              (starred-mode (if (eq action-type 'add) 1 0)))
          ;; When ticking, also mark as read
          (when (and (memq 'tick marks) (eq action-type 'add))
            (push 'read marks))
          (dolist (article (range-uncompress range))
            (when-let ((sql-id (nnttrss-storage-num-to-id srv group article)))
              (when (memq 'read marks)
                (nnttrss-sync-queue-mark srv sql-id 2 unread-mode)
                (nnttrss--update-local-mark srv group article 'read
                                            (= unread-mode 0)))
              (when (memq 'tick marks)
                (nnttrss-sync-queue-mark srv sql-id 0 starred-mode)
                (nnttrss--update-local-mark srv group article 'tick
                                            (= starred-mode 1)))))))))
  nil)

(deffoo nnttrss-request-update-info (group info &optional server)
  "Update INFO for GROUP with read and marked article ranges.
Reads from local storage only."
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let* ((srv (nnttrss--server-key (or server (nnoo-current-server 'nnttrss))))
         (active (gnus-active (gnus-info-group info)))
         (min-article (car active))
         (marks (nnttrss-storage-read-marks srv group))
         (read-ranges (cdr (assq 'read marks)))
         (tick-ranges (cdr (assq 'tick marks))))
    ;; Gnus expects read ranges to start from 1, marking non-existent
    ;; articles as read.  Prepend (1 . min-1) when first article > 1.
    (when (and min-article (> min-article 1))
      (setq read-ranges (range-concat `((1 . ,(1- min-article)))
                                      read-ranges)))
    (setf (gnus-info-read info) read-ranges)
    (setf (gnus-info-marks info)
          (when tick-ranges
            `((tick . ,tick-ranges)))))
  t)

(defun nnttrss--update-local-mark (server group article mark-type value)
  "Update local mark storage for ARTICLE in GROUP on SERVER.
MARK-TYPE is `read' or `tick'.  VALUE is t or nil."
  (let* ((marks (nnttrss-storage-read-marks server group))
         (current-range (cdr (assq mark-type marks)))
         (new-range (if value
                        (range-add-list current-range (list article))
                      (range-remove current-range (list article))))
         (new-marks (cl-remove mark-type marks :key #'car)))
    (when new-range
      (push (cons mark-type new-range) new-marks))
    (nnttrss-storage-write-marks server group new-marks)))

;;; Interactive commands

;;;###autoload
(defun nnttrss-sync-now ()
  "Manually trigger sync for the current nnttrss server.
Use this to refresh feeds without waiting for the timer."
  (interactive)
  (let ((server (when (boundp 'gnus-current-select-method)
                  (cadr gnus-current-select-method))))
    (if-let ((session (nnttrss--get-session server)))
        (let ((address (plist-get session :address))
              (sid (plist-get session :sid)))
          (nnttrss-sync (nnttrss--server-key server) address sid))
      (message "nnttrss: No active session"))))

(provide 'nnttrss)
;;; nnttrss.el ends here
