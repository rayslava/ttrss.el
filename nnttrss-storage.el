;;; nnttrss-storage.el --- Storage layer for nnttrss -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: news, rss

;;; Commentary:

;; Gnus Agent-style file-per-article storage for nnttrss.
;;
;; Directory structure:
;;   ~/.gnus/ttrss/
;;   ├── server-name/
;;   │   ├── feed-name/
;;   │   │   ├── .overview    ; NOV format headers
;;   │   │   ├── .active      ; (min . max) article range
;;   │   │   ├── .marks       ; ((read ...) (tick ...) ...)
;;   │   │   ├── .idmap       ; hash-table sql-id -> article-num
;;   │   │   ├── 1            ; RFC 822 article
;;   │   │   ├── 2
;;   │   │   └── ...
;;   │   └── ...
;;   └── ...

;;; Code:

(require 'cl-lib)

;;; Custom variables

(defgroup nnttrss-storage nil
  "Storage settings for nnttrss."
  :group 'nnttrss)

(defcustom nnttrss-directory
  (expand-file-name "ttrss" (or (bound-and-true-p gnus-directory)
                                 (expand-file-name "News" "~")))
  "Directory for nnttrss storage."
  :type 'directory
  :group 'nnttrss-storage)

;;; Internal variables

(defvar nnttrss-storage--id-map-cache (make-hash-table :test 'equal)
  "Cache of ID maps: (server . group) -> hash-table.")

;;; Directory management

(defun nnttrss-storage-server-dir (server)
  "Return the storage directory for SERVER."
  (expand-file-name (nnttrss-storage--sanitize-name server)
                    nnttrss-directory))

(defun nnttrss-storage-group-dir (server group)
  "Return the storage directory for GROUP on SERVER."
  (expand-file-name (nnttrss-storage--sanitize-name group)
                    (nnttrss-storage-server-dir server)))

(defun nnttrss-storage--sanitize-name (name)
  "Sanitize NAME for use as directory/file name."
  (replace-regexp-in-string "[/\\:*?\"<>|]" "_" name))

(defun nnttrss-storage-init (server)
  "Initialize storage directory for SERVER.
Creates the directory structure if it doesn't exist."
  (let ((dir (nnttrss-storage-server-dir server)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun nnttrss-storage--ensure-group-dir (server group)
  "Ensure group directory exists for GROUP on SERVER."
  (let ((dir (nnttrss-storage-group-dir server group)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;;; Overview (NOV format) operations

(defun nnttrss-storage--overview-file (server group)
  "Return path to overview file for GROUP on SERVER."
  (expand-file-name ".overview"
                    (nnttrss-storage-group-dir server group)))

(defun nnttrss-storage-read-overview (server group)
  "Read overview data for GROUP on SERVER.
Returns list of header vectors.
Each vector: [num subject from date msgid refs chars lines xref extra]"
  (let ((file (nnttrss-storage--overview-file server group)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let (headers)
          (while (not (eobp))
            (let* ((line (buffer-substring (point) (line-end-position)))
                   (fields (split-string line "\t"))
                   (header (make-vector 10 nil)))
              (aset header 0 (string-to-number (nth 0 fields)))  ; num
              (aset header 1 (or (nth 1 fields) ""))             ; subject
              (aset header 2 (or (nth 2 fields) ""))             ; from
              (aset header 3 (or (nth 3 fields) ""))             ; date
              (aset header 4 (or (nth 4 fields) ""))             ; msgid
              (aset header 5 (or (nth 5 fields) ""))             ; refs
              (aset header 6 (string-to-number (or (nth 6 fields) "0"))) ; chars
              (aset header 7 (string-to-number (or (nth 7 fields) "0"))) ; lines
              (aset header 8 (or (nth 8 fields) ""))             ; xref
              (aset header 9 (or (nth 9 fields) ""))             ; extra
              (push header headers))
            (forward-line 1))
          (nreverse headers))))))

(defun nnttrss-storage-write-overview (server group headers)
  "Write HEADERS to overview file for GROUP on SERVER.
HEADERS is a list of header vectors."
  (nnttrss-storage--ensure-group-dir server group)
  (let ((file (nnttrss-storage--overview-file server group)))
    (with-temp-file file
      (dolist (header headers)
        (insert (format "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%s\t%s\n"
                        (aref header 0)           ; num
                        (or (aref header 1) "")   ; subject
                        (or (aref header 2) "")   ; from
                        (or (aref header 3) "")   ; date
                        (or (aref header 4) "")   ; msgid
                        (or (aref header 5) "")   ; refs
                        (or (aref header 6) 0)    ; chars
                        (or (aref header 7) 0)    ; lines
                        (or (aref header 8) "")   ; xref
                        (or (aref header 9) "")))); extra
      )))

(defun nnttrss-storage-append-overview (server group headers)
  "Append HEADERS to existing overview for GROUP on SERVER."
  (nnttrss-storage--ensure-group-dir server group)
  (let ((file (nnttrss-storage--overview-file server group)))
    (with-temp-buffer
      (dolist (header headers)
        (insert (format "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%s\t%s\n"
                        (aref header 0)
                        (or (aref header 1) "")
                        (or (aref header 2) "")
                        (or (aref header 3) "")
                        (or (aref header 4) "")
                        (or (aref header 5) "")
                        (or (aref header 6) 0)
                        (or (aref header 7) 0)
                        (or (aref header 8) "")
                        (or (aref header 9) ""))))
      (append-to-file (point-min) (point-max) file))))

;;; Article file operations

(defun nnttrss-storage--article-file (server group num)
  "Return path to article NUM file for GROUP on SERVER."
  (expand-file-name (number-to-string num)
                    (nnttrss-storage-group-dir server group)))

(defun nnttrss-storage-article-exists-p (server group num)
  "Return non-nil if article NUM exists in GROUP on SERVER."
  (file-exists-p (nnttrss-storage--article-file server group num)))

(defun nnttrss-storage-read-article (server group num &optional buffer)
  "Read article NUM from GROUP on SERVER.
If BUFFER is provided, insert content there.  Otherwise use current buffer.
Returns t if successful, nil otherwise."
  (let ((file (nnttrss-storage--article-file server group num)))
    (when (file-exists-p file)
      (with-current-buffer (or buffer (current-buffer))
        (erase-buffer)
        (insert-file-contents file)
        t))))

(defun nnttrss-storage-write-article (server group num content)
  "Write CONTENT as article NUM in GROUP on SERVER."
  (nnttrss-storage--ensure-group-dir server group)
  (let ((file (nnttrss-storage--article-file server group num)))
    (with-temp-file file
      (insert content))))

(defun nnttrss-storage-delete-article (server group num)
  "Delete article NUM from GROUP on SERVER."
  (let ((file (nnttrss-storage--article-file server group num)))
    (when (file-exists-p file)
      (delete-file file))))

;;; Active range operations

(defun nnttrss-storage--active-file (server group)
  "Return path to active file for GROUP on SERVER."
  (expand-file-name ".active"
                    (nnttrss-storage-group-dir server group)))

(defun nnttrss-storage-read-active (server group)
  "Read active range (min . max) for GROUP on SERVER.
Returns nil if no active file exists."
  (let ((file (nnttrss-storage--active-file server group)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun nnttrss-storage-write-active (server group range)
  "Write active RANGE for GROUP on SERVER.
RANGE is a cons cell (min . max)."
  (nnttrss-storage--ensure-group-dir server group)
  (let ((file (nnttrss-storage--active-file server group)))
    (with-temp-file file
      (prin1 range (current-buffer)))))

;;; Mark operations

(defun nnttrss-storage--marks-file (server group)
  "Return path to marks file for GROUP on SERVER."
  (expand-file-name ".marks"
                    (nnttrss-storage-group-dir server group)))

(defun nnttrss-storage-read-marks (server group)
  "Read marks alist for GROUP on SERVER.
Returns alist like ((read RANGES...) (tick NUMS...) (reply NUMS...))."
  (let ((file (nnttrss-storage--marks-file server group)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun nnttrss-storage-write-marks (server group marks)
  "Write MARKS alist for GROUP on SERVER."
  (nnttrss-storage--ensure-group-dir server group)
  (let ((file (nnttrss-storage--marks-file server group)))
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (prin1 marks (current-buffer))))))

;;; ID mapping operations

(defun nnttrss-storage--idmap-file (server group)
  "Return path to ID map file for GROUP on SERVER."
  (expand-file-name ".idmap"
                    (nnttrss-storage-group-dir server group)))

(defun nnttrss-storage--cache-key (server group)
  "Return cache key for SERVER and GROUP."
  (cons server group))

(defun nnttrss-storage-read-id-map (server group)
  "Read ID mapping for GROUP on SERVER.
Returns hash-table mapping TT-RSS SQL ID -> Gnus article number."
  (let ((cache-key (nnttrss-storage--cache-key server group)))
    (or (gethash cache-key nnttrss-storage--id-map-cache)
        (let ((file (nnttrss-storage--idmap-file server group)))
          (if (file-exists-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (let ((map (read (current-buffer))))
                  (puthash cache-key map nnttrss-storage--id-map-cache)
                  map))
            (let ((map (make-hash-table :test 'equal)))
              (puthash cache-key map nnttrss-storage--id-map-cache)
              map))))))

(defun nnttrss-storage-write-id-map (server group map)
  "Write ID MAP for GROUP on SERVER.
MAP is a hash-table mapping TT-RSS SQL ID -> Gnus article number."
  (nnttrss-storage--ensure-group-dir server group)
  (let ((file (nnttrss-storage--idmap-file server group))
        (cache-key (nnttrss-storage--cache-key server group)))
    (puthash cache-key map nnttrss-storage--id-map-cache)
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (prin1 map (current-buffer))))))

(defun nnttrss-storage-id-to-num (server group sql-id)
  "Convert SQL-ID to Gnus article number in GROUP on SERVER.
Returns nil if not found."
  (let ((map (nnttrss-storage-read-id-map server group)))
    (gethash sql-id map)))

(defun nnttrss-storage-num-to-id (server group num)
  "Convert Gnus article NUM to TT-RSS SQL ID in GROUP on SERVER.
Returns nil if not found."
  (let ((map (nnttrss-storage-read-id-map server group))
        result)
    (maphash (lambda (sql-id article-num)
               (when (= article-num num)
                 (setq result sql-id)))
             map)
    result))

(defun nnttrss-storage-register-id (server group sql-id num)
  "Register mapping from SQL-ID to NUM in GROUP on SERVER."
  (let ((map (nnttrss-storage-read-id-map server group)))
    (puthash sql-id num map)
    (nnttrss-storage-write-id-map server group map)))

(defun nnttrss-storage-next-article-num (server group)
  "Return next available article number for GROUP on SERVER."
  (let ((active (nnttrss-storage-read-active server group)))
    (if active
        (1+ (cdr active))
      1)))

;;; Feed metadata operations

(defun nnttrss-storage--feeds-file (server)
  "Return path to feeds metadata file for SERVER."
  (expand-file-name ".feeds"
                    (nnttrss-storage-server-dir server)))

(defun nnttrss-storage-read-feeds (server)
  "Read feeds metadata for SERVER.
Returns alist of (group-name . plist) where plist contains feed info."
  (let ((file (nnttrss-storage--feeds-file server)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun nnttrss-storage-write-feeds (server feeds)
  "Write FEEDS metadata for SERVER."
  (nnttrss-storage-init server)
  (let ((file (nnttrss-storage--feeds-file server)))
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (prin1 feeds (current-buffer))))))

;;; Cleanup operations

(defun nnttrss-storage-expire-articles (server group keep-days)
  "Delete articles older than KEEP-DAYS from GROUP on SERVER.
Returns number of articles deleted."
  (let ((dir (nnttrss-storage-group-dir server group))
        (cutoff (- (float-time) (* keep-days 24 60 60)))
        (deleted 0))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "^[0-9]+$"))
        (when (< (float-time (file-attribute-modification-time
                              (file-attributes file)))
                 cutoff)
          (delete-file file)
          (cl-incf deleted))))
    deleted))

(defun nnttrss-storage-clear (server &optional group)
  "Clear all storage for SERVER, or just GROUP if specified."
  (let ((cache-key (nnttrss-storage--cache-key server group)))
    (if group
        (let ((dir (nnttrss-storage-group-dir server group)))
          (when (file-directory-p dir)
            (delete-directory dir t))
          (remhash cache-key nnttrss-storage--id-map-cache))
      ;; Clear entire server
      (let ((dir (nnttrss-storage-server-dir server)))
        (when (file-directory-p dir)
          (delete-directory dir t)))
      ;; Clear all cached maps for this server
      (maphash (lambda (key _val)
                 (when (equal (car key) server)
                   (remhash key nnttrss-storage--id-map-cache)))
               nnttrss-storage--id-map-cache))))

(defun nnttrss-storage-list-groups (server)
  "List all groups with storage for SERVER."
  (let ((dir (nnttrss-storage-server-dir server)))
    (when (file-directory-p dir)
      (cl-remove-if-not
       (lambda (name)
         (file-directory-p (expand-file-name name dir)))
       (directory-files dir nil "^[^.]")))))

(provide 'nnttrss-storage)
;;; nnttrss-storage.el ends here
