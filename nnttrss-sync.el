;;; nnttrss-sync.el --- Async sync for nnttrss -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: news, rss

;;; Commentary:

;; Async background synchronization layer for nnttrss.
;; Uses url-retrieve for non-blocking HTTP requests.

;;; Code:

(require 'cl-lib)
(require 'range)
(require 'ttrss)
(require 'nnttrss-storage)
(require 'url-parse)

;;; Custom variables

(defgroup nnttrss-sync nil
  "Sync settings for nnttrss."
  :group 'nnttrss)

(defcustom nnttrss-sync-interval 300
  "Seconds between automatic background syncs.
Set to nil to disable automatic syncing."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'nnttrss-sync)

(defcustom nnttrss-sync-on-open t
  "If non-nil, trigger sync when server is opened."
  :type 'boolean
  :group 'nnttrss-sync)

(defcustom nnttrss-cache-full-content t
  "If non-nil, cache full article content during background sync.
This makes opening articles instant but increases storage size."
  :type 'boolean
  :group 'nnttrss-sync)

(defcustom nnttrss-content-fetch-batch-size 50
  "Number of articles to fetch content for in each batch during sync."
  :type 'integer
  :group 'nnttrss-sync)

(defcustom nnttrss-cache-max-age 90
  "Maximum age in days for cached articles.
Articles older than this are removed during cleanup.
Set to nil to disable age-based cleanup."
  :type '(choice (integer :tag "Days")
                 (const :tag "Unlimited" nil))
  :group 'nnttrss-sync)

(defcustom nnttrss-cache-max-articles-per-feed 500
  "Maximum number of articles to keep per feed.
Oldest articles are removed when this limit is exceeded.
Set to nil to disable size-based cleanup."
  :type '(choice (integer :tag "Max articles")
                 (const :tag "Unlimited" nil))
  :group 'nnttrss-sync)

(defvar nnttrss-sync-hook nil
  "Hook run after successful sync.
Called with three arguments: server, num-feeds, num-articles.")

;;; Internal variables

(defvar nnttrss-sync--timers (make-hash-table :test 'equal)
  "Hash table of server -> sync timer.")

(defvar nnttrss-sync--in-progress (make-hash-table :test 'equal)
  "Hash table of server -> sync-in-progress flag.")

(defvar nnttrss-sync--pending-marks (make-hash-table :test 'equal)
  "Hash table of server -> list of pending mark updates.
Each entry is a plist with :article-id, :field, :mode.")

(defvar nnttrss-sync--pending-content (make-hash-table :test 'equal)
  "Hash table of server -> list of article IDs needing content fetch.")

(defvar nnttrss-sync--context (make-hash-table :test 'equal)
  "Hash table of server -> sync context plist.
Context contains :address, :sid, :feeds-count, :articles-count.")

;;; Timer management

(defun nnttrss-sync-start-timer (server address sid)
  "Start background sync timer for SERVER using ADDRESS and SID."
  (nnttrss-sync-stop-timer server)
  (when nnttrss-sync-interval
    (let ((timer (run-with-timer
                  nnttrss-sync-interval
                  nnttrss-sync-interval
                  (lambda ()
                    (nnttrss-sync--async server address sid)))))
      (puthash server timer nnttrss-sync--timers))))

(defun nnttrss-sync-stop-timer (server)
  "Stop background sync timer for SERVER."
  (when-let ((timer (gethash server nnttrss-sync--timers)))
    (cancel-timer timer)
    (remhash server nnttrss-sync--timers)))

;;; Sync control

(defun nnttrss-sync (server address sid)
  "Manually trigger async sync for SERVER.
ADDRESS and SID are the TT-RSS server address and session ID."
  (if (gethash server nnttrss-sync--in-progress)
      (message "nnttrss: Sync already in progress for %s" server)
    (nnttrss-sync--async server address sid)))

(defun nnttrss-sync-cancel (server)
  "Cancel any running sync for SERVER."
  (remhash server nnttrss-sync--in-progress)
  (remhash server nnttrss-sync--pending-content)
  (remhash server nnttrss-sync--context))

(defun nnttrss-sync-status (server)
  "Return sync status for SERVER: nil, `syncing', or `idle'."
  (cond ((gethash server nnttrss-sync--in-progress) 'syncing)
        ((gethash server nnttrss-sync--timers) 'idle)
        (t nil)))

;;; Mark queueing

(defun nnttrss-sync-queue-mark (server article-id field mode)
  "Queue a mark update for ARTICLE-ID on SERVER.
FIELD is 0 for starred, 2 for unread.
MODE is 0 to unset, 1 to set.
Deduplicates updates: newer update for same article+field replaces older."
  (let ((pending (gethash server nnttrss-sync--pending-marks)))
    ;; Remove any existing update for same article+field
    (setq pending
          (cl-remove-if (lambda (entry)
                          (and (= (plist-get entry :article-id) article-id)
                               (= (plist-get entry :field) field)))
                        pending))
    ;; Add the new update
    (push (list :article-id article-id :field field :mode mode) pending)
    (puthash server pending nnttrss-sync--pending-marks)))

(defun nnttrss-sync-pending-marks-count (server)
  "Return count of pending mark updates for SERVER."
  (length (gethash server nnttrss-sync--pending-marks)))

;;; Main sync flow

(defun nnttrss-sync--async (server address sid)
  "Asynchronously sync feeds and headlines for SERVER.
ADDRESS is the TT-RSS API URL, SID is the session ID."
  (when (and sid (not (gethash server nnttrss-sync--in-progress)))
    (puthash server t nnttrss-sync--in-progress)
    (puthash server (list :address address :sid sid
                          :feeds-count 0 :articles-count 0)
             nnttrss-sync--context)
    ;; Initialize storage
    (nnttrss-storage-init server)
    ;; First flush any pending mark updates
    (let ((pending (gethash server nnttrss-sync--pending-marks)))
      (if pending
          (progn
            (message "nnttrss: Syncing %d pending mark changes..."
                     (length pending))
            (nnttrss-sync--flush-marks server))
        ;; No pending marks, proceed directly to feed sync
        (nnttrss-sync--fetch-feeds server)))))

(defun nnttrss-sync--flush-marks (server)
  "Flush pending mark updates for SERVER to the TT-RSS server."
  (let* ((ctx (gethash server nnttrss-sync--context))
         (address (plist-get ctx :address))
         (sid (plist-get ctx :sid))
         (pending (gethash server nnttrss-sync--pending-marks))
         (unread-true '())
         (unread-false '())
         (starred-true '())
         (starred-false '()))
    ;; Group updates by field and mode
    (dolist (update pending)
      (let ((id (plist-get update :article-id))
            (field (plist-get update :field))
            (mode (plist-get update :mode)))
        (cond ((and (= field 2) (= mode 1)) (push id unread-true))
              ((and (= field 2) (= mode 0)) (push id unread-false))
              ((and (= field 0) (= mode 1)) (push id starred-true))
              ((and (= field 0) (= mode 0)) (push id starred-false)))))
    ;; Clear the queue
    (puthash server nil nnttrss-sync--pending-marks)
    ;; Send grouped updates
    (nnttrss-sync--send-marks
     server address sid
     (list (cons (cons 2 1) unread-true)
           (cons (cons 2 0) unread-false)
           (cons (cons 0 1) starred-true)
           (cons (cons 0 0) starred-false)))))

(defun nnttrss-sync--send-marks (server address sid groups)
  "Send mark updates for GROUPS on SERVER using ADDRESS and SID.
GROUPS is a list of ((field . mode) . article-ids)."
  (let ((remaining (cl-remove-if (lambda (g) (null (cdr g))) groups)))
    (if (null remaining)
        ;; All done, continue to feeds
        (nnttrss-sync--fetch-feeds server)
      ;; Send first group
      (let* ((group (car remaining))
             (field (caar group))
             (mode (cdar group))
             (ids (cdr group)))
        (ttrss-update-article-async
         address sid ids
         (lambda (_result)
           (nnttrss-sync--send-marks server address sid (cdr remaining)))
         (lambda (err)
           (nnttrss-sync--error server err))
         :mode mode
         :field field)))))

(defun nnttrss-sync--fetch-feeds (server)
  "Fetch feeds from TT-RSS SERVER."
  (let* ((ctx (gethash server nnttrss-sync--context))
         (address (plist-get ctx :address))
         (sid (plist-get ctx :sid)))
    (message "nnttrss: Syncing feeds...")
    (ttrss-get-feeds-async
     address sid
     (lambda (feeds)
       (nnttrss-sync--feeds-callback server feeds))
     (lambda (err)
       (nnttrss-sync--error server err))
     :include_nested t
     :cat_id -4)))

(defun nnttrss-sync--feeds-callback (server feeds)
  "Handle FEEDS response for SERVER."
  (let ((ctx (gethash server nnttrss-sync--context)))
    ;; Store feeds in storage layer
    (let ((feeds-alist (mapcar (lambda (f)
                                 (cons (plist-get f :title) f))
                               feeds)))
      (nnttrss-storage-write-feeds server feeds-alist))
    ;; Update context
    (plist-put ctx :feeds-count (length feeds))
    ;; Fetch headlines for each feed
    (nnttrss-sync--fetch-headlines server feeds)))

(defun nnttrss-sync--fetch-headlines (server feeds)
  "Fetch headlines for FEEDS on SERVER."
  (let* ((ctx (gethash server nnttrss-sync--context))
         (address (plist-get ctx :address))
         (sid (plist-get ctx :sid)))
    ;; For simplicity, fetch all headlines at once with feed_id -4 (all feeds)
    ;; We determine since_id from the latest article we have across all feeds
    (let ((max-id 0))
      (dolist (feed feeds)
        (let* ((group (plist-get feed :title))
               (active (nnttrss-storage-read-active server group)))
          (when active
            (let* ((id-map (nnttrss-storage-read-id-map server group))
                   (max-num (cdr active)))
              (when (and id-map max-num)
                (maphash (lambda (sql-id num)
                           (when (= num max-num)
                             (setq max-id (max max-id sql-id))))
                         id-map))))))
      (message "nnttrss: Fetching headlines since ID %d..." max-id)
      (ttrss-get-headlines-async
       address sid
       (lambda (headlines)
         (nnttrss-sync--headlines-callback server headlines feeds))
       (lambda (err)
         (nnttrss-sync--error server err))
       :feed_id -4
       :limit -1
       :since_id max-id
       :show_content nnttrss-cache-full-content))))

(defun nnttrss-sync--headlines-callback (server headlines feeds)
  "Handle HEADLINES response for SERVER.  FEEDS is the list of feeds."
  (let ((ctx (gethash server nnttrss-sync--context))
        (new-articles 0)
        (feed-map (make-hash-table :test 'eq)))
    ;; Build feed-id -> feed-title map
    (dolist (feed feeds)
      (puthash (plist-get feed :id) (plist-get feed :title) feed-map))
    ;; Process each headline
    (dolist (headline headlines)
      (let* ((sql-id (plist-get headline :id))
             (feed-id (plist-get headline :feed_id))
             (group (gethash feed-id feed-map)))
        (when group
          ;; Check if we already have this article
          (unless (nnttrss-storage-id-to-num server group sql-id)
            ;; Assign new article number
            (let ((num (nnttrss-storage-next-article-num server group)))
              (nnttrss-storage-register-id server group sql-id num)
              ;; Update active range
              (let ((active (nnttrss-storage-read-active server group)))
                (nnttrss-storage-write-active
                 server group
                 (cons (if active (min (car active) num) num)
                       (if active (max (cdr active) num) num))))
              ;; Write overview
              (let ((header (nnttrss-sync--make-header headline num group)))
                (nnttrss-storage-append-overview server group (list header)))
              ;; Write article if content is available
              (when-let ((content (plist-get headline :content)))
                (nnttrss-storage-write-article
                 server group num
                 (nnttrss-sync--format-article headline num group)))
              (cl-incf new-articles))))))
    ;; Update context
    (plist-put ctx :articles-count new-articles)
    (message "nnttrss: Added %d new articles" new-articles)
    ;; Fetch unread state
    (nnttrss-sync--fetch-unread server)))

(defun nnttrss-sync--make-header (headline num _group)
  "Create overview header vector from HEADLINE at NUM in GROUP."
  (let ((header (make-vector 10 nil))
        (sql-id (plist-get headline :id))
        (feed-id (plist-get headline :feed_id)))
    (aset header 0 num)
    (aset header 1 (or (plist-get headline :title) ""))
    (aset header 2 (or (url-host
                        (url-generic-parse-url
                         (or (plist-get headline :link) "")))
                       ""))
    (aset header 3 (if (plist-get headline :updated)
                       (format-time-string
                        "%a, %d %b %Y %T %z"
                        (seconds-to-time (plist-get headline :updated)))
                     ""))
    (aset header 4 (format "<%d@%d.nnttrss>" sql-id feed-id))
    (aset header 5 "")  ; refs
    (aset header 6 (length (or (plist-get headline :content) "")))
    (aset header 7 -1)  ; lines
    (aset header 8 "")  ; xref
    (aset header 9 "")  ; extra
    header))

(defun nnttrss-sync--format-article (headline _num group)
  "Format HEADLINE as RFC 822 article text at NUM in GROUP."
  (let ((sql-id (plist-get headline :id))
        (feed-id (plist-get headline :feed_id))
        (title (or (plist-get headline :title) ""))
        (link (or (plist-get headline :link) ""))
        (content (or (plist-get headline :content) ""))
        (updated (plist-get headline :updated)))
    (format "Newsgroups: %s
Subject: %s
From: %s
Date: %s
Message-ID: <%d@%d.nnttrss>
Content-Type: text/html; charset=utf-8
X-TTRSS-ID: %d
X-TTRSS-Feed-ID: %d
X-TTRSS-Link: %s

%s
<p><a href=\"%s\">%s</a></p>
"
            group
            title
            (or (url-host (url-generic-parse-url link)) "unknown")
            (if updated
                (format-time-string "%a, %d %b %Y %T %z"
                                    (seconds-to-time updated))
              "")
            sql-id feed-id
            sql-id feed-id link
            content link
            (or (url-host (url-generic-parse-url link)) ""))))

(defun nnttrss-sync--fetch-unread (server)
  "Fetch unread article IDs for SERVER."
  (let* ((ctx (gethash server nnttrss-sync--context))
         (address (plist-get ctx :address))
         (sid (plist-get ctx :sid)))
    (message "nnttrss: Updating read/unread marks...")
    (ttrss-get-headlines-async
     address sid
     (lambda (unread-headlines)
       (nnttrss-sync--unread-callback server unread-headlines))
     (lambda (err)
       (nnttrss-sync--error server err))
     :feed_id -4
     :view_mode "unread"
     :limit -1)))

(defun nnttrss-sync--unread-callback (server unread-headlines)
  "Handle UNREAD-HEADLINES response for SERVER."
  (let ((unread-set (make-hash-table :test 'eq))
        (feeds (nnttrss-storage-read-feeds server)))
    ;; Build set of unread article IDs
    (dolist (h unread-headlines)
      (puthash (plist-get h :id) t unread-set))
    ;; Update marks for each feed
    (dolist (feed-entry feeds)
      (let* ((group (car feed-entry))
             (id-map (nnttrss-storage-read-id-map server group))
             (read-list '()))
        (when id-map
          (maphash (lambda (sql-id num)
                     (unless (gethash sql-id unread-set)
                       (push num read-list)))
                   id-map)
          ;; Merge with existing local read marks (local takes precedence)
          ;; This preserves marks set locally that haven't been confirmed by server yet
          (let* ((old-marks (nnttrss-storage-read-marks server group))
                 (old-read (cdr (assq 'read old-marks)))
                 (tick-marks (cdr (assq 'tick old-marks)))
                 ;; Union of server-read and locally-read articles
                 (merged-read (range-concat
                               old-read
                               (range-compress-list (sort read-list #'<))))
                 (new-marks `((read . ,merged-read)
                              ,@(when tick-marks
                                  `((tick . ,tick-marks))))))
            (nnttrss-storage-write-marks server group new-marks))))))
  ;; Fetch starred articles
  (nnttrss-sync--fetch-starred server))

(defun nnttrss-sync--fetch-starred (server)
  "Fetch starred article IDs for SERVER."
  (let* ((ctx (gethash server nnttrss-sync--context))
         (address (plist-get ctx :address))
         (sid (plist-get ctx :sid)))
    (ttrss-get-headlines-async
     address sid
     (lambda (starred-headlines)
       (nnttrss-sync--starred-callback server starred-headlines))
     (lambda (err)
       (nnttrss-sync--error server err))
     :feed_id -4
     :view_mode "marked"
     :limit -1)))

(defun nnttrss-sync--starred-callback (server starred-headlines)
  "Handle STARRED-HEADLINES response for SERVER."
  (let ((starred-set (make-hash-table :test 'eq))
        (feeds (nnttrss-storage-read-feeds server)))
    ;; Build set of starred article IDs
    (dolist (h starred-headlines)
      (puthash (plist-get h :id) t starred-set))
    ;; Update tick marks for each feed
    (dolist (feed-entry feeds)
      (let* ((group (car feed-entry))
             (id-map (nnttrss-storage-read-id-map server group))
             (tick-list '()))
        (when id-map
          (maphash (lambda (sql-id num)
                     (when (gethash sql-id starred-set)
                       (push num tick-list)))
                   id-map)
          ;; Update marks
          (let* ((old-marks (nnttrss-storage-read-marks server group))
                 (read-marks (cdr (assq 'read old-marks)))
                 (new-marks `((read . ,read-marks)
                              ,@(when tick-list
                                  `((tick . ,(range-compress-list
                                              (sort tick-list #'<))))))))
            (nnttrss-storage-write-marks server group new-marks))))))
  ;; Check if we need to fetch missing content
  (if nnttrss-cache-full-content
      (nnttrss-sync--check-missing-content server)
    (nnttrss-sync--complete server)))

(defun nnttrss-sync--check-missing-content (server)
  "Check for articles missing content on SERVER."
  (let ((missing-ids '())
        (feeds (nnttrss-storage-read-feeds server)))
    ;; Find articles without content
    (dolist (feed-entry feeds)
      (let* ((group (car feed-entry))
             (id-map (nnttrss-storage-read-id-map server group)))
        (when id-map
          (maphash (lambda (sql-id num)
                     (unless (nnttrss-storage-article-exists-p server group num)
                       (push sql-id missing-ids)))
                   id-map))))
    (if missing-ids
        (progn
          (message "nnttrss: Fetching content for %d articles..."
                   (length missing-ids))
          (puthash server (nreverse missing-ids) nnttrss-sync--pending-content)
          (nnttrss-sync--fetch-content-batch server))
      (nnttrss-sync--complete server))))

(defun nnttrss-sync--fetch-content-batch (server)
  "Fetch content for a batch of articles on SERVER."
  (let ((pending (gethash server nnttrss-sync--pending-content)))
    (if (null pending)
        (nnttrss-sync--complete server)
      ;; Take a batch
      (let ((batch '())
            (count 0))
        (while (and pending (< count nnttrss-content-fetch-batch-size))
          (push (pop pending) batch)
          (cl-incf count))
        (puthash server pending nnttrss-sync--pending-content)
        ;; Fetch this batch
        (let* ((ctx (gethash server nnttrss-sync--context))
               (address (plist-get ctx :address))
               (sid (plist-get ctx :sid)))
          (apply #'ttrss-get-article-async
                 address sid
                 (lambda (articles)
                   (nnttrss-sync--content-callback server articles))
                 (lambda (err)
                   (nnttrss-sync--error server err))
                 (nreverse batch)))))))

(defun nnttrss-sync--content-callback (server articles)
  "Handle ARTICLES content response for SERVER."
  (let ((feeds (nnttrss-storage-read-feeds server))
        (feed-map (make-hash-table :test 'eq)))
    ;; Build feed-id -> group map
    (dolist (feed-entry feeds)
      (let ((group (car feed-entry))
            (feed (cdr feed-entry)))
        (puthash (plist-get feed :id) group feed-map)))
    ;; Store each article
    (dolist (article articles)
      (let* ((sql-id (plist-get article :id))
             (feed-id (plist-get article :feed_id))
             (group (gethash feed-id feed-map)))
        (when group
          (when-let ((num (nnttrss-storage-id-to-num server group sql-id)))
            (nnttrss-storage-write-article
             server group num
             (nnttrss-sync--format-article article num group)))))))
  ;; Continue with next batch or finish
  (let ((pending (gethash server nnttrss-sync--pending-content)))
    (if pending
        (progn
          (message "nnttrss: %d articles remaining..." (length pending))
          (nnttrss-sync--fetch-content-batch server))
      (nnttrss-sync--complete server))))

(defun nnttrss-sync--complete (server)
  "Finalize sync for SERVER."
  (let ((ctx (gethash server nnttrss-sync--context)))
    (remhash server nnttrss-sync--in-progress)
    (remhash server nnttrss-sync--pending-content)
    ;; Run cleanup if configured
    (when (or nnttrss-cache-max-age nnttrss-cache-max-articles-per-feed)
      (nnttrss-sync--cleanup server))
    ;; Report completion
    (let ((feeds-count (plist-get ctx :feeds-count))
          (articles-count (plist-get ctx :articles-count)))
      (message "nnttrss: Sync complete (%d feeds, %d new articles)"
               feeds-count articles-count)
      ;; Run hook
      (run-hook-with-args 'nnttrss-sync-hook server feeds-count articles-count))
    (remhash server nnttrss-sync--context)))

(defun nnttrss-sync--error (server error-msg)
  "Handle sync error for SERVER with ERROR-MSG."
  (remhash server nnttrss-sync--in-progress)
  (remhash server nnttrss-sync--pending-content)
  (remhash server nnttrss-sync--context)
  (if (string-match-p "not logged in\\|login\\|session" (downcase error-msg))
      (message "nnttrss: Session expired for %s - please reconnect" server)
    (message "nnttrss: Sync failed for %s - %s" server error-msg)))

;;; Cache cleanup

(defun nnttrss-sync--cleanup (server)
  "Clean up old articles from SERVER storage."
  (let ((feeds (nnttrss-storage-read-feeds server))
        (total-removed 0))
    ;; Age-based cleanup
    (when nnttrss-cache-max-age
      (dolist (feed-entry feeds)
        (let ((group (car feed-entry)))
          (cl-incf total-removed
                   (nnttrss-storage-expire-articles
                    server group nnttrss-cache-max-age)))))
    (when (> total-removed 0)
      (message "nnttrss: Cleanup removed %d old articles" total-removed))))

;;;###autoload
(defun nnttrss-cleanup-cache (server)
  "Manually trigger cache cleanup for SERVER."
  (interactive "sServer: ")
  (if (or nnttrss-cache-max-age nnttrss-cache-max-articles-per-feed)
      (nnttrss-sync--cleanup server)
    (message "nnttrss: No cleanup limits configured")))

(provide 'nnttrss-sync)
;;; nnttrss-sync.el ends here
