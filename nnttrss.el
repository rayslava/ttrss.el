;;; nnttrss.el --- interfacing with Tiny Tiny RSS  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Pedro Silva
;; Copyright (C) 2023 Slava Barinov

;; Author: Pedro Silva <psilva+git@pedrosilva.pt>
;;         Slava Barinov <rayslava@gmail.com>
;; Created: 01 April 2013
;; Updated: 22 January 2023
;; Version: 0.0.2
;; Keywords: news, local
;; Package-Requires ((emacs "28.2"))

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

;;; Code:

(require 'ttrss)
(require 'gnus)
(require 'nnoo)
(require 'nnmail)
(require 'nnrss)
(require 'nnheader)
(require 'mm-util)

;; Declare dynamic variables from gnus
(defvar gnus-read-mark)
(defvar gnus-del-mark)
(defvar gnus-catchup-mark)
(defvar gnus-unread-mark)
(defvar gnus-ticked-mark)

(nnoo-declare nnttrss)
(nnoo-define-basics nnttrss)
(gnus-declare-backend "nnttrss" 'news 'address 'agent)

(defvoo nnttrss-address nil
  "Address of the tt-rss server.")

(defvoo nnttrss-user nil
  "Username to use for authentication to the tt-rss server.")

(defvoo nnttrss-password nil
  "Password to use for authentication to the tt-rss server.")

(defvoo nnttrss-directory (nnheader-concat gnus-directory "ttrss/")
  "Where nnttrss will save its files.")

(defvoo nnttrss-fetch-partial-articles nil
  "If non-nil, nnttrss will fetch partial articles.")

(defvoo nnttrss-status-string "")

(defvar nnttrss--sid nil
  "Current session id, if any, set after successful login.")

(defvar nnttrss--api-level nil
  "API version level, increased with each API functionality change.")

(defvar nnttrss--server-version nil
  "Server version number.")

(defvar nnttrss--headlines nil
  "List of all headline propertly lists.")

(defvar nnttrss--last-article-id 0
  "Internal server ID of last article nnttrss knows about.")

(defvar nnttrss--article-map nil
  "Property list of association lists.
The properties are group name strings.  Values are association
lists of SQL IDs to article numbers.")

(defvar nnttrss--feeds nil
  "List of all feed property lists.")

;;; Async sync configuration

(defgroup nnttrss nil
  "Gnus backend for Tiny Tiny RSS."
  :group 'gnus)

(defcustom nnttrss-sync-interval 300
  "Seconds between automatic background syncs.
Set to nil to disable automatic syncing."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'nnttrss)

(defvar nnttrss--sync-timer nil
  "Timer for background sync.")

(defvar nnttrss--sync-in-progress nil
  "Non-nil when async sync is running.")


;;; Interface bits
(defun nnttrss-decode-gnus-group (group)
  (decode-coding-string group 'utf-8))

(defun nnttrss-encode-gnus-group (group)
  (encode-coding-string group 'utf-8))

(deffoo nnttrss-open-server (server &optional defs)
  (unless (nnttrss-server-opened server)
    (dolist (def '(nnttrss-address nnttrss-user nnttrss-password))
      (unless (assq def defs)
	(setq defs (append defs (list (list def server)))))
      (setf (symbol-value def) (cadr (assq def defs))))
    (nnttrss--read-feeds)
    (nnttrss--read-headlines)
    (nnttrss--read-article-map)
    (let ((sid (ttrss-login nnttrss-address nnttrss-user nnttrss-password)))
      (setq nnttrss--sid sid
	    nnttrss--server-version (ttrss-get-version nnttrss-address nnttrss--sid)
	    nnttrss--api-level (ttrss-get-api-level nnttrss-address nnttrss--sid))))
  (unless nnttrss--sync-timer
    (nnttrss--start-sync-timer)
    (nnttrss--sync-async))
  (nnoo-change-server 'nnttrss server defs))

(deffoo nnttrss-close-server (&optional server)
  (when (nnttrss-server-opened server)
    (nnttrss--stop-sync-timer)
    (ttrss-logout nnttrss-address nnttrss--sid)
    (setq nnttrss--sid nil
	  nnttrss--server-version nil
	  nnttrss--api-level nil
	  nnttrss--feeds nil
	  nnttrss--headlines nil
	  nnttrss--article-map nil
	  nnttrss--last-article-id 0
	  nnttrss--sync-in-progress nil)))

(deffoo nnttrss-request-close ()
  t)

(deffoo nnttrss-server-opened (&optional server)
  "Check if server is opened.  Non-blocking (checks local state only).
Session validity is verified during background sync."
  (and nnttrss--sid t))

(deffoo nnttrss-request-list (&optional server)
  "Return list of feeds from cache.  Non-blocking.
Use `nnttrss-sync' to refresh from server."
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (feed (mapcar 'cdr nnttrss--feeds))
      (let* ((title (plist-get feed :title))
	     (id (plist-get feed :id))
	     (article-ids (nnttrss--feed-articles id)))
	(if article-ids
	    (insert (format "\"%s\" %d %d y\n"
			    title
			    (apply 'max article-ids)
			    (apply 'min article-ids)))
	  (insert (format "\"%s\" 0 1 y\n" title)))))
    t))

(deffoo nnttrss-status-message (&optional server)
  nnttrss-status-string)

(deffoo nnttrss-request-group (group &optional server fast info)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (setq group (nnttrss-decode-gnus-group group))
    (if fast
	t
      (let* ((feed (cdr (assoc group nnttrss--feeds)))
	     (id (plist-get feed :id))
	     (article-ids (nnttrss--feed-articles id))
	     (total-articles (length article-ids)))
	(if article-ids
	    (insert (format "211 %d %d %d \"%s\"\n"
			    total-articles
			    (apply 'min article-ids)
			    (apply 'max article-ids)
			    (nnttrss-encode-gnus-group group)))
	  (insert (format "211 %d %d %d \"%s\"\n"
			  total-articles 1 0
			  (nnttrss-encode-gnus-group group))))
	t))))

;; (deffoo nnttrss-request-group-scan (group &optional server info)
;;   (debug))

(deffoo nnttrss-retrieve-headers (articles &optional group server fetch-old)
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (article articles)
      (let* ((article-obj (nnttrss--find-article article group))
	     (unread (plist-get article-obj :unread))
	     (fetch (or fetch-old unread)))
	(when fetch
	  (insert (nnttrss--format-header article group))))))
  'nov)

(deffoo nnttrss-request-article (article &optional group server to-buffer)
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let ((destination (or to-buffer nntp-server-buffer))
	(article (nnttrss--find-article article group)))
    (with-current-buffer destination
      (erase-buffer)
      (insert (format "Newgroups: %s\nSubject: %s\nFrom: %s\nDate: %s\nMIME-Version: 1.0\nContent-Type: text/html; charset=utf-8\nContent-Transfer-Encoding: quoted-printable\n\n"
		      group
		      (plist-get article :title)
		      (url-host (url-generic-parse-url (plist-get article :link)))
		      (format-time-string "%a, %d %b %Y %T %z"
					  (seconds-to-time (plist-get article :updated)))))
      (let* ((start (point)))
	(insert (plist-get article :content))
	(insert (format "<p><a href=\"%s\">%s</a></p>"
			(plist-get article :link)
			(url-host (url-generic-parse-url (plist-get article :link)))))
	(insert "\n\n"))))
  (cons group article))

(deffoo nnttrss-close-group (group &optional server)
  t)

(deffoo nnttrss-request-update-mark (group article mark)
  "Update marks for ARTICLE in GROUP.

Currently `unread' and `ticked' marks are supported, the latter
one changes 'Starred' status in TT-RSS.
Setting up an `unread' mark removes `ticked' as well."
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let* ((group-id (plist-get (cdr (assoc group nnttrss--feeds)) :id))
	 (article-id (nnttrss--get-article-id article group-id)))
    (cond ((or (= mark gnus-read-mark)
	       (= mark gnus-del-mark)
	       (= mark gnus-catchup-mark))
	   (ttrss-update-article nnttrss-address nnttrss--sid article-id
				 :mode 0 :field 2))
	  ((= mark gnus-unread-mark)               ;;; Untick and unmark
	   (ttrss-update-article nnttrss-address nnttrss--sid article-id
				 :mode 1 :field 2)
	   (ttrss-update-article nnttrss-address nnttrss--sid article-id
				 :mode 0 :field 0))
	  ((= mark gnus-ticked-mark)
	   (ttrss-update-article nnttrss-address nnttrss--sid article-id
				 :mode 1 :field 0))
	  (t (message "nnttrss: Unknown mark seting %s" mark)))
    mark))

(deffoo nnttrss-request-update-info  (group info &optional server)
  "Update INFO for GROUP about marked and unread articles.
Reads from cache only - use `nnttrss-sync' to refresh from server."
  (when group
    (setq group (nnttrss-decode-gnus-group group)))
  (let* ((feed (cdr (assoc group nnttrss--feeds)))
	 (id (plist-get feed :id)))
    (setf (gnus-info-read info)
	  (gnus-compress-sequence
	   (nnttrss--get-filtered-articles group :unread nil)))
    (setf (gnus-info-marks info)
	  (list (cons 'tick
		      (gnus-compress-sequence
		       (nnttrss--get-filtered-articles group :marked t)))))))

(deffoo nnttrss-request-set-mark (group actions &optional server)
  (dolist (action actions)
    (let ((group-id (plist-get (cdr (assoc group nnttrss--feeds)) :id))
	  (updatelist nil)
	  (unreadstate 0)
	  (tickedstate 0))
      (cl-destructuring-bind (range action marks) action
	(when (and (memq 'tick marks)
		   (eq action 'add))
	  (push 'read marks))
	(pcase action
	  ('add (progn
		  (setf unreadstate 0)
		  (setf tickedstate 1)))
	  ('del (progn
		  (setf unreadstate 1)
		  (setf tickedstate 0))))
	(dolist (article (gnus-uncompress-sequence range))
	  (let ((article-id (nnttrss--get-article-id article group-id)))
	    (push article-id updatelist)))
	(when (memq 'read marks)
	  (ttrss-update-article nnttrss-address nnttrss--sid
				updatelist
				:mode unreadstate :field 2))
	(when (memq 'tick marks)
	  (ttrss-update-article nnttrss-address nnttrss--sid
				updatelist
				:mode tickedstate :field 0))))))

;;; Private bits

(defun nnttrss--get-filtered-articles (group key value)
  "Return list of articles in `GROUP' where `KEY' is eq to `VALUE'."
  (let* ((group-id (plist-get (cdr (assoc group nnttrss--feeds)) :id))
	 (articles (lax-plist-get nnttrss--article-map group-id)))
    (cl-sort
     (delq nil
	   (mapcar (lambda (art)
		     (when (eq
			    (plist-get (cdr (assoc (car art) nnttrss--headlines)) key)
			    value)
		       (cdr art)))
		   articles))
     #'<)))

(defun nnttrss--update-articles-info (group)
  "Download information on articles from `GROUP' and merges changes into
`nnttrss--headlines'"
  (let* ((group-id (plist-get (cdr (assoc group nnttrss--feeds)) :id))
	 (articles (cl-sort (mapcar #'car
				    (lax-plist-get nnttrss--article-map group-id))
			    #'<))
	 (headlines (ttrss-get-headlines
		     nnttrss-address
		     nnttrss--sid
		     :feed_id group-id
		     :limit -1
		     :since_id (car articles))))
    (mapc (lambda (art)
	    (let* ((article-id (plist-get art :id))
		   (article (alist-get article-id nnttrss--headlines)))
	      (setf (plist-get article :unread) (plist-get art :unread))
	      (setf (plist-get article :marked) (plist-get art :marked))
	      (when (plist-get article :marked)
		(message "Marked article: %s" article-id))
	      ))
	  headlines)))

(defun nnttrss--find-article (number group)
  "Return property list for article NUMBER in GROUP."
  (let* ((group-id (plist-get (cdr (assoc group nnttrss--feeds)) :id))
	 (article-id (nnttrss--get-article-id number group-id))
	 (article (cdr (assoc article-id nnttrss--headlines)))
	 (content (or (plist-get article :content)
		      (nth 1 (ttrss-get-article nnttrss-address nnttrss--sid article-id)))))
    (plist-put article :content content)
    (setf (cdr (assoc article-id nnttrss--headlines)) article)
    article))

(defun nnttrss--format-header (number group)
  "Return headline NUMBER in GROUP formated in nov format."
  (let* ((group-id (plist-get (cdr (assoc group nnttrss--feeds)) :id))
	 (article-id (nnttrss--get-article-id number group-id))
	 (article (nnttrss--find-article number group))
	 (size (length (plist-get article :content))))
    (if article
	(format "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%s\t%S\n"
		number
		(plist-get article :title)
		(url-host (url-generic-parse-url (plist-get article :link)))
		(format-time-string "%a, %d %b %Y %T %z"
				    (seconds-to-time (plist-get article :updated)))
		(format "<%d@%s.nnttrss>" article-id group-id)
		""
		size
		-1
		""
		nil))))

(defun nnttrss--read-vars (&rest vars)
  "Read VARS from local file in 'nnttrss-directory'.
Sets the variables VARS'."
  (dolist (var vars)
					;(setf (symbol-value var) nil)
    (let* ((name (symbol-name var))
	   (file (nnttrss-make-filename name))
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (coding-system-for-read mm-universal-coding-system))
      (when (file-exists-p file)
	(load file nil t t)))))

(defun nnttrss--write-vars (&rest vars)
  "Write VARS from memory to local file in 'nnttrss-directory'.
Assumes the variables VARS are set."
  (gnus-make-directory nnttrss-directory)
  (dolist (var vars)
    (let* ((name (symbol-name var))
	   (file (nnttrss-make-filename name))
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (coding-system-for-write mm-universal-coding-system))
      (with-temp-file (nnttrss-make-filename name)
	(insert (format ";; -*- coding: %s; -*-\n"
			mm-universal-coding-system))
	(let ((value (symbol-value var)))
	  (if (listp value)
	      (gnus-prin1 `(setq ,var ',value))
	    (gnus-prin1 `(setq ,var ,value))))
	(insert "\n")))))

(defun nnttrss-make-filename (name)
  "Build filename based on NAME in 'nnttrss-directory'."
  (expand-file-name
   (nnrss-translate-file-chars
    (concat name ".el"))
   nnttrss-directory))

(defun nnttrss--read-feeds ()
  "Read feeds file in 'nnttrss-directory'.
Sets the variable 'nnttrss--feeds."
  (nnttrss--read-vars 'nnttrss--feeds))

(defun nnttrss--write-feeds ()
  "Write feeds from memory to local file in 'nnttrss-directory'.
Assumes the variable 'nnttrss--feeds' is set."
  (nnttrss--write-vars 'nnttrss--feeds))

(defun nnttrss--update-feeds ()
  "Update 'nnttrss--feeds'."
  (let ((feeds (ttrss-get-feeds nnttrss-address
				nnttrss--sid
				:include_nested t
				:cat_id -4)))
    (setq nnttrss--feeds (mapcar (lambda (f) (cons (plist-get f :title) f))
				 feeds)))
  (nnttrss--write-feeds))

(defun nnttrss--feed-articles (feed-id)
  "Return list of article numbers corresponding to article IDs in FEED-ID."
  (let ((feed-article-map (lax-plist-get nnttrss--article-map feed-id)))
    (mapcar 'cdr feed-article-map)))

(defun nnttrss--read-article-map ()
  "Read articles mapping file in 'nnttrss-directory'.
Sets the variables 'nnttrss--article-map and
'nnttrss--last-article-id'."
  (nnttrss--read-vars 'nnttrss--article-map 'nnttrss--last-article-id))

(defun nnttrss--write-article-map ()
  "Write article map from memory to local file in 'nnttrss-directory'.
Assumes the variables 'nnttrss--article-map' and
'nnttrss--last-article-id' are set."
  (nnttrss--write-vars 'nnttrss--article-map 'nnttrss--last-article-id))

(defun nnttrss--update-single-article-map (article-id group)
  "Add ARTICLE-ID in GROUP to 'nnttrss--article-map'."
  (if (not (lax-plist-get nnttrss--article-map group))
      (setq nnttrss--article-map
	    (lax-plist-put nnttrss--article-map group `((,article-id . 1))))
    (let ((mapping (lax-plist-get nnttrss--article-map group)))
      (unless (assoc article-id mapping)
	(let* ((last-artno (cdar mapping))
	       (next-artno (+ 1 (or last-artno 0)))
	       (mapping (cons `(,article-id . ,next-artno) mapping)))
	  (setq nnttrss--article-map
		(lax-plist-put nnttrss--article-map group mapping)))))))

(defun nnttrss--update-article-map ()
  "Update 'nnttrss--article-map' with new articles in 'nnttrss--headlines'."
  (dolist (headline (mapcar 'cdr nnttrss--headlines))
    (let* ((article-id (plist-get headline :id))
	   (group (plist-get headline :feed_id)))
      (when (> article-id nnttrss--last-article-id)
	(nnttrss--update-single-article-map article-id group))))
  (setq nnttrss--last-article-id (apply 'max (mapcar 'car nnttrss--headlines)))
  (nnttrss--write-article-map))

(defun nnttrss--update-article-map-for-all ()
  "Update 'nnttrss--article-map' with new articles in 'nnttrss--headlines'."
  (dolist (headline (mapcar 'cdr nnttrss--headlines))
    (let* ((article-id (plist-get headline :id))
	   (group -4))
      (when (> article-id nnttrss--last-article-id)
	(nnttrss--update-single-article-map article-id -4))))
  (setq nnttrss--last-article-id (apply 'max (mapcar 'car nnttrss--headlines)))
  (nnttrss--write-article-map))

(defun nnttrss--get-article-number (article-id group)
  "Return article number corresponding to ARTICLE-ID in GROUP.
Note that ARTICLE-ID is an internal SQL identifier obtained from the API.
ARTICLE-NUMBER is the Gnus identifier."
  (cdr (assoc article-id (lax-plist-get nnttrss--article-map group))))

(defun nnttrss--get-article-id (article-number group)
  "Return article id corresponding to ARTICLE-NUMBER in GROUP.
Note that ARTICLE-ID is an internal SQL identifier obtained from the API.
ARTICLE-NUMBER is the Gnus identifier."
  (car (rassoc article-number (lax-plist-get nnttrss--article-map group))))

(defun nnttrss--read-headlines ()
  "Read headlines from local file in 'nnttrss-directory'.
Sets the variables 'nnttrss--headlines'."
  (nnttrss--read-vars 'nnttrss--headlines))

(defun nnttrss--write-headlines ()
  "Write headlines from memory to local file in 'nnttrss-directory'.
Assumes the variable 'nnttrss--headlines' is set."
  (nnttrss--write-vars 'nnttrss--headlines))

(defun nnttrss--update-headlines ()
  "Update 'nnttrss--headlines' since 'nnttrss--last-article-id'."
  (let* ((headlines (ttrss-get-headlines
		     nnttrss-address
		     nnttrss--sid
		     :feed_id -4
		     :limit -1
		     :since_id nnttrss--last-article-id
		     :show_content (not nnttrss-fetch-partial-articles))))
    (setq nnttrss--headlines (append nnttrss--headlines
				     (mapcar (lambda (h)
					       (cons (plist-get h :id) h))
					     headlines))))
  (nnttrss--write-headlines))

;;; Async machinery

(defun nnttrss--start-sync-timer ()
  "Start the background sync timer if `nnttrss-sync-interval' is set."
  (nnttrss--stop-sync-timer)
  (when nnttrss-sync-interval
    (setq nnttrss--sync-timer
          (run-with-timer nnttrss-sync-interval
                          nnttrss-sync-interval
                          #'nnttrss--sync-async))))

(defun nnttrss--stop-sync-timer ()
  "Stop the background sync timer."
  (when nnttrss--sync-timer
    (cancel-timer nnttrss--sync-timer)
    (setq nnttrss--sync-timer nil)))

(defun nnttrss--sync-async ()
  "Asynchronously sync feeds and headlines from server.
Updates local cache without blocking Emacs."
  (when (and nnttrss--sid (not nnttrss--sync-in-progress))
    (setq nnttrss--sync-in-progress t)
    (message "nnttrss: Syncing feeds...")
    (ttrss-get-feeds-async
     nnttrss-address
     nnttrss--sid
     #'nnttrss--sync-feeds-callback
     #'nnttrss--sync-error-callback
     :include_nested t
     :cat_id -4)))

(defun nnttrss--sync-feeds-callback (feeds)
  "Callback for async feeds fetch. FEEDS is the list of feed plists."
  (setq nnttrss--feeds (mapcar (lambda (f) (cons (plist-get f :title) f))
                               feeds))
  (nnttrss--write-feeds)
  (ttrss-get-headlines-async
   nnttrss-address
   nnttrss--sid
   #'nnttrss--sync-headlines-callback
   #'nnttrss--sync-error-callback
   :feed_id -4
   :limit -1
   :since_id nnttrss--last-article-id
   :show_content (not nnttrss-fetch-partial-articles)))

(defun nnttrss--sync-headlines-callback (headlines)
  "Callback for async headlines fetch. HEADLINES is the list of headline plists."
  (setq nnttrss--headlines (append nnttrss--headlines
                                   (mapcar (lambda (h)
                                             (cons (plist-get h :id) h))
                                           headlines)))
  (nnttrss--write-headlines)
  (nnttrss--update-article-map)
  (message "nnttrss: Updating marks...")
  (ttrss-get-headlines-async
   nnttrss-address
   nnttrss--sid
   #'nnttrss--sync-unread-callback
   #'nnttrss--sync-error-callback
   :feed_id -4
   :view_mode "unread"
   :limit -1))

(defun nnttrss--sync-unread-callback (unread-headlines)
  "Callback for unread articles fetch.  Updates unread marks in cache."
  (let ((unread-set (make-hash-table :test 'eq)))
    (dolist (h unread-headlines)
      (puthash (plist-get h :id) t unread-set))
    (dolist (entry nnttrss--headlines)
      (let ((article (cdr entry)))
        (plist-put article :unread (gethash (plist-get article :id) unread-set)))))
  (ttrss-get-headlines-async
   nnttrss-address
   nnttrss--sid
   #'nnttrss--sync-marked-callback
   #'nnttrss--sync-error-callback
   :feed_id -4
   :view_mode "marked"
   :limit -1))

(defun nnttrss--sync-marked-callback (marked-headlines)
  "Callback for marked articles fetch.  Updates starred marks in cache."
  (let ((marked-set (make-hash-table :test 'eq)))
    (dolist (h marked-headlines)
      (puthash (plist-get h :id) t marked-set))
    (dolist (entry nnttrss--headlines)
      (let ((article (cdr entry)))
        (plist-put article :marked (gethash (plist-get article :id) marked-set)))))
  (nnttrss--write-headlines)
  (setq nnttrss--sync-in-progress nil)
  (message "nnttrss: Sync complete (%d feeds, %d articles)"
           (length nnttrss--feeds)
           (length nnttrss--headlines)))

(defun nnttrss--sync-error-callback (error-msg)
  "Callback for sync errors.  ERROR-MSG is the error description.
Attempts re-login if session expired."
  (setq nnttrss--sync-in-progress nil)
  (if (string-match-p "not logged in\\|login\\|session" (downcase error-msg))
      (progn
        (message "nnttrss: Session expired, re-logging in...")
        (condition-case err
            (let ((sid (ttrss-login nnttrss-address nnttrss-user nnttrss-password)))
              (setq nnttrss--sid sid)
              (message "nnttrss: Re-login successful, retrying sync...")
              (nnttrss--sync-async))
          (error
           (message "nnttrss: Re-login failed - %s" (error-message-string err)))))
    (message "nnttrss: Sync failed - %s" error-msg)))

;;;###autoload
(defun nnttrss-sync ()
  "Manually trigger an asynchronous sync from TT-RSS server.
Updates feeds and headlines in the background without blocking Emacs.
After sync completes, press `g' in the Group buffer to see updates."
  (interactive)
  (if nnttrss--sync-in-progress
      (message "nnttrss: Sync already in progress")
    (if nnttrss--sid
        (nnttrss--sync-async)
      (message "nnttrss: Not connected to server"))))

(provide 'nnttrss)
;;; nnttrss.el ends here
