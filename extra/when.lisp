(in-package :menbot)

(defvar *events* (make-hash-table :test 'equal))
(defvar *timers* (make-hash-table :test 'equal))
(defvar *event-search* (make-hash-table :test 'equal))

(setf local-time:*default-timezone* local-time:+utc-zone+)

(defun events-db (channel)
  (merge-pathnames (format nil "events-~A.db" channel) *confdir*))

;; Pretty printing time durations
(defun get-remaining-time (seconds)
  (multiple-value-bind (minutes seconds) (floor seconds 60)
    (multiple-value-bind (hours minutes) (floor minutes 60)
      (multiple-value-bind (days hours) (floor hours 24)
        (list days hours minutes seconds)))))

(defun print-remaining-time (event seconds)
  (apply #'format nil
         "~A is in~#[ now~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^,~}~]."
         (append (list event)
                 (remove-if-not #'identity
                                (mapcar (lambda (x y) (when (and (not (zerop x)) x)
                                                        (format nil "~A ~A~P" x y x)))
                                        (get-remaining-time seconds)
                                        '("day" "hour" "minute" "second"))))))

;; Event timers
(defun add-event-timer (event channel time)
  (let ((timers (gethash channel *timers*))
        (timer-connection *connection*))
    (when (gethash event timers)
      (remove-event-timer event channel))
    (let ((timer (sb-ext:make-timer
                  (lambda ()
                    (privmsg timer-connection channel
                             (format nil "~A is occurring now." event))
                    (remove-event event channel))
                  :name (format nil "timer-~A" event))))
      (setf (gethash event timers) timer)
      (sb-ext:schedule-timer timer time :absolute-p t))))

(defun remove-event-timer (event channel)
  (let* ((timers (gethash channel *timers*))
         (timer (gethash event timers)))
    (when timer
      (sb-ext:unschedule-timer timer)
      (remhash event timers))))

;; Events
(defun add-event (event channel time)
  (let ((events (gethash channel *events*)))
    (setf (gethash event events) time)
    (cl-store:store events (events-db channel))
    (add-event-timer event channel time)
    (bk-tree:insert-value event (gethash channel *event-search*))))

(defun remove-event (event channel)
  (let ((events (gethash channel *events*)))
    (remhash event events)
    (cl-store:store events (events-db channel))
    (remove-event-timer event channel)
    (let ((tree (make-instance 'bk-tree:bk-tree)))
      (mapc (lambda (word) (bk-tree:insert-value word tree))
            (alexandria:hash-table-keys (gethash channel *events*)))
      (setf (gethash channel *event-search*) tree))))

(defun get-event (event channel)
  (let ((events (gethash channel *events*)))
    (gethash event events)))

(defun get-random-event (channel)
  (let ((events (gethash channel *events*)))
    (unless (zerop (hash-table-count events))
      (let ((event (random-elt (hash-table-keys events))))
        (values event (gethash event events))))))

(defun search-event (event channel)
  (let ((tree (gethash channel *event-search*)))
    (mapcar #'bk-tree:value-of
            (bk-tree:search-value event tree
                                  :threshold (floor (/ (length event) 2))
                                  :ordered-results t))))

;; Database initialization
(defun when-join-hook (message)
  (when (self-message-p message)
    (let* ((channel (first (arguments message)))
           (*connection* (connection message)))
      (unless (gethash channel *events*)
        (setf (gethash channel *events*)
              (if (probe-file (events-db channel))
                  (cl-store:restore (events-db channel))
                  (make-hash-table :test 'equalp)))
        (setf (gethash channel *timers*) (make-hash-table :test 'equalp))
        (loop for k being the hash-keys of (gethash channel *events*)
           using (hash-value v)
           do (add-event-timer k channel v))
        (let ((tree (make-instance 'bk-tree:bk-tree)))
          (mapc (lambda (word) (bk-tree:insert-value word tree))
                (alexandria:hash-table-keys (gethash channel *events*)))
          (setf (gethash channel *event-search*) tree))))))

;; Commands
(defcommand when (&optional event &rest time)
  "when event [time] - Returns the duration until `event'. If `time' is
provided, creates the `event' occuring at `time' (UTC)."
  (cond (time
         (let ((time (ignore-errors (chronicity:parse (format nil "~{~A~^ ~}" time)))))
           (cond (time
                  (add-event event *dest* (local-time:timestamp-to-universal time))
                  "Set.")
                 (t "Bad timestring."))))
        (event
         (let ((time (get-event event *dest*)))
           (if time
               (let ((remaining (- time (get-universal-time))))
                 (print-remaining-time event remaining))
               (let ((matches (search-event event channel)))
                 (format nil "Nothin' here. Did you mean ~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}?"
                         matches)
                 "Never."))))
        (t (multiple-value-bind (event time) (get-random-event *dest*)
             (when time
               (print-remaining-time event
                                     (- time (get-universal-time))))))))

(defcommand whens ()
  "whens - Returns the next five upcoming events."
  (let* ((events (gethash *dest* *events*))
         (sorted (sort (alexandria:hash-table-alist events) #'< :key #'cdr)))
    (format nil"~{~A~^ ~}" (mapcar #'(lambda (event)
                                       (print-remaining-time (car event)
                                                             (- (cdr event)
                                                                (get-universal-time))))
                                   (subseq sorted 0 5)))))
