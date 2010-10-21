(in-package :menbot)

;; Keeps notes of random stuff
(defvar *notes-db* (make-hash-table :test 'equal))
(defvar *notes-search* (make-hash-table :test 'equal))

(defun notes-db (channel)
  (merge-pathnames (format nil "notes-~A.db" channel) *confdir*))

;; Restore or create notes db upon joining a channel
(defun notes-join-hook (message)
  (when (self-message-p message)
    (let ((channel (first (arguments message))))
      (unless (gethash channel *notes-db*)
        (setf (gethash channel *notes-db*)
              (if (probe-file (notes-db channel))
                  (cl-store:restore (notes-db channel))
                  (make-hash-table :test 'equal))))
      (unless (gethash channel *notes-search*)
        (let ((tree (make-instance 'bk-tree:bk-tree)))
          (mapc (lambda (word) (bk-tree:insert-value word tree))
                (hash-table-keys (gethash channel *notes-db*)))
          (setf (gethash channel *notes-search*) tree))))))

(defun add-note (topic channel note)
  (let ((notes (gethash channel *notes-db*)))
    (setf (gethash topic notes) note)
    (cl-store:store notes (notes-db channel))
    (handler-case (bk-tree:insert-value topic (gethash channel *notes-search*))
      (bk-tree:duplicate-value () t))))

(defun remove-note (topic channel)
  (let ((notes (gethash channel *notes-db*)))
    (remhash topic notes)
    (cl-store:store notes (notes-db channel))
    (let ((tree (make-instance 'bk-tree:bk-tree)))
      (mapc (lambda (word) (bk-tree:insert-value word tree))
            (hash-table-keys (gethash channel *notes-db*)))
      (setf (gethash channel *notes-search*) tree))))

(defun get-note (topic channel)
  (let ((notes (gethash channel *notes-db*)))
    (gethash topic notes)))

(defun get-random-note (channel)
  (let ((notes (gethash channel *notes-db*)))
    (unless (zerop (hash-table-count notes))
      (let ((topic (random-elt (hash-table-keys notes))))
        (values (gethash topic notes) topic)))))

(defun search-note (topic channel)
  (let ((tree (gethash channel *notes-search*)))
    (mapcar #'bk-tree:value-of
            (bk-tree:search-value topic tree
                                  :threshold (floor (/ (length topic) 2))
                                  :ordered-results t))))

(defcommand note (&optional topic &rest note)
  "note arg [rest ...] - Retrieve the note with `arg'. If `rest' is provided,
creates a note with `arg'. If neither is provided, a random note is retrieved."
  (cond (note (add-note topic *dest* note) "Noted.")
        (topic
         (let ((note (get-note topic *dest*)))
           (if note
               (format nil "~A ~{~A~^ ~}" topic note)
               (let ((matches (search-note topic *dest*)))
                 (if matches
                     (format nil "Nothin' here. Did you mean ~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}?"
                             matches)
                     "Nothin' here.")))))
        (t (multiple-value-bind (note topic) (get-random-note *dest*)
             (format nil "~A ~{~A~^ ~}"  topic note)))))

(defcommand unnote (topic)
  "unnote arg - Remove the note with `arg'."
  (remove-note topic *dest*)
  "Forgotten.")
