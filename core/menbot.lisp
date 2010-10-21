(in-package :menbot)

(defclass menbot ()
  ((connection :initform nil :accessor connection)
   (handler-thread :initform nil :accessor handler-thread)
   (nickname :initform *nickname* :initarg :nickname :accessor nickname)
   (username :initform *username* :initarg :username :accessor username)
   (realname :initform *realname* :initarg :realname :accessor realname)
   (server :initform *default-server* :initarg :server :accessor server)
   (channels :initform *default-channels* :initarg :channels :accessor channels)
   (commands :initform (make-hash-table :test 'equalp) :accessor commands)))

;; This won't work on all servers, obviously
(defun identify (bot password)
  (privmsg (connection bot) "NickServ" (format nil "identify ~A" password)))

;;; Split messages if they are too long
(defvar *max-msg-length* 420) ;; This needs to be handled better
;; TODO: Rate limiting
(defmethod privmsg :around ((connection connection) (target string) (message string))
  (cond ((> (length message) *max-msg-length*)
         (let ((pos (or (position #\Space message :from-end t :end *max-msg-length*)
                        *max-msg-length*)))
           (call-next-method connection target (subseq message 0 pos))
           (privmsg connection target (subseq message (1+ pos)))))
        (t (call-next-method))))

;;; Starting and stopping
(defun start-bot (bot)
  (setf (connection bot) (connect :nickname (nickname bot)
                                  :username (username bot)
                                  :realname (realname bot)
                                  :server (server bot)))
  (setf (client-stream (connection bot)) (make-broadcast-stream))
  (when *identify* (identify bot *identify*))
  (add-hook (connection bot) 'irc-privmsg-message 'msg-hook)
  (add-hook (connection bot) 'irc-invite-message 'invite-hook)
  ;; Those probably should be added by some other means
  (add-hook (connection bot) 'irc-join-message 'notes-join-hook)
  (add-hook (connection bot) 'irc-join-message 'when-join-hook)
  (cl-irc::multi-join (connection bot) (channels bot))
  (setf (handler-thread bot)
        (sb-thread:make-thread
         (lambda ()
           (handler-bind
               ((no-such-reply
                 (lambda (c) (declare (ignore c)) (invoke-restart 'continue))))
             (read-message-loop (connection bot))))
         :name "handler-thread")))

(defun stop-bot (bot)
  (quit (connection bot))
  ;; Those should be someplace else
  (dolist (c (hash-table-keys *timers*))
    (mapc #'sb-ext:unschedule-timer (hash-table-values (gethash c *timers*))))
  (setf *events* (make-hash-table :test 'equal))
  (setf *timers* (make-hash-table :test 'equal))
  (sb-thread:destroy-thread (handler-thread bot)))

(defun restart-bot (bot)
  (stop-bot bot)
  (start-bot bot))
