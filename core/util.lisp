(in-package :menbot)

;;; Various irc utilities

(defgeneric user-list (connection channel))

(defmethod user-list ((connection connection) (channel channel))
  (loop for key being the hash-keys of (users channel) using (hash-value value)
       collect (nickname value)))

(defmethod user-list ((connection connection) (channel string))
  (user-list connection (find-channel connection channel)))

;;; IRC formatting
(declaim (inline bold color underline action))

(defun bold (text) (format nil "~C~A~C" #\^B text #\^B))

(defun color (text fg &optional bg)
  (let ((colors #(white black blue green light-red red magenta orange yellow
                  light-green cyan light-cyan light-blue light-magenta gray
                  light-gray)))
    (format nil "~C~2,'0D~@[,~2,'0D~]~A~C"
            #\^C (position fg colors) (position bg colors) text #\^C)))

(defun underline (text) (format nil "~C~A~C" #\^_ text #\^_))

(defun action (text) (format nil "~CACTION ~A~C" #\^A text #\^A))
