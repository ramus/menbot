(in-package :menbot)

(defvar *commands* (make-hash-table :test 'equalp))
(defvar *help-msgs* (make-hash-table :test 'equalp))

(defvar *connection*)
(defvar *src*)
(defvar *dest*)
(defvar *message*)

(defun msg-hook (message)
  (let* ((*connection* (connection message))
         (*src* (source message))
         ;; multiple dests are possible
         (*dest* (let ((loc (first (arguments message))))
                   (if (equal loc (nickname (user *connection*)))
                       *src*
                       loc)))
         (text (remove-if (lambda (c) (< (char-code c) 32))
                          (cadr (arguments message))))
         (command (first (split-sequence #\Space text :count 1)))
         (*message* (let ((pos (position #\Space text)))
                      (and pos (subseq text (1+ pos)))))
         (func (gethash command *commands*)))
    (when (and func (not (equal *src* *dest*))) ; not handling queries
      (let ((res (funcall func)))
        (when res
          (privmsg *connection* *dest* res))))))

(defun invite-hook (message)
  (let ((channel (first (last (arguments message)))))
    (client-log (connection message) message "INVITE:")
    (when (find channel *default-channels* :test #'string-equal)
      (join (connection message) channel))))

(defmacro defcommand (name args &body body)
  (let ((ref (format nil "~A~A" *prefix* (string-downcase (symbol-name name))))
        (rest (unless (find '&rest args) (gensym))))
    `(progn ,(when (and (stringp (first body)) (rest body))
                   `(setf (gethash ,ref *help-msgs*)
                          (format nil "~A~A" *prefix* ,(first body))))
            (setf (gethash ,ref *commands*)
                  (lambda ()
                    (handler-case
                        (destructuring-bind
                              ,(cond (rest (append args `(&rest ,rest)))
                                     (t args))
                            (split-sequence #\Space
                                            (remove-if (lambda (c)
                                                         (< (char-code c) 32))
                                                       *message*)
                                            :remove-empty-subseqs t)
                          ,(when rest `(declare (ignorable ,rest)))
                          ,@body)
                      (sb-kernel::arg-count-error () nil)))))))

#|(defmacro set-response (name func)
  `(setf (gethash (string ,name) *priv-hash*) ,func))|#

;;; Help
(defun get-help-string (command)
  (multiple-value-bind (key found)
      (gethash command *help-msgs*)
    (if found
        (substitute #\Space #\Newline (substitute #\^_ #\' (substitute #\^_ #\` key)))
        (format nil "Sorry, I don't have help for ~a yet..." command))))

(defcommand help (&optional command)
  "help arg - Show the help of `arg', or lists commands if `arg' is absent."
  (cond (command (get-help-string command))
        (t (format nil "~{~A~^ ~}" (reverse (hash-table-keys *commands*))))))

(defcommand version ()
  "version - Shows the bot's version. (Disclaimer: Said version is irrelevant.)"
  (let ((system (asdf:find-system "menbot")))
    (format nil "~A ~A"
            (asdf:component-version system)
            (asdf:system-description system))))

(defcommand todo ()  "Not implementing anything else. Fuck off.")

;; Return more results
(defvar *user-conts* (make-hash-table :test 'equal))

(defcommand more ()
  ",more - Gets the next result of your previous command, if applicable."
  (multiple-value-bind (cont found)
      (gethash *src* *user-conts*)
    (if found
        ; XXX Don't think I need the handler-case
        (handler-case
            (let ((res (funcall cont)))
              (if (stringp res)
                  res
                  (progn (setf (gethash *src* *user-conts*) res)
                         (funcall res))))
          (t () "Nothing left. (error?)"))
        "Nothing left.")))

;; body should return a closure, which is handled thereafter
#|(defmacro def-continuable-response (name args &body body)
  (let ((ref (string name)))
    `(progn ,(when (and (stringp (first body)) (rest body))
                   `(setf (gethash ,ref *help-msgs*)
                          (format nil "~A~A" *prefix* ,(first body))))
            (setf (gethash ,ref *priv-hash*)
                  (lambda ,args
                    (let ((cont ,@body))
                      ; Depending on arg being named src...
                      (setf (gethash src *user-conts*) cont)
                      (or (funcall cont) "Nothing here.")))))))|#
