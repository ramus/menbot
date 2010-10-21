;;;; Commands for various Google APIs
(in-package :menbot)

(setf drakma:*drakma-default-external-format* ':utf-8)
(defvar *referer* (concatenate 'string "irc://" *default-server* "/" *nickname*))
(defvar *failure* "Didn't find anything...")

;;; Google Search
(defun google-request (query &key (start 0))
  (drakma:http-request "http://ajax.googleapis.com/ajax/services/search/web"
                       :parameters `(("q" . ,query)
                                     ("v" . "1.0")
                                     ("rsz" . "large")
                                     ("start" . ,(write-to-string start)))
                       :additional-headers `(("Referer" . ,*referer*))))

(defun google-search (text &key (start 0))
  (let* ((request (json:decode-json-from-string
                   (google-request text :start start)))
         (response-data (cdr (assoc :response-data request)))
         (results (cdr (assoc :results response-data)))
         (cursor (cdr (assoc :cursor response-data))))
    (lambda ()
      (let* ((result (pop results))
             (title (html-entities:decode-entities
                     (cdr (assoc :title-no-formatting result))))
             (link (cdr (assoc :unescaped-url result))))
        (cond (result (format nil "~A - ~A" title link))
              (t (when (assoc :pages cursor)
                   (google-search text :start (+ start 8)))))))))

(defcommand g (&rest query)
  "g query ... - Returns Google search results for `query'."
  (when query
    (let ((cont (google-search (format nil "~{~A~^ ~}" query))))
      (setf (gethash *src* *user-conts*) cont)
      (or (funcall cont) *failure*))))

;;; Google Image Search
(defun google-image-request (query &key (start 0))
  (drakma:http-request "http://ajax.googleapis.com/ajax/services/search/images"
                       :parameters `(("q" . ,query)
                                     ("v" . "1.0")
                                     ("rsz" . "large")
                                     ("start" . ,(write-to-string start))
                                     ("safe" . "off"))
                       :additional-headers `(("Referer" . ,*referer*))))

(defun google-image-search (text &key (start 0))
  (let* ((request (json:decode-json-from-string
                   (google-image-request text :start start)))
         (response-data (cdr (assoc :response-data request)))
         (results (cdr (assoc :results response-data)))
         (cursor (cdr (assoc :cursor response-data))))
    (lambda ()
      (let* ((result (pop results))
             (title (html-entities:decode-entities
                     (cdr (assoc :title-no-formatting result))))
             (link (cdr (assoc :unescaped-url result))))
        (cond (result (format nil "~A - ~A" title link))
              (t (when (assoc :pages cursor)
                   (google-search text :start (+ start 8)))))))))

(defcommand gi (&rest query)
  "gi query ... - Returns Google Image search results for `query'."
  (when query
    (let ((cont (google-image-search (format nil "~{~A~^ ~}" query))))
      (setf (gethash *src* *user-conts*) cont)
      (or (funcall cont) *failure*))))

;;; Google Translate
(defun translate-request (src dst text)
  (drakma:http-request "http://ajax.googleapis.com/ajax/services/language/translate"
                       :parameters `(("q" . ,text)
                                     ("langpair" .
                                      ,(format nil "~A|~A"
                                               (if (string= src "auto") "" src)
                                               dst))
                                     ("v" . "1.0"))
                       :additional-headers `(("Referer" . ,*referer*))))

(defun google-translate (src dst text)
  (let* ((res (json:decode-json-from-string (translate-request src dst text)))
         (response-data (cdr (assoc :response-data res)))
         (translated-text (cdr (assoc :translated-text response-data))))
    (when translated-text
      (format nil "Translation - ~A" translated-text))))

(defcommand trans (src dest &rest text)
  "trans src dest text ... - Translates `text' from `src' to `dest' (as
two-letter codes) using the Google Translate API."
  (or (google-translate src dest
                        (format nil "~{~A~^ ~}" text))
      "No clue, pal."))

;;; Youtube API
(defun youtube-request (query)
  (drakma:http-request "http://gdata.youtube.com/feeds/api/videos"
                       :parameters (list (cons "q" query)
                                         (cons "v" "1.0"))
                       :additional-headers `(("Referer" . ,*referer*))))

(defun youtube-search (query)
  (klacks:with-open-source (source (cxml:make-source (youtube-request query)))
    (lambda ()
      (when (klacks:find-element source "entry")
        (let ((title (progn (klacks:find-element source "title")
                            (klacks:peek-next source)
                            (klacks:consume-characters source)))
              (link (progn (klacks:find-element source "link")
                           (let ((url (klacks:get-attribute source "href")))
                             (first (split-sequence #\& url))))))
          (format nil "~A - ~A" title link))))))

(defcommand yt (&rest query)
  "yt query ... - Returns YouTube search results for `query'."
  (when query
    (let ((cont (youtube-search (format nil "~{~A~^ ~}" query))))
      (setf (gethash *src* *user-conts*) cont)
      (or (funcall cont) *failure*))))
