(in-package :menbot)

(defvar *nickname* "menbot")
(defvar *username* "menbot")
(defvar *realname* "menbot")

(defvar *default-server* nil)
(defvar *default-channels* '())

(defvar *confdir* (asdf:system-source-directory 'menbot))

(defvar *identify* nil)

(defvar *prefix* ",")
