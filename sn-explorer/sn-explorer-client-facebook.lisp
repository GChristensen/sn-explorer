;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

;;; facebook client (unfinished)

(defpackage :sn-explorer-client-facebook
  (:use :common-lisp :cl-ppcre :drakma :sn-explorer-client))

(in-package :sn-explorer-client-facebook)

(push :facebook.com *supported-social-networks*)

(defparameter *mozilla-agent-str*
  "Mozilla/5.0 (U; U; Unknown; en-US; rv:0.0.0.0) Gecko/00000000 Unknown Firefox/0.0.0")

(defparameter *root-url* "http://facebook.com")

(defparameter *login-url* "https://login.facebook.com/login.php")
(defparameter *login-field* "email")
(defparameter *passw-field* "pass")

(defparameter *friend-page* "/friends.php?id=")
(defparameter *profile-page* "/profile.php?id=")

(defparameter *friend-page-marker* "friendsFilter")

(defparameter *friend-list-scanner* 
  (create-scanner "friendsData.*'friends':\\[(\\[.*?\\])\\]"
   :single-line-mode t))

(defparameter *friend-info-scanner* 
  (create-scanner 
   "\\[(\\d+),\\s*\"([^\"]*)\",\\s*\"([^\"]*)\".*?\]"
   :extended-mode t
   :single-line-mode t))

(defparameter *profile-data-scanner* 
  (create-scanner
   "<title>.*\\|\\s*(.*?)</title>(?:.*?profileimage.*?<img class=\"logo img\" src=\"(.*?)\")?"
   :extended-mode nil
   :single-line-mode t
   :case-insensitive-mode t))

(defmacro with-mozilla (&body body)
  `(let ((*user-agent* *mozilla-agent-str*))
	 ,@body))

(defmacro user-page (page-name user-id)
  `(concatenate 'string
		*root-url*
		,page-name
		(write-to-string ,user-id)))

;; if user does not provide userpic, put nothing
(defmacro ensure-userpic (userpic-url)
  `(when (not (search "question"
					  ,userpic-url))
     (remove #\\ ,userpic-url)))

(defclass sn-session-facebook (sn-session) 
  ((sn-x-agent-id :reader sn-x-agent-id
				  :initform nil)))

(defmethod sn-login ((network (eql :facebook.com)) &key user password)
  (let ((session (make-instance 'sn-session-facebook)))
	(with-mozilla
	  ;; at first we need to get some authorization secret in cookies
	  (sn-http-request session ; get cookies
					   :post
					   *login-url*
					   :parameters `((,*login-field* . ,user)
									 (,*passw-field* . ,password)))
	  ;; login with proper cookies should succeed then
	  (sn-http-request session ; get cookies
					   :post
					   *login-url*
					   :parameters `((,*login-field* . ,user)
									 (,*passw-field* . ,password)))
      ;; check whether login succeeded
	  (register-groups-bind (id-string)
		  ("(\\d+)" (cookie-value (sn-get-session-cookie session "c_user")))
		(when (not (null id-string))
		  (setf (slot-value session 'sn-x-agent-id)
				(parse-integer id-string))))
      (when (sn-x-agent-id session)
		session))))

(defmethod sn-agent-id ((session sn-session-facebook))
  (sn-x-agent-id session))

(defmethod sn-user-link ((session sn-session-facebook) user-id)
  (user-page *profile-page* user-id))

(defmethod sn-id->string ((session sn-session-facebook) user-id)
  (princ-to-string user-id))

(defmethod sn-instantiate-user ((session sn-session-facebook) user-id)
  (with-mozilla
   (multiple-value-bind (user-page headers status-code)
	   (sn-http-request session ; get user profile page body
						:get
						(user-page *profile-page* user-id))
	 (declare (ignore headers status-code))
	 (register-groups-bind (user-name userpic-url) ;cut off user data
						   (*profile-data-scanner* user-page)
						   (make-instance 'sn-user 
										  :sn-user-id user-id
										  :sn-user-name user-name
										  :sn-userpic-uri (ensure-userpic
														   userpic-url))))))

(defmethod sn-adjacent-users ((session sn-session-facebook) user-id)
  #|(let ((result nil)
	(friend-page (sn-http-request session
				      :get
				      (user-page *friend-page* user-id)
				      :redirect nil)))
    ;; check if recieved page is actually friend page
    (when (search *friend-page-marker* friend-page)
      ;; extract friend list from the javascript
      (register-groups-bind (friend-list) 
	  (*friend-list-scanner* friend-page)
	(when friend-list ; extract user data
	  (do-register-groups ((#'parse-integer user-id) user-name userpic-url)
	      (*friend-info-scanner* friend-list)
	    (push (make-instance 'sn-user
				 :sn-user-id user-id
				 :sn-user-name user-name
				 :sn-userpic-uri (ensure-userpic userpic-url))
		  result))
	  (nreverse result)))))|#)