;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

;;; facebook client (not comprehensive tested, may not work properly)

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

(defparameter *friend-page* "/friends/?id=")
(defparameter *friend-page-json*
"/ajax/typeahead/first_degree.php?__a=1&filter%5B0%5D=user&options%5B0%5D=friends_only&viewer=")
(defparameter *profile-page* "/profile.php?id=")

(defparameter *agent-id-scanner* (create-scanner "id=(\\d+)&amp;v=info"))

(defparameter *friend-list-scanner-json* 
  (create-scanner "\"payload\":{\"entries\":\\[(.*?)\\],[^\\]]*}}"
   :single-line-mode t))

(defparameter *friend-info-scanner-json* 
  (create-scanner 
   "{\"uid\":(\\d+),.*?\"text\":\"([^\"]*)\",.*?\"photo\":\"([^\"]*)\".*?}"
   :single-line-mode t))

(defparameter *friend-info-scanner* 
  (create-scanner 
   "<div class=\\\\\"ffriend [^>]*id=\\\\\"f(\\d+)\\\\\"[^>]*><a class=\\\\\"fpic.*?src=\\\\\"([^\"]*)\".*?alt=\\\\\"(.*?)\\\\\""
   :single-line-mode nil))

(defparameter *profile-data-scanner* 
  (create-scanner
   "<title>.*\\|\\s*(.*?)</title>(?:.*?<img class=\\\\?\"logo img\\\\?\" src=\\\\?\"([^\"]+)\")?"
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
  `(remove #\\ ,userpic-url))

(defclass sn-session-facebook (sn-session) 
  ((sn-x-agent-id :reader sn-x-agent-id
				  :initform nil)))

(defmethod sn-login ((network (eql :facebook.com)) &key user password)
  (let ((session (make-instance 'sn-session-facebook)))
	(with-mozilla
	  ;; at first we need to get some authorization secret in cookies
	  (sn-http-request session :head *root-url*)
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

(defmethod sn-extract-user-id ((session sn-session-facebook) page-url)
  (with-mozilla
	(let ((body (sn-http-request session :get page-url)))
	  (register-groups-bind (id-string)
		  (*agent-id-scanner* body)
		(when (not (null id-string))
		  (print id-string)
		  (parse-integer id-string))))))

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
	 (princ user-page)
	 (register-groups-bind (user-name userpic-url) ;cut off user data
						   (*profile-data-scanner* user-page)
						   (make-instance 'sn-user 
										  :sn-user-id user-id
										  :sn-user-name user-name
										  :sn-userpic-uri (ensure-userpic
														   userpic-url))))))

(defun facebook-get-friends-json (session)
  (let* ((result nil)
		 (agent-id (sn-x-agent-id session))
		 (friend-page 
		  (flexi-streams:octets-to-string
		   ;; don't know how this will work with huge (> 100) user lists
		   (sn-http-request session :get
							(user-page *friend-page-json* agent-id)
							:redirect nil)
		  :external-format :utf-8)))
	(register-groups-bind (friend-list) 
		(*friend-list-scanner-json* friend-page)
	  (when friend-list ; extract user data
		(do-register-groups ((#'parse-integer user-id) user-name userpic-url)
			(*friend-info-scanner-json* friend-list)
		  (when (not (= user-id agent-id))
			(push (make-instance 'sn-user
								 :sn-user-id user-id
								 :sn-user-name user-name
								 :sn-userpic-uri (ensure-userpic userpic-url))
				  result)))
		(nreverse result)))))

(defun facebook-get-friends-html (session user-id)
  (let ((result nil)
		(friend-page 
		 (sn-http-request session :get
						  (user-page *friend-page* user-id)
						  :redirect t)))
	(princ friend-page)
	(do-register-groups ((#'parse-integer user-id) userpic-url user-name)
		(*friend-info-scanner* friend-page)
	  (push (make-instance 'sn-user
						   :sn-user-id user-id
						   :sn-user-name user-name
						   :sn-userpic-uri (ensure-userpic userpic-url))
			result))
	(nreverse result)))

(defmethod sn-adjacent-users ((session sn-session-facebook) user-id)
  (with-mozilla
	(if (= user-id (sn-x-agent-id session))
		(facebook-get-friends-json session)
		(facebook-get-friends-html session user-id))))