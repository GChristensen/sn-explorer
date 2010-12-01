;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

;;; sn-explorer client base package

(in-package :sn-explorer-client)

(defvar *supported-social-networks* nil)

;; useragent string
(defparameter *user-agent* "sn-explorer (0.1)")

(defvar *sn-http-proxy* nil)

(defclass sn-session ()
  ;; client should not rely on content of these fields
  ((sn-stream :reader sn-stream ; a flexy-stream when stream reuse is on
	      :initform nil)
   (sn-cookies :reader sn-cookies ; drakma cookie jar
	       :initform (make-instance 'cookie-jar))
   ;; allow stream reuse between sn-http-request calls
   (sn-stream-reusable-p :reader sn-stream-reusable-p 
			 :initform nil))
  (:documentation "Class to keep session specific data"))

(defclass sn-user ()
  ((sn-user-id :reader sn-user-id ; user ID in the social network 
			                      ; (a number or a string)
	       :initform (error "Must specify an user ID.")
	       :initarg :sn-user-id)
   (sn-user-name :reader sn-user-name
		 :initform (error "Must specify an user name.")
		 :initarg :sn-user-name)
   (sn-userpic-uri :reader sn-userpic-uri
		   :initarg :sn-userpic-uri
		   :initform nil)
   (sn-user-data :accessor sn-user-data
		 :initarg :sn-user-data
		 :initform nil))
  (:documentation "Class for SN user info storage"))

;; accessors for arbitrary data stored in user data-object
(defmethod sn-user-set-data ((user sn-user) key data)
  (let ((cell (assoc key (sn-user-data user))))
    (if cell
	(rplacd cell data)
	(push (cons key data)
	      (slot-value user 'sn-user-data)))))

(defmethod sn-user-get-data ((user sn-user) key)
  (let ((cell (assoc key (sn-user-data user))))
    (if cell
	(values (cdr cell) t)
	(values nil nil))))

(defmethod sn-user-rem-data ((user sn-user) key)
  (remove key (sn-user-data user) :key #'car))

;; light wrapper on drakma:http-request
(defun sn-http-request (session method url &key parameters headers 
						(redirect t))
  (let* ((stream-is-reusable (sn-stream-reusable-p session))
		 (stream (when stream-is-reusable (sn-stream session))))
    (let ((stream (when (and stream (open-stream-p stream)) 
					stream)))
      (multiple-value-bind (body status-code headers uri stream1 must-close)
		  (http-request url
						:method method
						:parameters parameters
						:additional-headers headers
						:proxy *sn-http-proxy*
						:user-agent *user-agent*
						:cookie-jar (sn-cookies session)
						:redirect redirect
						:redirect-methods '(:get :post :head)
						:stream stream
						:close (not stream-is-reusable))
		(declare (ignore uri must-close))
		(when stream-is-reusable
		  (setf (slot-value session 'sn-stream) stream1))
		(values body headers status-code)))))

;; allow connection reuse between sn-http-request calls
;; and enshure to forbid this when finished
(defmacro sn-with-reusable-connection ((session) &body body)
  (let ((gnested (gensym)))
    `(let ((,gnested (sn-stream-reusable-p ,session)))
       (if ,gnested
	   (warn "Nested reusable connection macros detected")
	   (setf (slot-value ,session 'sn-stream-reusable-p) t))
       (unwind-protect (progn ,@body)
	 ;; cleanup
	 (unless ,gnested
	   (setf (slot-value ,session 'sn-stream-reusable-p) nil)
	   (close (sn-stream ,session))
	   (setf (slot-value ,session 'sn-stream) nil))))))

(defun sn-extract-filetype (filename)
   (subseq filename
	   (1+ (search "." filename :from-end t))))

(defmethod sn-mk-userfile-path ((user sn-user) type directory)
  (make-pathname :name (princ-to-string (sn-user-id user))
		 :type type
		 :defaults directory))

#-sn-web-deployment
(defmethod sn-load-userpic ((user sn-user) session directory &key overwrite)
  "Loads userpic for given sn-user instance and places it into
specified directory under <user-id>.<original-ext> name.
Returns downloaded image pathname."
  (let ((userpic-uri (sn-userpic-uri user)))
    (when userpic-uri
      (let ((userpic-pathname 
			 (sn-mk-userfile-path user 
								  (sn-extract-filetype userpic-uri) 
								  directory)))
		(if (or overwrite (not (probe-file userpic-pathname)))
			(let ((picture-data (sn-http-request session :get userpic-uri)))
			  (with-open-file (picture-file 
							   userpic-pathname
							   :direction :output
							   :element-type '(unsigned-byte 8)
							   :if-exists (when overwrite :supersede))
				(write-sequence picture-data picture-file)
				(file-namestring userpic-pathname)))
			(file-namestring userpic-pathname))))))

#+sn-web-deployment
(defmethod sn-load-userpic ((user sn-user) session directory &key overwrite)
  "Simply returns userpic uri."
  (sn-userpic-uri user))

(defgeneric sn-login (network &key)
  (:documentation "Generic method to enter a social network.
Returns fresh SN session."))

(defgeneric sn-get-session-cookie (session name))

(defmethod sn-get-session-cookie ((session sn-session) name)
  (let ((cookies (cookie-jar-cookies (sn-cookies session))))
	(do ((c cookies (cdr cookies)))
		((or (null c) (string= (cookie-name (car c)) name)) (car c)))))
  
(defgeneric sn-extract-user-id (session page-url)
  (:documentation "Extracts user id from user's page content."))

(defgeneric sn-logout (session)
  (:documentation "Generic method to logout from the social network."))

(defgeneric sn-agent-id (session)
  (:documentation "Retrieve currently logged-on user ID."))

(defgeneric sn-agent-name (session)
  (:documentation "Retrieve currently logged-on user name."))

(defgeneric sn-user-link (session user-id)
  (:documentation "Returns an URL to the user profile page"))

(defgeneric sn-id->string (session user-id)
  (:documentation "Transform raw SN user id to string"))

(defgeneric sn-instantiate-user (session user-id)
  (:documentation "Instantiates SN user by the given ID."))

(defgeneric sn-adjacent-users (session user-id)
  (:documentation "Retrieve directly related users (friends) as list for the user deisgnated by given user-id."))

;; client only should perform actions specific to his sn-session descendant
(defmethod sn-logout ((session sn-session))
  (setf (cookie-jar-cookies (sn-cookies session)) nil)
  (let ((stream (sn-stream session)))
    (when stream
      (close stream)
      (setf (slot-value session 'sn-stream) nil)))
    t)