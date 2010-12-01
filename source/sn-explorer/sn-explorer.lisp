;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-user)

(defvar *sn-dot-program* "neato")

(defvar *path-sentinel*
  #+(or ccl abcl) "\"~a\""
  #+(or sbcl clisp) "~a")

(defmacro protect (path)
  `(format nil *path-sentinel* ,path))
           
(defun chdir (dir)
  #+sbcl (sb-posix:chdir dir)
  #+clisp (ext:cd dir)
  #+ccl (ccl::%chdir (namestring dir))
  #-(or sbcl clisp ccl) (error "chidir does not implemented"))

(defun sn-dot->image (dot-file-path image-type &optional parameters)
  (let ((dot-dir (make-pathname :name nil :type nil
								:defaults dot-file-path))
		(image-file-path (make-pathname :type image-type
										:defaults dot-file-path))
		(working-dir (truename ".")))
    (chdir dot-dir)
    (multiple-value-bind (status code)
		(run *sn-dot-program* 
			 (append parameters
					 (list (concatenate 'string "-T" image-type) 
						   (protect (namestring dot-file-path)) "-o"
						   (protect (namestring image-file-path)))))
	  (declare (ignore status code))
	  (chdir working-dir)
	  (make-pathname :directory nil :device nil :defaults image-file-path))))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun assocv (key alist)
  (cadr (assoc key alist)))

(defun collect-user-data ()
  (let ((data ()))
    (flet ((ask-user (key question &optional default numeric)
	     (let ((answer (prompt-read question)))
	       (let ((value 
		      (if (zerop (length answer)) 
			  default 
			  (if numeric
			      (parse-integer answer)
			      answer))))
		 (push (list key value) data)))))
      (ask-user :agent-email "Agent e-mail" "")
      (ask-user :agent-password "Agent password" "")
      (ask-user :target-dir "Target directory (current is default)" nil) 
      (ask-user :root-user-id "Root user id (agent id is default)" nil t)
      (ask-user :depth "Depth (1 is default)" 1 t)
      (ask-user :delay "Delay (0 is default)" 0 t))
    (append data '((:verbose t) (:loag-graphics t)))))

;; get agent data and target user data from command line
(defun sn-explore-interactive (network &optional data)
  (let* ((data (if (null data) (collect-user-data) data))
		 (verbose (assocv :verbose data)))
    (format verbose "Logging in...~%")
    (let ((session (sn-login network
							 :user (assocv :agent-email data)
							 :password (assocv :agent-password data))))
      (if session
		  (let* ((user-id (assocv :root-user-id data))
				 (target-dir (assocv :target-dir data))
				 (target-dir (if (null target-dir) (truename ".") target-dir)))
			(format verbose "Login OK~%")
			(format verbose "Working...~%")
			(finish-output)
			(let ((user-pool (sn-crawl-users 
							  session 
							  (if (null user-id) (sn-agent-id session) user-id)
							  (assocv :depth data)
							  :delay (assocv :delay data))))
			  (prog1 
				  (sn-print-graph user-pool session target-dir 
								  :load-graphics (assocv :load-graphics data))
				(format verbose "Done~%"))))
		  (progn
			(format verbose "Login failed~%")
			nil)))))

;; use sn-explorer as a library
(defun sn-explore (network user password &key target-dir root-user
				   load-graphics (verbose nil) (depth 1) (delay 0))
  (let ((data `((:target-dir ,target-dir)
				(:agent-email ,user)
				(:agent-password ,password)
				(:root-user-id ,root-user)
				(:depth ,depth)
				(:delay ,delay)
				(:verbose ,verbose)
				(:load-graphics ,load-graphics))))
	(sn-explore-interactive network data)))