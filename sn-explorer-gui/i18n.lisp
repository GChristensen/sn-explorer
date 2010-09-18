;; This file is part of sn-explorer-gui library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-gui)

(defparameter *lang->string-hash* (make-hash-table :test 'equal))

(defvar *lang-key*)

(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun interpret-string (v1 v2)
  (if (eq 'file v1) 
	  (file-string 
	   (merge-pathnames (pathname v2)
						*compile-file-truename*))
	  (if (not (null v2))
		  (octets-to-string v1 :external-format v2)
		  v1)))
							
(defmacro i18n-def-lang (key &body body)
  (let ((gkey (gensym))
		(ghash (gensym)))
	`(let ((,gkey ,key)
		   (,ghash (make-hash-table)))
	   (setf (gethash ,gkey *lang->string-hash*) ,ghash)
	   ,@(loop for (k s p) in body
			collect `(setf (gethash ',k ,ghash) ,(interpret-string s p))))))
						   
(defun i18n (key)
  (gethash key (gethash *lang-key* *lang->string-hash*)))

(defun get-useragent-language ()
  (let ((accept-language (header-in* "Accept-Language")))
	(if (search "ru" accept-language) "ru" "en")))

(defmacro with-language (key &body body)
  `(let ((*lang-key* ,key))
	 ,@body))

(defmacro with-useragent-language (&body body)
  `(with-language (get-useragent-language)
	 ,@body))  

(defmacro with-system-language (&body body)
  `(with-language (get-system-language)
	 (let ((*default-foreign-encoding* :utf-16))
	   ,@body)))

(set-dispatch-macro-character
  #\# #\$
  #'(lambda (stream subchar arg)
	  (declare (ignore subchar arg))
	  (let ((sexp (read stream t)))
		`(i18n ',sexp))))