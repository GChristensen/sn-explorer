;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-graph)

(defparameter *dot-header*
  "graph \"graph\"
{
  overlap=false
  layers=\"edges:nodes\"
  node [shape=plaintext, fontname=Arial, fontsize=8, fontcolor=blue, layer=nodes]
  edge [layer=edges]~%")

(defparameter *dot-footer* "}")

(defvar *print-photos*)
  
(defparameter *node-stub-photo* 
  "  n~a [
    label=<
      <table border=\"0\">
        <tr>~a</tr>
        <tr><td>~a</td></tr>
      </table>>,
    URL=\"~a\",
    target=\"_blank\"
    tooltip=\"~a\"]~%")

(defparameter *node-stub-plain* 
  "  n~a [
    label=\"~a\",
    URL=\"~a\",
    tooltip=\"~a\"]~%")

(defparameter *graphical-cell-stub* "<td><img src=\"~a\"/></td>")
(defparameter *plain-cell-stub* "<td border=\"1\">No image</td>")

(defparameter *edge-stub* "  n~a -- n~a [color=~a]~%")

(defvar *color-list*
  (list
   "mediumturquoise"
   "black"
   "coral"
   "brown"
   "sienna"
   "indigo"
   "maroon"
   "darkslategray"
   "orange"
   "gray"
   "limegreen"
   "orangered"
   "navy"
   "mediumorchid"
   "forestgreen"
   "violet"))

(rplacd (last *color-list*) *color-list*)

;; print dot node representation
(defmethod print-node ((node sn-user-node) session stream)
  (let ((user-link (sn-user-link session (sn-node-user-id node)))
		(node-name (concatenate 'string
								(sn-node-user-name node)
								" (" (write-to-string 
									  (sn-user-node-level node)) ")")))
	(if *print-photos*
		(format stream *node-stub-photo*
				(sn-id->string session (sn-node-user-id node))
				(let ((userpic-uri (sn-user-get-data (sn-user-node-user node)
													 :userpic-path)))
				  (if userpic-uri
					  (format nil *graphical-cell-stub* userpic-uri)
					  *plain-cell-stub*))
				node-name user-link user-link)
		(format stream *node-stub-plain*
				(sn-id->string session (sn-node-user-id node))
				node-name user-link user-link))))

;; print edges to non visited nodes in the given user-pool
(defmethod print-edges ((node sn-user-node) session user-pool stream)
  (dolist (adjanced-id (sn-user-node-adjacent node))
    (let ((adjanced-node (sn-user-pool-node user-pool adjanced-id)))
      (when (and adjanced-node
		 (not (sn-user-node-visited-p adjanced-node)))
	(format stream *edge-stub*
		(sn-id->string session (sn-node-user-id node))
		(sn-id->string session adjanced-id)
		(car *color-list*))
	(setf *color-list* (cdr *color-list*))))))

(defmethod sn-print-graph ((user-pool sn-user-pool) session directory &key 
						   (load-graphics t)
						   delay
						   acceptorf)
    (let* ((root-user (sn-user-node-user (sn-user-pool-root-node user-pool)))
		   (dot-file-path (sn-mk-userfile-path root-user "dot" directory))
		   (*print-photos* load-graphics)
		   (pics-loaded 0))
	  (flet ((load-userpic (node)
			   (let ((user (sn-user-node-user node)))
				 (let ((picture-path (sn-load-userpic user session directory)))
				   (when picture-path
					 (setf pics-loaded (1+ pics-loaded))
					 (when acceptorf (funcall acceptorf pics-loaded))
					 (sn-user-set-data user :userpic-path picture-path))))))
		(with-open-file (dot-file dot-file-path 
								  :direction :output 
								  :if-exists :supersede
								  :external-format 
								  #+clisp (ext:make-encoding
										   :charset charset:utf-8
										   :line-terminator #+win32 :dos
										   #-win32 :unix)
								  #+(or ccl sbcl) :utf-8
								  #-(or clisp sbcl ccl) :default)
		  (format dot-file *dot-header*)
		  (sn-traverse-user-pool user-pool 
								 #'(lambda (node)
									 (when load-graphics
									   (load-userpic node))
									 (print-node node session dot-file))
								 :algorithm :breadth-first
								 :delay (when load-graphics delay))
		  (sn-traverse-user-pool 
		   user-pool
		   #'(lambda (node)
			   (print-edges node session user-pool dot-file)))
		  (format dot-file *dot-footer*))
		dot-file-path)))
