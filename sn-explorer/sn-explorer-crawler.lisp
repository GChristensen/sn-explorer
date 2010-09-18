;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-crawler)

(defstruct sn-user-node
  "Represents node in user adjacency table"
  user level adjacent visited-p)

(defclass sn-user-pool ()
  ((sn-adjacency-list :reader sn-adjacency-list
		      :initarg :sn-adjacency-list)
   (sn-root-user-id :reader sn-root-user-id
		    :initarg :sn-root-user-id))
  (:documentation "Contains SN traversal data"))

(defun sn-node-user-id (node)
  (sn-user-id (sn-user-node-user node)))

(defun sn-node-user-name (node)
  (sn-user-name (sn-user-node-user node)))

(defun sn-node-userpic (node)
  (sn-userpic-uri (sn-user-node-user node)))

(defun sn-node-user-data (node)
  (sn-user-data (sn-user-node-user node)))

(defmethod sn-user-pool-node ((user-pool sn-user-pool) user-id)
  (gethash user-id (sn-adjacency-list user-pool)))

(defmethod sn-user-pool-root-node ((user-pool sn-user-pool))
  (gethash (sn-root-user-id user-pool) 
	   (sn-adjacency-list user-pool)))

;; iterative breadth-first traversal algorithm (web-crawling)
(defun sn-crawl-users (session root-user-id depth &key group delay acceptorf)
  "Collects data about user relations up to defined depth."
  (declare (ignore group))
  (let ((adjacency-list (make-hash-table :test #'equal)))
    (labels ((crawl (node-queue)
	       (when delay (sleep delay))
	       (if (null node-queue) ; finish crawling
		   (make-instance 'sn-user-pool
				  :sn-root-user-id root-user-id
				  :sn-adjacency-list adjacency-list)
		   (crawl ; hope for tail recursion optimization
		    (let* ((current-node (pop node-queue))
			   (current-node-id (sn-node-user-id current-node))
			   (next-level (1+ (sn-user-node-level current-node))))
		      (if (nth-value 1 (gethash current-node-id	adjacency-list))
			  node-queue ; skip this node, we adready have it
			  (let ((children ; retreive current user children
				 (mapcar #'(lambda (user) ; wrap user to node struct
					     (make-sn-user-node
					      :user user
					      :level next-level))
					 (sn-adjacent-users session 
							    current-node-id))))
			    ;; add current node to adjacency list
			    (setf (sn-user-node-adjacent current-node)
				  (mapcar #'(lambda (n) 
					      (sn-node-user-id n)) children))
			    (setf (gethash current-node-id adjacency-list) 
				  current-node)
				(when acceptorf
				  (funcall acceptorf (hash-table-count adjacency-list)))
			    ;; add children to queue if required depth hasn't been
			    ;; reached
			    (if (and children 
				     (< (sn-user-node-level current-node) depth))
				(nconc node-queue children)
				node-queue))))))))
      (sn-with-reusable-connection (session)
		(crawl (list (make-sn-user-node 
					  :user (sn-instantiate-user session root-user-id)
					  :level 0)))))))

(defmethod sn-reset-user-pool ((user-pool sn-user-pool))
  (let ((adjacency-list (sn-adjacency-list user-pool)))
    (loop for user-node being the hash-value in adjacency-list
	  do (setf (sn-user-node-level user-node) nil)
	     (setf (sn-user-node-visited-p user-node) nil))))

;; recursive depth-first traversal algorithm (for graph printing)
(defmethod sn-traverse-user-pool ((user-pool sn-user-pool) acceptorf &key
				  (algorithm :depth-first)
				  root-user-id 
				  depth 
				  delay)
  "Calls acceptorf on each user-pool node prior to given depth if specified.
Any user ID contained as key in adjacency list could be used as root-user-id
seed. :depth-first and :breadth-first algorithms are available."
  (labels ((traverse-df (node)
	     (when delay (sleep delay))
	     (let ((next-level (1+ (sn-user-node-level node))))
	       (funcall acceptorf node)
	       (setf (sn-user-node-visited-p node) t)
	       (when (or (not depth)
			 (<= next-level depth))
		 ;; iterate ajdanced nodes
		 (dolist (adjacent-id (sn-user-node-adjacent node))
		   (let ((adjacent-node (sn-user-pool-node user-pool 
							   adjacent-id)))
		     (when (and adjacent-node 
				(not (sn-user-node-visited-p adjacent-node)))
		       (setf (sn-user-node-level adjacent-node) next-level)
		       (traverse-df adjacent-node)))))))
	   (filter-uid-bf (user-id) 
	     (let ((node (sn-user-pool-node user-pool user-id)))
	       (when (and node
			  (not (sn-user-node-visited-p node))
			  (not (sn-user-node-level node)))
		 node)))
	   (traverse-bf (node-queue)
	     (when delay (sleep delay))
	     (when node-queue
	       (traverse-bf 
		(let ((current-node (pop node-queue)))
		  (if (sn-user-node-visited-p current-node)
		      node-queue ; skip
		      (let ((children (sn-user-node-adjacent current-node))
			    (next-level (1+ (sn-user-node-level current-node))))
			(funcall acceptorf current-node)
			(setf (sn-user-node-visited-p current-node) t)
			(if (or (not depth)
				(<= next-level depth))
			    (nconc node-queue ; add adjacent nodes to node queue
				   (mapcan #'(lambda (user-id) 
					       (let ((node (filter-uid-bf user-id)))
						 (when node
						   (setf (sn-user-node-level node)
							 next-level)
						   (list node))))
					   children))
			    node-queue))))))))
    (sn-reset-user-pool user-pool)
    (let ((root-node (sn-user-pool-node user-pool
					(or root-user-id
					    (sn-root-user-id user-pool)))))
      (when root-node
	(setf (sn-user-node-level root-node) 0)
	(case algorithm
	  (:depth-first (traverse-df root-node))
	  (:breadth-first (traverse-bf (list root-node)))
	  (t (error (format nil "Invalid algorithm: ~a" algorithm)))))
      t)))