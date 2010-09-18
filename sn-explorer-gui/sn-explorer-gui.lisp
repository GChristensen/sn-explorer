;; This file is part of sn-explorer-gui library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-gui)

#-production
(progn
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*log-lisp-backtraces-p* t)
  (setf hunchentoot:*message-log-pathname* (pathname "hunchentoot-log.txt")))

#+production 
(progn
    (setf hunchentoot:*handle-http-errors-p* nil)
	(defvar *entry-point-lock* (make-lock))
	(defvar *entry-point-event* (make-condition-variable)))

(defparameter *http-port* 50005)

(defvar *main-acceptor* nil)

(setf hunchentoot:*default-content-type* "text/html; charset=utf-8")

(define-condition sn-explorer-error (condition) ())
(define-condition sn-explorer-stop (condition) ())

(defconstant +working-root+ ".sn-explorer")
(defvar *working-root-pathname* nil)

(defun create-session-working-dir ()
  (let* ((session-id (subseq (session-cookie-value *session*) 2))
		 (working-dir
		  (merge-pathnames (make-pathname :directory `(:relative ,session-id))
						   *working-root-pathname* )))
	(ensure-directories-exist working-dir)
	(setf (session-value 'working-dir) working-dir)
	(setf (session-value 'session-id) session-id)
	(push (create-folder-dispatcher-and-handler (conc "/" session-id "/")
												working-dir)
      *dispatch-table*)
	working-dir))

;; sticky handlers - server will automatically redirect to a page defined      
;; by a sticky-handler named like the current session state despite of 
;; entered url; but there are some exceptions exist in 
;; `default-redirect-dispatch-fn'
(defparameter *name-to-uri* (make-hash-table))

(defun default-redirect-dispatch-fn (state)
  (if *session*
	  (let ((asummed-state (session-value 'state)))
		(when (and asummed-state (not (eq state asummed-state)))
		  (let ((uri (gethash asummed-state *name-to-uri*))) 
			(when uri
			  (when (not (and (eq asummed-state 'displaying-graph)
							  (or (eq state 'tuning-crawler)
								  (eq state 'tuning-graphviz))))
				(redirect uri) 
				t)))))
	  (when (not (eq state 'welcome-screen))
		(redirect "/")
		t)))
	  
(defmacro default-redirect-dispatch (asummed-state)
  `(when (default-redirect-dispatch-fn ',asummed-state)
	(return-from ,asummed-state)))

(defmacro def-sticky-handler ((name &key uri) (&rest args) 
							  &body body)
  `(progn
	 (setf (gethash ',name *name-to-uri*) ,uri)
	 (define-easy-handler (,name :uri ,uri) (,@args)
	   (default-redirect-dispatch ,name)
	   (setf (session-value 'state) ',name)
	   ,@body)))

;; basic skeleton of a page
(defmacro page-skeleton ((&key (class "centre") (logout t)) &body body)
  `(with-useragent-language
    (with-html-output-to-string (*standard-output* nil :indent t :prologue nil)
	 (:html :class ,class
      (:head
       (:meta :http-equiv "Content-Type" 
			  :content "text/html;charset=UTF-8")
	   (:link :type "text/css" 
			  :rel "stylesheet"
			  :href "/files/css/style.css")
       (:title (str #$title)))
      (:body :class ,class
	   (:div :class "heading"
		(:table :class "heading"
				(:tr (:td (:span :class "logo" (str #$title))
						  (:span :class "ver" (str #$version)))
					 (:td ,@(when logout
								  '((:div :class "button" 
									(:a :href "/quit" (str #$exit)))))))))
	   (when (search "MSIE" (user-agent))
		 (htm (:div :class "red-text warn" (str #$refuse-ie))))
       ,@(if (string= class "centre")
			 `((:div :id "page"
				(:div :id "content_container"
				 (:div :id "content"
					   ,@body))))
			 `(,@body)))))))

(define-easy-handler (error-handler :uri "/error") nil
  (page-skeleton ()
     (:div :class "error"
      (:div (:h3 :align "center" (str #$error))
			(:p (str (session-value 'error-message)))
			(:p :align "right" 
				(:a :href (session-value 'path-back)
					(str #$back)))))))

(defun redirect-to-error (message path-back &optional (session *session*))
  (setf (session-value 'error-message session) message)
  (setf (session-value 'path-back session) path-back)
  (redirect "/error"))

(defun generic-error-handler ()
  (redirect-to-error "" "javascript:history.go(-1)"))

(setf *default-handler* #'generic-error-handler) 

(def-sticky-handler (welcome-screen :uri "/") nil
  (page-skeleton (:logout nil)
	  (:a :class "agreement" :href "/agreement" (str #$agreement))
	  (:a :class "about" :href "/about" (str #$about))
      (:form :action "/login" :method "post"
	   (:div :class "frame"
		(:table :class "form"
		 (:tr (:td (str #$network))
			  (:td (:select :name "network"
							:class "input-control"
							(dolist (netw *supported-social-networks*)
							  (htm
							   (:option :value (format nil "~(~s~)" netw)
										(str (format nil "~(~a~)" netw))))))))
		 (:tr (:td (str #$login))
			  (:td (:input :name "login"
						   :class "input-control")))
		 (:tr (:td (str #$password))
			  (:td (:input :name "password" 
						   :type "password"
						   :class "input-control")))
		 (:tr (:td :colspan "2" 
				   (:div :class "submit" 
						 (:input :type "submit" 
								 :value (str #$enter))))))))))

(defun agreement-screen ()
  (page-skeleton ()
   (:div :class "agreement"
	(:div (str #$agreement-text);(file-string *agreement-file*))
		  (:p :align "right" 
			  (:a :href "/" (str #$back)))))))

(push (create-regex-dispatcher "^/agreement/?$" 'agreement-screen)
      *dispatch-table*)

(defun about-screen ()
  (page-skeleton ()
   (:div :class "about"
		 (:h3 (str #$title))
		 (:p (str #$version))
		 (:p :class "justify" (str #$about-desc))
		 (:p (str #$credits))
		 (:p (str #$str-dept))
		 (:p :align "right" 
			 (:a :href "/" (str #$back))))))
	
(push (create-regex-dispatcher "^/about/?$" 'about-screen)
      *dispatch-table*)

;; social network entrance logic
(define-easy-handler (login-handler :uri "/login"
									:default-parameter-type 'string
									:default-request-type :post)
	(network login password)
  (with-useragent-language
	(let ((session (if *session* *session* (start-session))))
	  (create-session-working-dir)
	  (handler-case
		(let ((sn-session (sn-login (read-from-string network) 
								  :user login :password password)))
		  (if sn-session
			  (progn
				(setf (session-value 'sn-session session)  sn-session)
				(setf (session-value 'state) 'tuning-crawler)
				(redirect "/step1"))
			  (redirect-to-error #$access-denied "/" session)))
	(condition ()
	  (setf (session-value 'state) 'login-fail)
	  (redirect-to-error #$net-sybsistem-fail-cap "/" session))))))

;; ajax setup
(defparameter *ajax-processor* 
  (make-instance 'ajax-processor :server-uri "/ajax"))

(push (create-ajax-dispatcher *ajax-processor*) *dispatch-table*)

(defun compose-status (status &optional (message ""))
  (encode-json-to-string `((type . "status") (status . ,status)
						   (message . ,(url-encode message)))))

(defun-ajax check-status () (*ajax-processor*)
  (with-useragent-language
	(let* ((state (session-value 'state))
		   (displayed-status (session-value 'displayed-status))
		   (displayed-status (if (null displayed-status) 
								 ""
								 displayed-status))
		   (message 
			(if (numberp displayed-status)
				(let ((template
					  (cond ((eq state 'crawling) #$profiles-processed)
							((eq state 'printing-graph) #$photos-loaded)
							(t ""))))
				  (format nil template displayed-status))
				displayed-status)))
	  (compose-status "REPORT" message))))

(defun-ajax interrupt () (*ajax-processor*)
  (setf (session-value 'state) 'interrupted)
  (encode-json-to-string '((type . "silent"))))

(defun profile-load-status-callback (n)
  (when (eq (session-value 'state) 'interrupted)
	(signal 'sn-explorer-stop))
  (setf (session-value 'displayed-status) n))

(defun photo-load-status-callback (n)
  (when (eq (session-value 'state) 'interrupted)
	(signal 'sn-explorer-stop))
  (setf (session-value 'displayed-status) n))

(defun sanitize-address (address)
  (if (or (null address) (string= "" address) (search "http://" address))
	  address
	  (conc "http://" address)))

(defun-ajax start-crawling (address depth load-photos) (*ajax-processor*)
  (with-useragent-language
	(handler-case
		(progn
		 (setf (session-value 'displayed-status) #$crawler-init)
		 (let* ((sn-session (session-value 'sn-session *session*))
				(address (sanitize-address address))
				(user-id (if (or (null address) (string= "" address))
							 (sn-agent-id sn-session)
							 (handler-case
								 (sn-extract-user-id sn-session address)
								(condition () (signal 'sn-explorer-error)))))
				(depth (parse-integer depth))
				(load-photos (string= (string-downcase load-photos) "true")))
		   (if user-id
			   (let ((working-dir (session-value 'working-dir)))
				 (setf (session-value 'state) 'crawling)
				 (let ((user-pool (sn-crawl-users sn-session user-id depth
								   :acceptorf #'profile-load-status-callback)))
				   (setf (session-value 'state) 'printing-graph)
				   (when load-photos
					 (setf (session-value 'displayed-status) #$img-load-init))
				   (setf (session-value 'dot-path)
						 (sn-print-graph user-pool sn-session working-dir
						  :acceptorf #'photo-load-status-callback
						  :load-graphics load-photos))
				   (setf (session-value 'state) 'tuning-graphviz)
				   (compose-status "OK")))
			   (signal 'sn-explorer-error))))
	  (sn-explorer-error ()
		(setf (session-value 'state) 'tuning-crawler)
		(compose-status "ERROR" #$cant-obtain-user-info))
	  (sn-explorer-stop ()
		(setf (session-value 'state) 'tuning-crawler)
		(compose-status "ERROR" #$interrupted))
	  (condition ()
		(setf (session-value 'state) 'tuning-crawler)
		(encode-json-to-string 
		 `((type . "status") (status . "ERROR")
		   (message . ,(url-encode #$net-sybsistem-fail))))))))

(defun crawler-ajax ()
  (ps 
	(defvar interval-id 0)
	(defun decode-uri (value)
	  (eval "decodeURIComponent(value);"))
	(defmacro &* (id &optional (prop 'value)) ; getElementById shortcut
	  `(chain document (get-element-by-id ',id) ,prop))
	(defun interpret (response) ; instantiate JSON object
	  (let* ((node (chain response (get-elements-by-tag-name "response") 0))
			 (text (@ node text-content))
			 (text (if (=== text undefined) (@ node text) text))) ; IE
		(eval (concatenate 'string "(" text ")"))))
	(defun get-message (response)
	   (decode-uri (getprop response 'message)))
	(defun update-status (message)
	  (setf (&* "status-info" text-content) message))
	(defun status-checker ()
	  (ajax_check_status status-checker-callback))
	(defun status-checker-callback (response)
		(update-status (get-message (interpret response))))
	(defun start-crawling ()
	  (setf (&* start) (decode-uri (lisp (url-encode #$stop-crawling))))
	  (setf (&* start onclick) (lambda () (ajax_interrupt nil)))
	  (ajax_start_crawling (&* address) (&* depth) (&* "load-photos" checked)
						   crawler-callback)
	  (setf interval-id (set-interval (lambda () (status-checker)) 1000)))
	(defun crawler-callback (response)
	  (let* ((response (interpret response))
			 (type (getprop response 'type)))
		(when (= type "status")
		  (clear-interval interval-id)
		  (setf (&* start) (decode-uri (lisp (url-encode #$start-crawling))))
		  (setf (&* start onclick) start-crawling)
		  (let ((status (getprop response 'status)))
			(cond ((= status "ERROR")
				   (update-status (get-message response)))
				  ((= status "OK")
				   (update-status "")
				   (setf (@ window location) "/step2")))))))))

(def-sticky-handler (tuning-crawler :uri "/step1") nil
  (page-skeleton (:class "loose")
	(str (generate-prologue *ajax-processor*))
	(:script :type "text/javascript" (str (crawler-ajax)))
	(:div :id "common-page"
		  (:h3 (str #$step1))
		  (:p (str #$specify-addres))
		  (:hr :class "separator")
		  (:form
		   (:table :class "form"
			(:tr
             (:td (str #$target-address))
			 (:td (:input :id "address" :type "text" :class "address")
				  (:input :id "start" :type "button"
						  :onclick (ps-inline (start-crawling))
						  :value (str #$start-crawling))))
			(:tr
			 (:td (str #$crawling-depth))
			 (:td (:select :id "depth" :class "depth"
						   (:option :value "1" (str #$depth2))
						   (:option :value "2" (str #$depth3)))))
			(:tr
			 (:td (str #$load-photos))
			 (:td
			  (:input :id "load-photos" :type "checkbox" 
					  :style "margin: 0;")))))
		  (:hr :class "separator")
		  (:p :class "status" (str #$status)
			  (:span :id "status-info")))))

(def-sticky-handler (tuning-graphviz :uri "/step2") nil
  (page-skeleton (:class "loose")
	(:div :id "common-page"
		  (:h3 (str #$step2))
		  (:p (str #$specify-plot-options))
		  (:hr :class "separator")
		  (:form :action "/rendering" :method "post"
		   (:table :class "form"
			(:tr
			 (:td (str #$algorithm))
			 (:td (:select :name "algorithm" :class "graph-settings"
						   (:option :value "dot" "dot")
						   (:option :selected "yes" :value "neato" "neato")
						   (:option :value "fdp" "fdp")
						   (:option :value "twopi" "twopi")
						   (:option :value "circo" "circo"))))
			(:tr
			 (:td (str #$image-format))
			 (:td (:select :name "format" :class "graph-settings"
						   (:option :value "svg" "svg")
						   (:option :value "pdf" "pdf"))))
			(:tr
			 (:td (str #$scale))
			 (:td (:select :name "scale" :class "graph-settings" :disabled t
						   (:option :value "100" "100%")
						   (:option :value "75" "75%") 
						   (:option :value "50" "50%")))))
		(:hr :class "separator")
		(:input :id "build" :type "submit"
				:value #$do-build)))))

(define-easy-handler (rendering :uri "/rendering") (algorithm format)
  (with-useragent-language
	(handler-case
		(let* ((image-file 
				(let ((*sn-dot-program* algorithm))
				  (sn-dot->image (session-value 'dot-path) "png" 
								 '("-Gsize=\"5.2,5.2\""))
				  (sn-dot->image (session-value 'dot-path) format)))
			   (file-name (conc "/" (session-value 'session-id) "/" 
								(namestring 
								 (make-pathname :type nil 
												:defaults image-file)))))
		  (when (not (file-exists-p 
					  (merge-pathnames image-file 
									   (session-value 'working-dir))))
			(signal 'sn-explorer-error))
		  (setf (session-value 'state) 'rendered)
		  (setf (session-value 'image-format) format)
		  (setf (session-value 'thumb-file) (conc file-name ".png"))
		  (setf (session-value 'image-file) (conc file-name "." format))
		  (redirect "/step3"))
	(condition ()
	  (redirect-to-error #$graphviz-error "/step2")))))

(def-sticky-handler (displaying-graph :uri "/step3") nil
	(page-skeleton (:class "loose")
	  (:div :id "common-page"
	   (:h3 (str #$step3))
	   (:hr :class "separator")
	   (:div :class "graph-frame"
			 (:img :class "graph" :src (session-value 'thumb-file)))
	   (:div :class "graph-links"
			 (:p (:a :href (session-value 'image-file) :target "_blank" 
					 (str #$view-full-size)))
			 (:p (:a :href "/step2" (str #$change-viz-settings)))
			 (:p (:a :href "/step1" (str #$new-collection)))))))

(define-easy-handler (quit :uri "/quit") nil
  (setf (session-value 'state) 'welcome-screen)
  (redirect "/"))

(define-easy-handler (ping :uri "/ping") nil
  "PONG")
 
(define-easy-handler (style :uri "/files/css/style.css") nil
  (load-time-value
   (file-string (merge-pathnames 
				 "files/css/style.css"
				 #.*compile-file-truename*))))

(push #'dispatch-easy-handlers *dispatch-table*)

(defun cleanup-and-terminate ()
  (when (directory-exists-p *working-root-pathname*)
	(delete-directory-and-files *working-root-pathname*))
  #+production
  (progn
	(acquire-lock *entry-point-lock*)
	(condition-notify *entry-point-event*)
	(release-lock *entry-point-lock*)))

(define-easy-handler (terminate :uri "/terminate") nil
  (cleanup-and-terminate))

(defun gui-start ()
  (setf *working-root-pathname* 
		(merge-pathnames 
		 (make-pathname :directory `(:relative ,+working-root+))
		 (user-homedir-pathname)))
  (ensure-directories-exist *working-root-pathname*)
  (setf *main-acceptor*	(start (make-instance 'acceptor :port *http-port*)))
  #+production
  (progn
	(sysdep-init)
	(acquire-lock *entry-point-lock*)
	(condition-wait *entry-point-event* *entry-point-lock*)
	(sysdep-finalize)
	(when *main-acceptor*
	  (stop *main-acceptor*)
	  (setf *main-acceptor* nil))))

(defun gui-stop ()
  (when *main-acceptor*
	(stop *main-acceptor*)))