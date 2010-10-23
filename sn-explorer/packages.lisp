;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(defpackage :sn-explorer-client
  (:use :common-lisp :drakma)
  (:export *supported-social-networks*
		   *sn-http-proxy*
		   *user-agent*
		   sn-session
		   sn-user
		   sn-user-id
		   sn-user-name
		   sn-userpic-uri
		   sn-user-data
		   sn-user-set-data
		   sn-user-get-data
		   sn-user-rem-data
		   sn-http-request
		   sn-with-reusable-connection
		   sn-extract-filetype
		   sn-mk-userfile-path
		   sn-load-userpic
		   sn-login
		   sn-get-session-cookie
		   sn-extract-user-id
		   sn-logout
		   sn-agent-id
		   sn-user-link
		   sn-id->string
		   sn-instantiate-user
		   sn-adjacent-users))

(defpackage :sn-explorer-client-vkontakte
  (:use :common-lisp :cl-ppcre :drakma :sn-explorer-client)
  (:export sn-session-vkontakte))

(defpackage :sn-explorer-crawler
  (:use :common-lisp
		:sn-explorer-client)
  (:export sn-user-node
		   sn-user-node-user
		   sn-user-node-level
		   sn-user-node-adjacent
		   sn-user-node-visited-p
		   sn-node-user-id
		   sn-node-user-name
		   sn-node-userpic
		   sn-node-user-data
		   sn-user-pool
		   sn-user-pool-node
		   sn-user-pool-root-node
		   sn-crawl-users
		   sn-traverse-user-pool))

(defpackage :sn-explorer-graph
  (:use :common-lisp
		:sn-explorer-client
		:sn-explorer-crawler)
  (:export sn-print-graph))

(defpackage :sn-explorer-user
  (:use :common-lisp
		:external-program
		:sn-explorer-client
		:sn-explorer-crawler
		:sn-explorer-graph)
  (:export *sn-dot-program*
           sn-dot->image
           sn-explore
           sn-explore-interactive))
