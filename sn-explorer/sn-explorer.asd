;; This file is part of sn-explorer library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(defpackage :sn-explorer-asd
  (:use :common-lisp :asdf))

(in-package :sn-explorer-asd)

(defsystem :sn-explorer
  :description "A simple social network crawler."
  :version "0.1"
  :components ((:file "packages")
               (:file "sn-explorer-client" :depends-on ("packages"))
               (:file "sn-explorer-client-vkontakte" 
					  :depends-on ("sn-explorer-client"))
               (:file "sn-explorer-crawler" :depends-on ("sn-explorer-client"))
               (:file "sn-explorer-graph" 
					  :depends-on ("sn-explorer-client" "sn-explorer-crawler"))
               (:file "sn-explorer" :depends-on ("sn-explorer-graph")))
  :depends-on (:drakma :cl-ppcre :external-program))