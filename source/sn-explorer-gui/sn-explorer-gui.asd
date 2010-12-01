;; This file is part of sn-explorer-gui library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(defpackage :sn-explorer-gui-asd
  (:use :common-lisp :asdf))

(in-package :sn-explorer-gui-asd)

(defsystem :sn-explorer-gui
  :description "Graphical user interface for sn-explorer library."
  :version "0.1"
  :components ((:file "packages")
               (:file "i18n" :depends-on ("packages"))
               (:file "strings-ru" :depends-on ("i18n"))
               (:file "strings-en" :depends-on ("i18n"))
               (:file "sysdep" :depends-on ("packages"))
               (:file "sn-explorer-gui" :depends-on ("i18n" "sysdep")))
  :depends-on (:hunchentoot :ht-simple-ajax :cl-who :parenscript :cl-json
			   :cl-fad :sn-explorer :trivial-features :cffi))