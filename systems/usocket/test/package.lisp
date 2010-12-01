;;;; $Id: package.lisp 528 2010-06-29 12:16:48Z ctian $
;;;; $URL: svn://common-lisp.net/project/usocket/svn/usocket/trunk/test/package.lisp $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(defpackage :usocket-test
  (:use :cl :regression-test)
  (:nicknames :usoct)
  (:export :do-tests :run-usocket-tests))
