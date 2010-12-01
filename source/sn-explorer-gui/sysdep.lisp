;; This file is part of sn-explorer-gui library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-gui)

(defun sysdep-init-impl ()
  #+windows (load-foreign-library "Shell32"))

#+windows
(defun open-gui-impl ()
  (foreign-funcall "ShellExecuteA" 
				   :pointer (null-pointer) 
				   :string "open"
				   :string (format nil "http://localhost:~a" *http-port*)
				   :pointer (null-pointer)
				   :pointer (null-pointer)
				   :int 0))

#-windows
(defun open-gui-impl ()
  (external-program:start "xdg-open" 
						  (list (format nil "http://localhost:~a"
										*http-port*))))

(defun sysdep-init ()
  (sysdep-init-impl)
  (open-gui-impl))

(defun sysdep-finalize ())

#+windows
(defun get-system-language-impl ()
  (let ((lang-id (foreign-funcall "GetSystemDefaultLangID" :short)))
	(if (= lang-id #x419) "ru" "en")))

(defun get-system-language ()
  #+windows (get-system-language-impl)
  #-windows "en")
  