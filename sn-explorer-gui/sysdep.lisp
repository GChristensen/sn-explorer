;; This file is part of sn-explorer-gui library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-gui)

#+(and (or win32 windows) production)
(defun open-gui-impl ()
  (foreign-funcall "ShellExecuteA" 
				   :pointer (null-pointer) 
				   :string "open"
				   :string (format nil "http://localhost:~a" *http-port*)
				   :pointer (null-pointer)
				   :pointer (null-pointer)
				   :int 0))

#+(and (or win32 windows) production)
(defcallback menu-callback :void ((code :int))
  (case code
	(200 (open-gui-impl))
	(201 (cleanup-and-terminate))))

#+(and (or win32 windows) production)
(defun sysdep-init-impl ()
  (load-foreign-library "Shell32")
  (load-foreign-library "traymenu")
  (with-system-language
	(foreign-funcall "show_tray_icon" :string #$icon-tip 
					 :pointer (callback menu-callback))
	(foreign-funcall "add_tray_menu_item" :int 200 :string #$menu-open)
	(foreign-funcall "add_tray_menu_item" :int 201 :string #$menu-exit))
  (open-gui-impl))

#+(and (or win32 windows) production)
(defun sysdep-finalize-impl ()
  (foreign-funcall "hide_tray_icon"))

(defun sysdep-init ()
  #+(and (or win32 windows) production) (sysdep-init-impl))

(defun sysdep-finalize ()
  #+(and (or win32 windows) production) (sysdep-finalize-impl))

#+(and (or win32 windows) production)
(defun get-system-language-impl ()
  (let ((lang-id (foreign-funcall "GetSystemDefaultLangID" :short)))
	(if (= lang-id #x419) "ru" "en")))

(defun get-system-language ()
  #+(and (or win32 windows) production) (get-system-language-impl)
  #-(and (or win32 windows) production) "en")
  