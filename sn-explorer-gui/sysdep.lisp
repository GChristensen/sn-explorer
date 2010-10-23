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
(def-menu-callback menu-callback (code)
  (case code
	(200 (open-gui-impl))
	(201 (cleanup-and-terminate))))

#+(and (or win32 windows) production)
(defun sysdep-init-impl ()
  (load-foreign-library "Shell32")
  (use-tray-menu)
  (with-system-language
	(show-tray-icon #$icon-tip menu-callback)
	(add-tray-menu-item 200 #$menu-open)
	(add-tray-menu-item 201 #$menu-exit))
  (open-gui-impl))

#+(and (or win32 windows) production)
(defun sysdep-finalize-impl ()
  (hide-tray-icon))

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
  