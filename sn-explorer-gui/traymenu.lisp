;; traymenu library Lisp bindings
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

(in-package :sn-explorer-gui)

(cffi:define-foreign-library traymenu
  (t (:default "traymenu")))

(cffi:defcfun ("show_tray_icon" show-tray-icon-ff) :void
		 (tooltip :string) 
		 (menu-callback :pointer))

(defmacro show-tray-icon (title callback)
  `(let ((cffi:*default-foreign-encoding* :utf-16))
    (show-tray-icon-ff ,title (cffi:callback ,callback))))

(cffi:defcfun ("hide_tray_icon" hide-tray-icon) :void)

(cffi:defcfun ("add_tray_menu_item" add-tray-menu-item-ff) :void 
		 (code :int) 
		 (text :string))

(defun add-tray-menu-item (code text)
  (let ((cffi:*default-foreign-encoding* :utf-16))
    (add-tray-menu-item-ff code text)))

(defun use-tray-menu ()
  (cffi:use-foreign-library traymenu))

(defmacro def-menu-callback (name (arg) &body body)
  `(cffi:defcallback ,name :void ((,arg :int))
	 ,@body)) 
