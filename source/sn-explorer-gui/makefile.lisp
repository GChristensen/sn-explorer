;; This file is part of sn-explorer-gui library
;;
;; (C) 2010 g/christensen (gchristnsn@gmail.com)

;; example usage: (load "./lisp/development/sn-explorer-gui/makefile.lisp")

(push :production *features*)

(asdf:oos 'asdf:load-op :sn-explorer-gui)

#+(and windows ccl)
(ccl:save-application "sn-explorer-gui.bin" 
                      :toplevel-function #'sn-explorer-gui:gui-start 
                      :prepend-kernel t)