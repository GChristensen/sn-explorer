sn-explorer

(C) 2010, g/christensen (gchristnsn@gmail.com)

sn-explorer is a toy web crawler for social networks written in Common Lisp.
It implements simplest possible breadth-first network traversal algorithm 
without multiprocessing and supports only Facebook and popular russian VKontakte
social network, therefore as is it could be interesting only for the corresponding 
audience. It fully localized though (offhand and a little clumsy), and it should 
be relatively easy to add support of any other social network to the sn-explorer. 
Besides crawling sn-explorer could also produce graphical social network 
representation with the Graphviz graph rendering system.

There is currently two ways to run sn-explorer:

  1. Download Windows package, and it should run out of the box (it will also
     isntall "Microsoft Visual C++ 2005 Redistributable" required by Graphviz).
  2. Install Graphviz, place sn-explorer and all its dependencies into ASDF 
     repository, issue the following instructions in your Lisp environment REPL:

       (asdf:oos 'asdf:load-op :sn-explorer-gui)
       (sn-explorer-gui:gui-start)

     and enter the http://localhost:50005/ URL in your browser.


Dependencies

  * Graphviz (http://graphviz.org)

  * Lisp libraries (all are ASDF-installable):

      drakma
      cl-ppcre
      external-program
      hunchentoot
      ht-simple-ajax
      parenscript
      cl-json
      cl-who
      cl-fad

sn-explorer-gui requires Common Lisp implementation with multiprocessing 
capabilities (currently it was tested only with ClozureCL on Windows), but 
sn-explorer library could run on single-threaded Lisp implementations
(ClozureCL, SBCL, CLISP are supported).
