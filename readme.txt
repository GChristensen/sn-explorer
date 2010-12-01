sn-explorer

(C) 2010, g/christensen (gchristnsn@gmail.com)

sn-explorer is a toy web crawler for social networks written in Common Lisp.
It implements simplest possible breadth-first network traversal algorithm 
without multiprocessing and supports only Facebook and popular russian 
VKontakte social network, therefore as is it could be interesting only for the 
corresponding audience. It fully localized though (offhand and a little 
clumsy), and it should be relatively easy to add support of any other social 
network to the sn-explorer. 
Besides crawling sn-explorer could also produce graphical social network 
representation with the Graphviz graph rendering system.

There is at least two ways to run sn-explorer:

  1. Download Windows self-contained package, and it should run out of the box 
     (it will also isntall the "Microsoft Visual C++ 2005 Redistributable" 
     required by Graphviz).
     
     Download link: https://github.com/downloads/GChristensen/sn-explorer/sn-explorer.7z.sfx.exe 

  2. You may try to use the cross-platform lispx-proxy executable source 
     distribution, which requires the following dependencies:

       * Graphviz (http://graphviz.org)
       * Lisp Execution Proxy (http://lispx-proxy.soruceforge.net)
       * Suitable Common Lisp implementation accessible through the PATH
         environment variable. Clozure CL (recommended) or SBCL (Linux only) 
         will work.

     Download link: https://github.com/downloads/GChristensen/sn-explorer/sn-explorer.lispxz

sn-explorer-gui requires Common Lisp implementation with multiprocessing 
capabilities, but sn-explorer library could run on single-threaded Lisp 
implementations (ClozureCL, SBCL, CLISP are supported).
