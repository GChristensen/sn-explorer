# $Id: Makefile 544 2010-07-12 16:40:18Z ctian $
# $URL: svn://common-lisp.net/project/usocket/svn/usocket/trunk/Makefile $

clean:
	find . \( -name "*~" -o -name "*.err" -o -name "*.x86f" -o -name "*.lib" -o -name "*.fas" -o -name "*.*fasl" -o -name "*.faslmt" -o -name "*.abcl" -o -name "*.*fsl" -o -name "*.o" -o -name "*.sse2f" \) -delete

commit:
	make clean; svn up; svn ci
