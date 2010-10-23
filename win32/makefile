all: dll launcher

dll: traymenu.c traymenu.def
	gcc -DUNICODE -D_UNICODE -shared -static-libgcc -o traymenu.dll traymenu.c traymenu.def -Wl,--enable-stdcall-fixup

launcher: launcher.c
	gcc -DUNICODE -D_UNICODE -mwindows -static-libgcc -o sn-explorer.exe launcher.c

clean:
	del /Q *.dll
	del /Q *.exe