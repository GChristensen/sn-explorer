all: dll launcher

dll: traymenu.cpp traymenu.def
	g++ -DUNICODE -D_UNICODE -shared -o traymenu.dll traymenu.cpp traymenu.def -Wl,--enable-stdcall-fixup

launcher: launcher.cpp
	g++ -DUNICODE -D_UNICODE -mwindows -o sn-explorer.exe launcher.cpp

clean:
	del /Q *.dll
	del /Q *.exe