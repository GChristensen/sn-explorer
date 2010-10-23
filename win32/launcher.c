// sn-explorer launcher
// (C) 2010 g/christensen (gchristnsn@gmail.com)

#include <tchar.h>
#include <windows.h>

#include "sn-explorer.h"

DWORD LaunchTarget(TCHAR *target, TCHAR *arguments)
{
	STARTUPINFOW siStartupInfo;
    PROCESS_INFORMATION piProcessInfo;
    ZeroMemory(&siStartupInfo, sizeof(siStartupInfo));
    ZeroMemory(&piProcessInfo, sizeof(piProcessInfo));
    siStartupInfo.cb = sizeof(siStartupInfo); 

	if (CreateProcess(target, arguments, NULL, NULL, TRUE, CREATE_NO_WINDOW,
			NULL, NULL, &siStartupInfo, &piProcessInfo))
	{
		CloseHandle(piProcessInfo.hProcess);
		CloseHandle(piProcessInfo.hThread); 
		return 0;
	}
	
	return 1;
}

int WINAPI _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
	if (FindWindow(TRAYMENU_WND_CLASS, NULL))
		return 1;

	// launcher directory
	TCHAR module_dir[MAX_PATH];
	GetModuleFileName(hInstance, module_dir, MAX_PATH);

	TCHAR *backslash = _tcsrchr(module_dir, _T('\\'));
	*(backslash + 1) = 0;

	size_t module_dir_len = _tcslen(module_dir);

	// get PATH env. var
	DWORD new_path_len = GetEnvironmentVariable(_T("PATH"), NULL, 0);
	new_path_len += module_dir_len * 2 + 50;

	TCHAR *path = (TCHAR *)malloc(new_path_len * sizeof(TCHAR));

	GetEnvironmentVariable(_T("PATH"), path, new_path_len);
	DWORD path_len = _tcslen(path);

	// add <module dir>/bin to path
	*(path + path_len) = _T(';');
	path_len += 1;
	_tcscpy(path + path_len, module_dir);
	path_len += module_dir_len;
	_tcscpy(path + path_len, _T("bin"));
	path_len += 3;

	// add <module dir>/bin/graphiz/bin to path
	*(path + path_len) = _T(';');
	path_len += 1;
	_tcscpy(path + path_len, module_dir);
	path_len += module_dir_len;
	_tcscpy(path + path_len, _T("bin\\graphviz\\bin"));

	SetEnvironmentVariable(_T("PATH"), path);

	// launch sn-explorer
	TCHAR target_path[MAX_PATH];
	_tcscpy(target_path, module_dir);
	_tcscpy(target_path + module_dir_len, _T("bin\\sn-explorer-gui.bin"));

	free(path);

	return LaunchTarget(target_path, NULL);
}

#if defined(__GNUC__) && defined(_UNICODE) && !defined(_tWinMain)
int WINAPI WinMain(HINSTANCE hInstance,
                   HINSTANCE hPrevInstance,
                   LPSTR     lpCmdLine,
                   int       nCmdShow)
{
    return _tWinMain(hInstance, NULL, GetCommandLine(), 0);
}
#endif
