// A simple library for tray icon display which is intended to be used as a 
// reminder about working web server

// (C) 2010 g/christensen (gchristnsn@gmail.com)

#include <tchar.h>
#include <windows.h>

#include "sn-explorer.h"

#define TOOLTIP_LEN 128
#define WM_TRAY_CALLBACK (WM_USER + 1)

typedef void (WINAPI *external_callback)(int code);

BOOL iconSet = FALSE;
TCHAR toolTip[TOOLTIP_LEN] = {0};
external_callback menuCallback = NULL;

HWND hWnd = NULL;
HINSTANCE hInstance = NULL;

BOOL APIENTRY DllMain(HMODULE hModule,
                                 DWORD  ul_reason_for_call,
                                 LPVOID lpReserved)
{
	switch( ul_reason_for_call ) 
    {
    case DLL_PROCESS_ATTACH:
		hInstance = (HINSTANCE)hModule;
		break;
    case DLL_THREAD_ATTACH:
    break;
    case DLL_THREAD_DETACH:
    break;
    case DLL_PROCESS_DETACH:
		SendMessage(hWnd, WM_CLOSE, 0, 0);
    break;
    }

    return TRUE;
}

ATOM MyRegisterClass(HINSTANCE hInstance);
BOOL InitInstance(HINSTANCE, int);
LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);

HMENU hMenu;
NOTIFYICONDATA niData;

DWORD WINAPI messageLoop(LPVOID lpParameter)
{
	MSG msg;

	MyRegisterClass(hInstance);

	if (!InitInstance (hInstance, SW_SHOW))
	{
		return FALSE;
	}

	while (GetMessage(&msg, NULL, 0, 0))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	return (int) msg.wParam;
}

ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;

	ZeroMemory(&wcex, sizeof(WNDCLASSEX));

	wcex.cbSize = sizeof(WNDCLASSEX);

	wcex.lpfnWndProc	= WndProc;
	wcex.hInstance		= hInstance;
	wcex.lpszClassName	= TRAYMENU_WND_CLASS;

	return RegisterClassEx(&wcex);
}

BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
	hWnd = CreateWindow(TRAYMENU_WND_CLASS, NULL, WS_OVERLAPPEDWINDOW,
								0, 0, 0, 0, NULL, NULL, hInstance, NULL);

	if (!hWnd)
	{
		return FALSE;
	}
	
	TCHAR icon_path[MAX_PATH];
	GetModuleFileName((HMODULE)hInstance, icon_path, MAX_PATH);
	TCHAR *dot = _tcsrchr(icon_path, _T('\\'));
	_tcscpy(dot + 1, _T("traymenu.ico"));

	ZeroMemory(&niData, sizeof(NOTIFYICONDATA));

	niData.cbSize = sizeof(NOTIFYICONDATA);
	niData.uID = 0;
	_tcscpy(niData.szTip, toolTip);
	niData.uFlags = NIF_ICON|NIF_MESSAGE|NIF_TIP;
	niData.hIcon = (HICON)LoadImage(NULL,
						icon_path,
						IMAGE_ICON,
						GetSystemMetrics(SM_CXSMICON),
						GetSystemMetrics(SM_CYSMICON),
						LR_DEFAULTCOLOR | LR_LOADFROMFILE);
	niData.hWnd = hWnd;
	niData.uCallbackMessage = WM_TRAY_CALLBACK;

	Shell_NotifyIcon(NIM_ADD, &niData);

	return TRUE;
}

LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	int wmId;

	switch (message)
	{
	case WM_COMMAND:
		wmId = LOWORD(wParam);
		if (menuCallback)
			menuCallback(wmId);
		break;
	case WM_TRAY_CALLBACK:
        switch(lParam)
        {
        case WM_RBUTTONDOWN:
			{
				POINT point;
				GetCursorPos(&point);
				SetForegroundWindow(hWnd);
				TrackPopupMenu(hMenu, TPM_RIGHTALIGN, point.x, point.y, 0, hWnd, NULL);
			}
        }
        break;
	case WM_DESTROY:
		PostQuitMessage(0);
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}

	return 0;
}

VOID WINAPI show_tray_icon(TCHAR *tooltip, external_callback callback)
{
	iconSet = TRUE;
	menuCallback = callback;
	hMenu = CreatePopupMenu();
	_tcsnset(toolTip, 0, TOOLTIP_LEN);
	_tcsncpy(toolTip, tooltip, TOOLTIP_LEN - 1);
	CreateThread(NULL, 0, messageLoop, NULL, 0, NULL);
}

VOID WINAPI hide_tray_icon()
{
	if (iconSet)
	{
		Shell_NotifyIcon(NIM_DELETE, &niData);
		iconSet = FALSE;
	}
}

VOID WINAPI add_tray_menu_item(int code, TCHAR *text)
{
	UINT flags = _tcsstr(text, _T("--"))? MF_SEPARATOR: MFT_STRING;
	AppendMenu(hMenu, flags, code, text);
}
