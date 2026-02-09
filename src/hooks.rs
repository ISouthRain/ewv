unsafe extern "system" fn win32_frame_hook(
    n_code: i32,
    w_param: WPARAM,
    l_param: LPARAM,
) -> LRESULT {
    let msg = unsafe { &mut *(l_param.0 as *mut WindowsAndMessaging::MSG) };
    match msg.message {
        WindowsAndMessaging::WM_MOUSEFIRST..=WindowsAndMessaging::WM_MOUSELAST => {
            if msg.message == WindowsAndMessaging::WM_LBUTTONDOWN {
                unsafe {
                    let _ = SetFocus(Some(msg.hwnd));
                }
            }
        }
        WindowsAndMessaging::WM_KEYDOWN => {
            // println!("wm_keydown");
        }
        _ => {}
    }
    unsafe { WindowsAndMessaging::CallNextHookEx(None, n_code, w_param, l_param) }
}


fn setup_frame_hook(hwnd: isize) {
    let hwnd = HWND(hwnd as *mut libc::c_void);
    let hook_proc = win32_frame_hook;
    let hook_id = WindowsAndMessaging::WH_GETMESSAGE;
    unsafe {
        let hwnd_tid = GetWindowThreadProcessId(hwnd, None);
        let h_instance = get_dll_hinstance();
        let _hook_handle = WindowsAndMessaging::SetWindowsHookExW(
            hook_id,
            Some(hook_proc),
            Some(h_instance),
            hwnd_tid, // 全局钩子，设置为0
        )
        .unwrap();
    }
}

// A function to get the stored HINSTANCE from within the DLL
#[unsafe(no_mangle)]
pub fn get_dll_hinstance() -> HINSTANCE {
    let mut module = HMODULE::default();
    unsafe {
        GetModuleHandleExW(
            GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
            PCWSTR(get_dll_hinstance as *mut u16),
            &mut module,
        )
        .unwrap();
    }
    module.into()
}