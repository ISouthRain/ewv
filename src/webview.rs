use emacs::IntoLisp;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::mpsc;
use webview2_com::*;
use webview2_com_sys::Microsoft::Web::WebView2::Win32::*;
use windows::Win32::Foundation::*;
use windows::Win32::System::Com::{
    self, DISPATCH_FLAGS, DISPPARAMS, EXCEPINFO, IDispatch, IDispatch_Impl, ITypeInfo,
};
use windows::Win32::System::Ole::DISPID_UNKNOWN;
use windows::Win32::System::Variant::VARIANT;
use windows::Win32::UI::Input::KeyboardAndMouse::{self, VIRTUAL_KEY, VK_CONTROL, VK_MENU};
use windows::Win32::UI::Input::KeyboardAndMouse::{
    INPUT, INPUT_0, INPUT_KEYBOARD, KEYBDINPUT, KEYEVENTF_KEYUP, SendInput,
};
use windows::Win32::UI::WindowsAndMessaging::*;
use windows_core::h;
use windows_implement::implement;
#[implement(IDispatch)]
struct MyBridge;
// impl IDispatch_Impl for MyBridge_Impl {
//     // You then provide the specific logic for Invoke or GetIDsOfNames
//     // if the macro-generated default needs customization.
//     // Note: IDispatch implementation must map these to Invoke/GetIDsOfNames
fn trigger_callback(callback: IDispatch, lcid: u32, message: &str) {
    unsafe {
        // Prepare the argument VARIANT (VT_BSTR for a string)
        // let mut arg = VARIANT::default();
        // (Note: Use windows::core::BSTR for string arguments)
        let mut arg = VARIANT::from(message);

        let params = DISPPARAMS {
            rgvarg: &mut arg,
            cArgs: 1,
            ..Default::default()
        };
        let mut result = VARIANT::default();
        let mut excep_info = std::mem::zeroed();
        let mut arg_err = 0;

        callback
            .Invoke(
                0, // DISPID_VALUE (default method for a JS function)
                &windows::core::GUID::zeroed(),
                lcid,
                Com::DISPATCH_METHOD,
                &params,
                Some(&mut result),
                Some(&mut excep_info),
                Some(&mut arg_err),
            )
            .unwrap()
    }
}
impl MyBridge {
    fn say_hello(&self, name: String, lcid: u32, callback: IDispatch) -> String {
        // format!("Hello from Rust, {}!", name);
        println!("say_hello name={:?}", name);
        EVENTS.with(|events| {
            let events = &mut *events.borrow_mut();
            events.push(Box::new(move |env: &Env| {
                println!("call events say_hello");
                // env.message("hello from say_hello");
                // env.call("eval", [])
                let lisp_expr: emacs::Value = name.into_lisp(env).unwrap();
                // let lisp_expr: emacs::Value = "(+ 1 30)".into_lisp(env).unwrap();
                // let result = env.call("ewv--eval-string", [lisp_expr]).unwrap();
                let result = match env.call("ewv--eval-string", &[lisp_expr]) {
                    Err(error) => {
                        // Handle `buffer-read-only` error.
                        if let Some(emacs::ErrorKind::Signal { symbol, .. }) =
                            error.downcast_ref::<emacs::ErrorKind>()
                        {
                            unsafe {
                                println!(
                                    "signal error {:?}",
                                    symbol.value(env).into_rust::<String>().unwrap()
                                );
                            }
                            // let buffer_read_only = env.intern("buffer-read-only")?;
                            // // `symbol` is a `TempValue` that must be converted to `Value`.
                            // let symbol = unsafe { Ok(symbol.value(env)) };
                            // if env.eq(symbol, buffer_read_only) {
                            //     env.message("This buffer is not writable!")?;
                            //     return Ok(());
                            // }
                        }
                        if let Some(emacs::ErrorKind::Throw { tag, .. }) =
                            error.downcast_ref::<emacs::ErrorKind>()
                        {
                            unsafe {
                                println!(
                                    "throw error {:?}",
                                    tag.value(env).into_rust::<String>().unwrap()
                                );
                            }
                            // let buffer_read_only = env.intern("buffer-read-only")?;
                            // // `symbol` is a `TempValue` that must be converted to `Value`.
                            // let symbol = unsafe { Ok(symbol.value(env)) };
                            // if env.eq(symbol, buffer_read_only) {
                            //     env.message("This buffer is not writable!")?;
                            //     return Ok(());
                            // }
                        }

                        // Propagate other errors.
                        Err(error)
                    }
                    v => v,
                }
                .unwrap();
                // let result= env.call("message",[lisp_expr]).unwrap();
                // let result = env.("hello from say_hello").unwrap();
                // let result= env.call("read-from-string",[lisp_expr]).unwrap();
                println!("call events say_hello after tx_send");
                let rust_result: String = match result.into_rust::<String>() {
                    Err(error) => {
                        // Handle `buffer-read-only` error.
                        if let Some(emacs::ErrorKind::Signal { symbol, data }) =
                            error.downcast_ref::<emacs::ErrorKind>()
                        {
                            unsafe {
                                let sym_name = env
                                    .call(
                                        "format",
                                        ["%S".into_lisp(env).unwrap(), symbol.value(env)],
                                    )
                                    .unwrap()
                                    .into_rust::<String>()
                                    .unwrap();
                                let data_name = env
                                    .call("format", ["%S".into_lisp(env).unwrap(), data.value(env)])
                                    .unwrap()
                                    .into_rust::<String>()
                                    .unwrap();
                                println!("signal error = {:?} data={:?}", sym_name, data_name,);
                            }
                            // let buffer_read_only = env.intern("buffer-read-only")?;
                            // // `symbol` is a `TempValue` that must be converted to `Value`.
                            // let symbol = unsafe { Ok(symbol.value(env)) };
                            // if env.eq(symbol, buffer_read_only) {
                            //     env.message("This buffer is not writable!")?;
                            //     return Ok(());
                            // }
                        }
                        if let Some(emacs::ErrorKind::Throw { tag, .. }) =
                            error.downcast_ref::<emacs::ErrorKind>()
                        {
                            unsafe {
                                println!(
                                    "throw error {:?}",
                                    tag.value(env).into_rust::<String>().unwrap()
                                );
                            }
                            // let buffer_read_only = env.intern("buffer-read-only")?;
                            // // `symbol` is a `TempValue` that must be converted to `Value`.
                            // let symbol = unsafe { Ok(symbol.value(env)) };
                            // if env.eq(symbol, buffer_read_only) {
                            //     env.message("This buffer is not writable!")?;
                            //     return Ok(());
                            // }
                        }

                        // Propagate other errors.
                        // Err(error) => "erro"
                        "error".to_string()
                    }
                    v => v.unwrap(),
                };
                // let rust_result = result.into_rust::<String>().unwrap();
                trigger_callback(callback, lcid, &rust_result);
            }));
        });
        println!("create events");
        unsafe {
            PostMessageW(
                Some(HWND_BROADCAST),
                WM_INPUTLANGCHANGE,
                WPARAM(0),
                LPARAM(0),
            )
            .unwrap();
        }
        "world".to_string()
    }
}
impl IDispatch_Impl for MyBridge_Impl {
    fn GetTypeInfoCount(&self) -> windows_core::Result<u32> {
        println!("GetTypeInfoCount");
        Ok(0)
    }
    fn GetTypeInfo(&self, _itinfo: u32, _lcid: u32) -> windows_core::Result<ITypeInfo> {
        println!("GetTypeInfo");
        Err(windows::core::Error::from_hresult(
            windows::Win32::Foundation::E_NOTIMPL,
        ))
    }
    fn GetIDsOfNames(
        &self,
        _riid: *const windows_core::GUID,
        rgsznames: *const windows_core::PCWSTR,
        cnames: u32,
        _lcid: u32,
        rgdispid: *mut i32,
    ) -> windows_core::Result<()> {
        println!("GetIDOfNames");
        unsafe {
            let names = std::slice::from_raw_parts(rgsznames, cnames as usize);
            let dispids = std::slice::from_raw_parts_mut(rgdispid, cnames as usize);

            for (i, name) in names.iter().enumerate() {
                let name_str = name.to_string().expect("Invalid string");
                dispids[i] = match name_str.as_str() {
                    "say_hello" => 1,
                    _ => DISPID_UNKNOWN,
                };
            }
        }
        Ok(())
    }
    fn Invoke(
        &self,
        dispidmember: i32,
        _riid: *const windows_core::GUID,
        lcid: u32,
        wflags: DISPATCH_FLAGS,
        pdispparams: *const DISPPARAMS,
        _pvarresult: *mut VARIANT,
        _pexcepinfo: *mut EXCEPINFO,
        _puargerr: *mut u32,
    ) -> windows_core::Result<()> {
        println!("Invoke");
        if !wflags.contains(Com::DISPATCH_METHOD) {
            return Err(windows::core::Error::from_hresult(
                windows::Win32::Foundation::DISP_E_MEMBERNOTFOUND,
            ));
        }
        match dispidmember {
            1 => {
                // "say_hello"
                println!("Rust: say_hello was called!");
                // You would extract arguments from pdispparams here
                let _var = VARIANT::default();
                unsafe {
                    // 1. 将原始指针转换为 Rust 引用
                    let params = {
                        if pdispparams.is_null() {
                            return Err(windows::core::Error::from_hresult(
                                windows::Win32::Foundation::DISP_E_MEMBERNOTFOUND,
                            ));
                        }
                        &*pdispparams
                    };

                    let arg_count = params.cArgs as usize;

                    // 2. 将 VARIANT 数组指针转换为切片
                    // 注意：rgvarg 存储的是从最后一个参数到第一个参数
                    let args: &[VARIANT] = {
                        if params.rgvarg.is_null() && arg_count > 0 {
                            return Err(DISP_E_BADPARAMCOUNT.into());
                        }
                        std::slice::from_raw_parts(params.rgvarg, arg_count)
                    };

                    // 3. 访问具体的参数（假设 JS 调用了 func(a, b)）
                    if arg_count == 2 {
                        // JS 中的第一个参数 'a' 在数组末尾
                        let arg_a = &args[arg_count - 1];
                        let value_a: String = arg_a.to_string();
                        println!("Argument A: {}", value_a);
                        // JS 中的第二个参数 'b' 在数组倒数第二个
                        let arg_b = &args[arg_count - 2];
                        // let jscb: Variant = VARIANT::try_into(*arg_b).unwrap();
                        if let Ok(callback) = arg_b
                            .Anonymous
                            .Anonymous
                            .Anonymous
                            .pdispVal
                            .as_ref()
                            .unwrap()
                            .cast::<IDispatch>()
                        {
                            self.say_hello(value_a, lcid, callback);
                        }
                        // 4. 转换类型 (例如转为字符串)
                        // 使用 windows-rs 的从 VARIANT 转换的方法
                        // let rt = self.say_hello(value_a);
                        // let var = VARIANT::from(rt.as_str());

                        // 5. 写入返回指针
                        // *pvarresult = var;
                    }
                }
                Ok(())
            }
            _ => Err(windows::core::Error::from_hresult(
                windows::Win32::Foundation::DISP_E_MEMBERNOTFOUND,
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Webview {
    id: i64,
    hwnd: isize,
    raw: ICoreWebView2,
    bounds: RECT,
    controller: ICoreWebView2Controller,
}
fn notify_emacs_for_webview(webview: &ICoreWebView2) {
    WEBVIEWS.with(|webviews| {
        let webviews = webviews.borrow();
        let wv = webviews.iter().find(|w| &w.raw == webview).unwrap();
        let hwnd = HWND(wv.hwnd as *mut libc::c_void);
        unsafe {
            PostMessageW(Some(hwnd), WM_INPUTLANGCHANGE, WPARAM(0), LPARAM(0)).unwrap();
        }
    });
}
impl Webview {
    pub fn new(hwnd_id: isize, bounds: RECT) -> Self {
        let hwnd = HWND(hwnd_id as *mut libc::c_void);
        let environment = {
            let (tx, rx) = mpsc::channel();

            CreateCoreWebView2EnvironmentCompletedHandler::wait_for_async_operation(
                Box::new(|environmentcreatedhandler| unsafe {
                    let env_options = CoreWebView2EnvironmentOptions::default();
                    env_options.set_are_browser_extensions_enabled(true);
                    let env_options: ICoreWebView2EnvironmentOptions = env_options.into();
                    // let user_data_folder = h!(r"C:\Users\xlzhang\AppData\Local\Microsoft\Edge\User Data");
                    let user_data_folder = None;
                    // let user_data_folder = h!(r"C:\Users\xlzhang\Downloads\mingw-w64-ucrt-x86_64_emacs_f3d278f9_mps_full\bin\");
                    // let browser_executable_folder = h!(r"C:\Program Files (x86)\Microsoft\Edge\Application\144.0.3719.92");
                    // let browser_executable_folder = h!(r"C:\Users\xlzhang\AppData\Local\Microsoft\Edge SxS\Application\146.0.3827.0");
                    let browser_executable_folder = None;
                    CreateCoreWebView2EnvironmentWithOptions(
                        browser_executable_folder,
                        user_data_folder,
                        &env_options,
                        &environmentcreatedhandler,
                    )
                    .map_err(webview2_com::Error::WindowsError)
                }),
                Box::new(move |error_code, environment| {
                    error_code?;
                    tx.send(environment.ok_or_else(|| windows::core::Error::from(E_POINTER)))
                        .expect("send over mpsc channel");
                    Ok(())
                }),
            )
            .unwrap();

            rx.recv().unwrap().unwrap()
        };

        let c2 = {
            let (tx, rx) = mpsc::channel();

            CreateCoreWebView2ControllerCompletedHandler::wait_for_async_operation(
                Box::new(move |handler| unsafe {
                    let env3: ICoreWebView2Environment10 = environment.cast().unwrap();
                    let mut version = PWSTR::default();
                    env3.BrowserVersionString(&mut version).unwrap();
                    println!("webview2 version = {:?}", version.to_string());
                    let options = env3.CreateCoreWebView2ControllerOptions().unwrap();
                    let options: ICoreWebView2ControllerOptions4 = options.cast().unwrap();
                    // options.SetAllowHostInputProcessing(true).unwrap();
                    env3.CreateCoreWebView2ControllerWithOptions(hwnd, &options, &handler)
                        .map_err(webview2_com::Error::WindowsError)
                }),
                Box::new(move |error_code, controller| {
                    error_code?;
                    tx.send(controller.ok_or_else(|| windows::core::Error::from(E_POINTER)))
                        .expect("send over mpsc channel");
                    Ok(())
                }),
            )
            .unwrap();
            rx.recv().unwrap().unwrap()
        };

        let controller: ICoreWebView2Controller = c2.cast().unwrap();
        let mut client_rect = RECT::default();
        unsafe {
            let _ = WindowsAndMessaging::GetClientRect(hwnd, &mut client_rect);
            controller.SetBounds(bounds).unwrap();
            controller.SetIsVisible(true).unwrap();
        }

        let webview = unsafe { controller.CoreWebView2().unwrap() };

        unsafe {
            webview
                .add_NavigationStarting(
                    &NavigationStartingEventHandler::create(Box::new(move |_, args| {
                        // // Inspect URI
                        let mut uri = PWSTR::default();
                        args.unwrap().Uri(&mut uri)?;
                        println!("Navigating to: {:?}", uri.to_string());
                        Ok(())
                    })),
                    &mut 0, // token placeholder
                )
                .unwrap();
        }
        let mut _token = 0;
        (unsafe {
            webview.add_NewWindowRequested(
                &NewWindowRequestedEventHandler::create(Box::new(move |webview, args| {
                    if let Some(args) = args {
                        // args.SetHandled(true).unwrap();
                        // let mut uri = windows::core::PWSTR::null();
                        // args.Uri(&mut uri).unwrap();
                        // webview.as_ref().unwrap().Navigate(uri).unwrap();

                        let mut uri = windows::core::PWSTR::null();
                        args.Uri(&mut uri).unwrap();
                        let deferral = args.GetDeferral().unwrap();
                        EVENTS.with(|events| {
                            let events = &mut *events.borrow_mut();
                            events.push(Box::new(move |env: &Env| {
                                let lisp_expr = format!(
                                    "(ewv-browser-open-url \"{}\")",
                                    uri.to_string().unwrap()
                                );
                                println!("lisp_expr = {}", lisp_expr);
                                let lisp_expr: emacs::Value = lisp_expr.into_lisp(env).unwrap();
                                let _result = env.call("ewv--eval-string", [lisp_expr]).unwrap();
                                args.SetHandled(true).unwrap();
                                deferral.Complete().unwrap();
                            }));
                        });
                        notify_emacs_for_webview(&webview.unwrap());
                        // args.SetNewWindow(webview.as_ref().unwrap()).unwrap();
                    }
                    Ok(())
                })),
                &mut _token,
            )
        })
        .unwrap();
        unsafe {
            let controller: ICoreWebView2Controller = c2.cast().unwrap();
            controller
                .add_AcceleratorKeyPressed(
                    &AcceleratorKeyPressedEventHandler::create(Box::new(move |_, args| {
                        let args = args.unwrap();
                        let mut key_kind = COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN;
                        args.KeyEventKind(&mut key_kind)?;

                        let mut virtual_key = 0;
                        args.VirtualKey(&mut virtual_key)?;
                        let mut key_status = COREWEBVIEW2_PHYSICAL_KEY_STATUS::default();

                        args.PhysicalKeyStatus(&mut key_status)?;

                        // println!("virtual key = {:?}", virtual_key);
                        // Example: Intercept F3
                        match VIRTUAL_KEY(virtual_key as u16) {
                            KeyboardAndMouse::VK_G => {
                                // VK_F3
                                // Mark as handled to prevent WebView from using it
                                let hwnd = GetActiveWindow();
                                SetFocus(Some(hwnd)).unwrap();
                                args.SetHandled(true).unwrap();
                            }
                            KeyboardAndMouse::VK_X => {
                                let hwnd = GetActiveWindow();
                                SetFocus(Some(hwnd)).unwrap();
                                if key_status.IsMenuKeyDown.into() {
                                    send_keycomb(VK_MENU, KeyboardAndMouse::VK_X);
                                } else {
                                    send_keycomb(VK_CONTROL, KeyboardAndMouse::VK_X);
                                }
                                args.SetHandled(true).unwrap();
                            }

                            _ => {
                                args.SetHandled(false).unwrap();
                            }
                        }
                        Ok(())
                    })),
                    &mut 0,
                )
                .unwrap();
        }
        unsafe {
            let handler =
                WebMessageReceivedEventHandler::create(Box::new(move |_webview, args| {
                    let mut rt = PWSTR::default();
                    args.unwrap().WebMessageAsJson(&mut rt).unwrap();
                    println!("rt= {:?}", rt.to_string());
                    Ok(())
                }));
            webview.add_WebMessageReceived(&handler, &mut 0).unwrap()
        }

        unsafe {
            // Assume `webview` is an instance of ICoreWebView2
            let bridge_object: IDispatch = MyBridge.into();
            webview
                .AddHostObjectToScript(pwstr_from_str("bridge"), &mut bridge_object.into())
                .expect("Failed to add host object");
        }
        unsafe {
            let settings = webview.Settings().unwrap();
            settings.SetAreDefaultContextMenusEnabled(true).unwrap();
            settings.SetAreDevToolsEnabled(true).unwrap();
        }
        // unsafe {
        // webview.Navigate(h!("https://www.baidu.com")).unwrap();
        // webview.Navigate(h!("edge://gpu")).unwrap();
        // webview
        //     .Navigate(h!("file:///C:/Users/xlzhang/tmp/greeting/modeline.html"))
        //     .unwrap();
        // webview.Navigate(h!("file:///C:/Users/xlzhang/Downloads/FOSDEM19_emacs_modules.pdf")).unwrap();
        // webview.Navigate(h!("extension://aajlcoiaogpknhgninhopncaldipjdnp/pages/pdf_viewer.html?file=file:///C:/Users/xlzhang/Downloads/FOSDEM19_emacs_modules.pdf")).unwrap();
        // webview.Navigate(h!("extension://aajlcoiaogpknhgninhopncaldipjdnp/pages/options.html")).unwrap();
        // webview.Navigate(h!("extension://kgnghhfkloifoabeaobjkgagcecbnppg/pages/neovim.html")).unwrap();
        // }

        unsafe {
            webview
                .Settings()
                .unwrap()
                .cast::<ICoreWebView2Settings3>()
                .unwrap()
                .SetAreBrowserAcceleratorKeysEnabled(true)
                .unwrap();
            webview
                .Settings()
                .unwrap()
                .cast::<ICoreWebView2Settings3>()
                .unwrap()
                .SetAreDefaultContextMenusEnabled(true)
                .unwrap();
        }

        let _wv = webview.clone();
        // NavigationCompletedEventHandler::wait_for_async_operation(
        //     Box::new(move |handler| unsafe {
        //         wv.add_NavigationCompleted(&handler, &mut 0).unwrap();
        //         Ok(())
        //     }),
        //     Box::new(|a, _| Ok(())),
        // )
        // .unwrap();
        Webview {
            id: new_webview_id(),
            hwnd: hwnd_id,
            bounds,
            raw: webview,
            controller,
        }
    }
    pub fn add_extension(&self, ext_path: String) {
        let webview: ICoreWebView2_28 = self.raw.cast().unwrap();
        let profile = unsafe {
            webview
                .Profile()
                .unwrap()
                .cast::<ICoreWebView2Profile7>()
                .unwrap()
        };
        ProfileAddBrowserExtensionCompletedHandler::wait_for_async_operation(
            Box::new(move |handler| unsafe {
                // let ext_path = h!(r"C:\Users\xlzhang\fun\Surfingkeys\dist\development\chrome");
                let ext_path = pwstr_from_str(&ext_path);
                profile.AddBrowserExtension(ext_path, &handler).unwrap();
                Ok(())
            }),
            Box::new(move |_error_code, extension| unsafe {
                let mut name = PWSTR::default();
                let extension = extension.unwrap();
                extension.Name(&mut name).unwrap();
                let mut id = PWSTR::default();
                extension.Id(&mut id).unwrap();
                println!(
                    "added extension name {:?} id {:?}",
                    name.to_string(),
                    id.to_string()
                );
                Ok(())
            }),
        )
        .unwrap();
    }
    pub fn update_position(&self) {
        unsafe {
            self.controller.NotifyParentWindowPositionChanged().unwrap();
        }
    }
    pub fn focus(&self) {
        unsafe {
            self.controller
                .MoveFocus(COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC)
                .unwrap();
        }
    }
    pub fn update_mouse(&self, _msg: &MSG) {
        // let (x, y) = get_mouse_x_y(msg.lParam);
        // if x < self.x || x >= self.x + self.w || y < self.y || y >= self.y + self.h {
        //     return;
        // }
        // let x = x - self.x;
        // let y = y - self.y;
        // // Placeholder implementation
        // println!(
        //     "Webview at HWND: {:x} received mouse event: msg_id: {}, wParam: {}, lParam: {}",
        //     self.hwnd, msg.message, msg.wParam.0, msg.lParam.0
        // );
        // let event_kind = COREWEBVIEW2_MOUSE_EVENT_KIND(msg.message as i32);
        // let mut mousedata = 0;
        // if msg.message == WindowsAndMessaging::WM_MOUSEWHEEL {
        //     mousedata = ((msg.wParam.0 >> 16) & 0xFFFF) as u32;
        // }
        // let c2: ICoreWebView2CompositionController = self.controller.cast().unwrap();
        // match event_kind {
        //     COREWEBVIEW2_MOUSE_EVENT_KIND_X_BUTTON_UP => unsafe {
        //         if ((msg.wParam.0 >> 16) & 0xFFFF) == XBUTTON1.into() {
        //             self.controller.CoreWebView2().unwrap().GoBack().unwrap();
        //         } else {
        //             self.controller.CoreWebView2().unwrap().GoForward().unwrap();
        //         }
        //     },
        //     _ => unsafe {
        //         c2.SendMouseInput(
        //             event_kind,
        //             COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS((msg.wParam.0 & 0xFFFF) as i32),
        //             mousedata,
        //             POINT { x, y },
        //         )
        //         .unwrap();
        //     },
        // }
        // println!("Sent mouse input to WebView: eventKind={:?}", event_kind);
        // println!("x={}, y={}", x, y);
    }
    pub fn resize(&mut self, bounds: RECT) {
        unsafe {
            self.bounds = bounds;
            self.controller.SetBounds(bounds).unwrap();
        }
    }
    pub fn set_visible(&self, visible: bool) {
        // println!("webvie set visible {:?}", visible);
        unsafe {
            self.controller.SetIsVisible(visible).unwrap();
        }
    }
    pub fn visible(&self) -> bool {
        let mut visible = FALSE;
        unsafe {
            self.controller.IsVisible(&mut visible).unwrap();
        }
        visible.into()
    }
    pub fn load(&self, url: &str) {
        unsafe { self.raw.Navigate(pwstr_from_str(url)).unwrap() }
        let wv = self.raw.clone();
        NavigationCompletedEventHandler::wait_for_async_operation(
            Box::new(move |handler| unsafe {
                wv.add_NavigationCompleted(&handler, &mut 0).unwrap();
                Ok(())
            }),
            Box::new(|_a, _| Ok(())),
        )
        .unwrap();
    }
    pub fn open_task_manager(&self) {
        let webview: ICoreWebView2_6 = self.raw.cast().unwrap();
        unsafe {
            webview.OpenTaskManagerWindow().unwrap();
        }
    }
    pub fn close(&self) {
        unsafe {
            self.controller.Close().unwrap();
        }
    }
    pub fn reparent(&mut self, hwnd_id: isize) {
        let hwnd = HWND(hwnd_id as *mut libc::c_void);
        unsafe {
            self.controller.SetParentWindow(hwnd).unwrap();
            self.controller.NotifyParentWindowPositionChanged().unwrap();
        }
        self.hwnd = hwnd_id;
    }
    pub fn eval_js_sync(&self, js: &str) {
        let js = pwstr_from_str(js);
        let webview: ICoreWebView2_21 = self.raw.cast().unwrap();
        ExecuteScriptWithResultCompletedHandler::wait_for_async_operation(
            Box::new(move |handler| unsafe { Ok(webview.ExecuteScriptWithResult(js, &handler)?) }),
            Box::new(|_a, b| unsafe {
                let b = b.unwrap();
                let mut rt = PWSTR::default();
                b.ResultAsJson(&mut rt).unwrap();
                let rt = rt.to_string().unwrap();
                println!("rt = {:?}", rt);
                Ok(())
            }),
        )
        .unwrap();
    }
    pub fn eval_js(&self, js: &str, cb: Value) {
        let js = pwstr_from_str(js);
        let raw = self.raw.clone();
        let gcb = cb.make_global_ref();
        println!("eval_js start");
        let webview: ICoreWebView2_21 = raw.cast().unwrap();
        let webview2 = webview.clone();
        unsafe {
            webview
                .ExecuteScriptWithResult(
                    js,
                    &ExecuteScriptWithResultCompletedHandler::create(Box::new(move |_a, b| {
                        let b = b.unwrap();
                        let mut rt = PWSTR::default();
                        b.ResultAsJson(&mut rt).unwrap();
                        let rt = rt.to_string().unwrap();
                        println!("rt = {:?}", rt);
                        EVENTS.with(|events| {
                            let events = &mut *events.borrow_mut();
                            events.push(Box::new(move |env: &Env| {
                                let cb2 = gcb.bind(env);
                                env.call(cb2, (rt,)).unwrap();
                                gcb.free(env).unwrap();
                            }));
                        });
                        notify_emacs_for_webview(&webview2);
                        Ok(())
                    })),
                )
                .unwrap();
        }
        println!("eval_js end");
    }
    pub fn eval_js_cdp(&self, js: &str, _cb: Value) {
        let raw = self.raw.clone();
        let method = pwstr_from_str("Runtime.evaluate");

        use serde::Serialize;
        #[derive(Serialize)]
        #[allow(non_snake_case)]
        struct CDPMethodOptions {
            expression: String,
            awaitPromise: bool,
        }
        let options = CDPMethodOptions {
            expression: js.to_string(),
            awaitPromise: true,
        };
        let options = serde_json::to_string(&options).unwrap();
        let options = pwstr_from_str(&options);
        unsafe {
            raw.CallDevToolsProtocolMethod(
                method,
                options,
                &CallDevToolsProtocolMethodCompletedHandler::create(Box::new(
                    move |_hresult, result| {
                        println!("result = {}", result);
                        Ok(())
                    },
                )),
            )
            .unwrap();
        }
    }
}

/// Generates a unique, monotonically increasing ID within the process
/// use i64 for easier interop with lisp
fn new_webview_id() -> i64 {
    static NEXT_WEBVIEW_ID: AtomicI64 = AtomicI64::new(1);
    NEXT_WEBVIEW_ID.fetch_add(1, Ordering::Relaxed)
}

fn send_keycomb(modifier: VIRTUAL_KEY, key: VIRTUAL_KEY) {
    unsafe {
        let mut inputs = [
            // Ctrl down
            create_key_input(modifier.0, false),
            // X down
            create_key_input(key.0, false),
            // X up
            create_key_input(key.0, true),
            // Ctrl up
            create_key_input(modifier.0, true),
        ];

        SendInput(&mut inputs, size_of::<INPUT>() as i32);
    }
}
unsafe fn create_key_input(vk: u16, is_up: bool) -> INPUT {
    INPUT {
        r#type: INPUT_KEYBOARD,
        Anonymous: INPUT_0 {
            ki: KEYBDINPUT {
                wVk: windows::Win32::UI::Input::KeyboardAndMouse::VIRTUAL_KEY(vk),
                wScan: 0,
                dwFlags: if is_up {
                    KEYEVENTF_KEYUP
                } else {
                    Default::default()
                },
                time: 0,
                dwExtraInfo: 0,
            },
        },
    }
}
