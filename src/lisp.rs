type LispResult<T> = emacs::Result<T>;
use anyhow::{anyhow};
fn rect_from_lisp(value: emacs::Value) -> LispResult<RECT> {
    let env = value.env;
    if !env.call("listp", [value])?.is_not_nil() {
        return env.signal(
            env.intern("wrong-type-argument")?,
            [value, env.intern("list")?],
        );
    }

    let mut rect = RECT::default();
    let mut list = value;
    rect.left = list.car()?;
    list = list.cdr()?;
    rect.top = list.car()?;
    list = list.cdr()?;
    rect.right = list.car()?;
    list = list.cdr()?;
    rect.bottom = list.car()?;
    Ok(rect)
}


pub fn with_webview<F, T>(wv_id: i64, f: F) -> LispResult<T>
where
    F: FnOnce(&mut Webview) -> LispResult<T>,
{
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id)
            .ok_or(anyhow!("no webview with id({wv_id}) found"))?;
        
        f(webview)
    })
}
macro_rules! plist_get {
    // 获取 Option<String> 类型的值
    ($env:expr, $options:expr, $key:expr, as Option<String>) => {
        $env.call("plist-get", ($options, $env.intern($key)?))?
            .into_rust::<Option<String>>()?
    };
    // 获取 bool 类型的值（基于 Lisp 的 nil 判断）
    ($env:expr, $options:expr, $key:expr, as bool) => {
        $env.call("plist-get", ($options, $env.intern($key)?))?
            .is_not_nil()
    };
}
#[defun(user_ptr)]
fn native_environment_new1(env: &Env, options: emacs::Value) -> LispResult<Environment>{
    let browser_executable_folder = plist_get!(env, options, ":browser-executable-folder", as Option<String>);
    let user_data_folder = plist_get!(env, options, ":user-data-folder", as Option<String>);
    let are_browser_extensions_enabled = plist_get!(env, options, ":are-browser-extensions-enabled", as bool);

    println!("browser_executable_folder = {browser_executable_folder:?}");
    println!("user_data_folder = {user_data_folder:?}");
    println!("are_browser_extensions_enabled = {are_browser_extensions_enabled:?}");
    Ok(Environment::new(browser_executable_folder, user_data_folder, are_browser_extensions_enabled))
}

#[defun]
fn native_webview_new(frame: emacs::Value, wenv: &Environment) -> LispResult<i64> {
    let hwnd = get_frame_hwnd(frame)?;
    // let bounds = rect_from_lisp(bounds)?;
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = Webview::new(hwnd, wenv);
        webviews.push(webview);
        let webview = webviews.last().unwrap();
        Ok(webview.id)
    })
}
#[defun]
fn native_webview_reparent(wv_id: i64, frame: emacs::Value) -> LispResult<()> {
    let hwnd = get_frame_hwnd(frame)?;
    with_webview(wv_id, |webview| {
        Ok(webview.reparent(hwnd))
    })
}
#[defun]
fn native_webview_resize(_env: &Env, wv_id: i64, bounds: Value) -> LispResult<()> {
    let bounds = rect_from_lisp(bounds)?;
    with_webview(wv_id, |webview| {
        Ok(webview.resize(bounds))
    })
}
#[defun]
fn native_webview_focus(_env: &Env, wv_id: i64) -> LispResult<()> {
    with_webview(wv_id, |webview: &mut Webview| {
        Ok(webview.focus())
    })
}
#[defun]
fn native_webview_load_sync(_env: &Env, wv_id: i64, url: String) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.load_sync(&url))
    })
}
#[defun]
fn native_webview_set_on_new_window_requested(_env: &Env, wv_id: i64, cb: Value) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.set_on_new_window_requested(cb))
    })
}
#[defun]
fn native_webview_set_on_focus(_env: &Env, wv_id: i64, cb: Value) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.set_on_focus(cb))
    })
}
#[defun]
fn native_webview_set_on_navigation_starting(_env: &Env, wv_id: i64, cb: Value) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.set_on_navigation_starting(cb))
    })
}
#[defun]
fn native_webview_set_on_history_changed(_env: &Env, wv_id: i64, cb: Value) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.set_on_history_changed(cb))
    })
}
#[defun]
fn native_webview_load(_env: &Env, wv_id: i64, url: String, cb: Value) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.load(&url, cb))
    })
}
#[defun]
fn native_webview_set_visible(_env: &Env, wv_id: i64, visible: Value) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.set_visible(visible.is_not_nil()))
    })
}
#[defun]
fn native_webview_is_visible(wv_id: i64) -> LispResult<bool> {
    with_webview(wv_id, |webview| {
        Ok(webview.visible())
    })
}
#[defun]
fn native_webview_get_document_title(wv_id: i64)-> LispResult<String> {
    with_webview(wv_id, |webview| {
        Ok(webview.get_document_title())
    }) 
}
#[defun]
fn native_webview_set_default_background_color(wv_id: i64, a: u8, r: u8, g:u8, b:u8)-> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.set_default_background_color(a, r, g, b))
    })
}
#[defun]
fn native_webview_get_url(wv_id: i64) -> LispResult<String> {
    with_webview(wv_id, |webview| {
        Ok(webview.get_url())
    })
}

#[defun]
fn native_webview_eval_js_sync<'a>(wv_id: i64, js: String) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.eval_js_sync(&js))
    })
}
#[defun]
fn native_webview_eval_js<'a>(wv_id: i64, js: String, cb: Value) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.eval_js_cdp(&js, cb))
    })
}

#[defun]
fn native_webview_close(_env: &Env, wv_id: i64) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.close();
        webviews.retain(|w| w.id != wv_id);
        Ok(())
    })
  
}
#[defun]
fn native_webview_open_task_manager(_env: &Env) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let webviews = webviews.borrow_mut();
        if webviews.len() > 0 {
            let webview = webviews.first().unwrap();
            webview.open_task_manager();
        }
    });
    Ok(())
}

#[defun]
fn native_webview_add_extension(_env: &Env, wv_id: i64, ext_path: String) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.add_extension(ext_path))
    })
}
#[defun]
fn native_webview_go_forward(wv_id: i64) -> LispResult<()> {
      with_webview(wv_id, |webview| {
        Ok(webview.go_forward())
    }) 
}
#[defun]
fn native_webview_go_back(wv_id: i64) -> LispResult<()> {
      with_webview(wv_id, |webview| {
        Ok(webview.go_back())
    }) 
}
#[defun]
fn native_webview_can_go_forward(wv_id: i64) -> LispResult<bool> {
      with_webview(wv_id, |webview| {
        Ok(webview.can_go_forward())
    }) 
}
#[defun]
fn native_webview_can_go_back(wv_id: i64) -> LispResult<bool> {
      with_webview(wv_id, |webview| {
        Ok(webview.can_go_back())
    }) 
}


// faster then (message)
#[defun]
fn native_print(message: String) -> LispResult<()> {
    println!("lisp: {}", message);
    Ok(())
}

/// init compositor for specific frame, will be called from elisp side
#[defun]
fn native_init_frame_thread_hook(env: &Env) -> LispResult<()> {
    // 所有 frame 共享一个 thread, 所以注册一次即可
    let frame = env.call("selected-frame", [])?;
    let hwnd = get_frame_hwnd(frame)?;

    setup_frame_thread_hook(hwnd);
    Ok(())
}
// fn eval_elisp<'a>(env: &'a Env, expr: &str) -> LispResult<Value<'a>> {
//     let sexp = env.call("read", (expr,))?;
//     env.call("eval", (sexp,))
// }
fn get_frame_hwnd(frame: emacs::Value)-> LispResult<HWND> {
    let env = frame.env;
    let hwnd_id: isize = env.call("frame-parameter", (frame, env.intern("window-id")?))?.into_rust::<String>()?.parse()?;
    Ok(HWND(hwnd_id as *mut libc::c_void))
}

#[defun]
fn native_process_events(env: &Env) -> LispResult<()> {
    EVENTS.with(|events| {
        let events = &mut *events.borrow_mut();
        while let Some(cb) = events.pop() {
            cb(env);
        }
    });
    CALLBACKS.with(|events| -> LispResult<()> {
        let events = &mut *events.borrow_mut();
        while let Some(Callback{rcb,  gcb, ..}) = events.pop() {
            rcb(env, gcb.bind(env))?
        }
        Ok(())
    })?;
    Ok(())
}
#[allow(unused_variables)]
fn window_features_to_lisp<'a>(env: &'a Env, features: &ICoreWebView2WindowFeatures) -> LispResult<emacs::Value<'a>>{
    unsafe {
        let has_position = {
            let mut rt = windows::core::BOOL::default();
            features.HasPosition(&mut rt).unwrap();
            println!("HasPosition = {rt:?}");
            rt.as_bool()
        };
        let has_size = {
            let mut rt = windows::core::BOOL::default();
            features.HasSize(&mut rt).unwrap();
            println!("HasSize = {rt:?}");
            rt.as_bool()
        };

        let should_display_menu_bar = {
            let mut rt = windows::core::BOOL::default();
            features.ShouldDisplayMenuBar(&mut rt).unwrap();
            println!("ShouldDisplayMenuBar = {rt:?}");
            rt.as_bool()
        };
        let should_display_status = {
            let mut rt = windows::core::BOOL::default();
            features.ShouldDisplayStatus(&mut rt).unwrap();
            println!("ShouldDisplayStatus = {rt:?}");
            rt.as_bool()
        };
        let should_display_toolbar = {
            let mut rt = windows::core::BOOL::default();
            features.ShouldDisplayToolbar(&mut rt).unwrap();
            println!("ShouldDisplayToolbar = {rt:?}");
            rt.as_bool()
        };
        let should_display_scroll_bars = {
            let mut rt = windows::core::BOOL::default();
            features.ShouldDisplayScrollBars(&mut rt).unwrap();
            println!("ShouldDisplayScrollBars = {rt:?}");
            rt.as_bool()
        };
        env.list((
            env.intern(":has-position")?, has_position,
            env.intern(":has-size")?, has_size,
            env.intern(":should-display-menu-bar")?, should_display_menu_bar,
            env.intern(":should-display-status")?, should_display_status,
            env.intern(":should-display-scroll-bars")?, should_display_scroll_bars,
        ))
    }

}

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name="ewv")]
fn init(_env: &Env) -> LispResult<()> {
    initialize_com();
    Ok(())
}
