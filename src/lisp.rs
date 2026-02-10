type LispResult<T> = emacs::Result<T>;

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
#[defun]
fn native_webview_new(hwnd: isize, bounds: emacs::Value) -> LispResult<i64> {
    let bounds = rect_from_lisp(bounds)?;
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = Webview::new(hwnd, bounds);
        webviews.push(webview);
        let webview = webviews.last().unwrap();
        Ok(webview.id)
    })
}
#[defun]
fn native_webview_reparent(wv_id: i64, hwnd: isize) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.reparent(hwnd);
    });
    Ok(())
}
#[defun]
fn native_webview_resize(_env: &Env, wv_id: i64, bounds: Value) -> LispResult<()> {
    let bounds = rect_from_lisp(bounds)?;
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.resize(bounds)
    });
    Ok(())
}
#[defun]
fn native_webview_focus(_env: &Env, wv_id: i64) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.focus()
    });
    Ok(())
}
#[defun]
fn native_webview_load_sync(_env: &Env, wv_id: i64, url: String) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.load_sync(&url)
    });
    Ok(())
}

#[defun]
fn native_webview_load(_env: &Env, wv_id: i64, url: String, cb: Value) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.load(&url, cb)
    });
    Ok(())
}
#[defun]
fn native_webview_set_visible(_env: &Env, wv_id: i64, visible: Value) -> Result<i64> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.set_visible(visible.is_not_nil());
    });
    Ok(0)
}
#[defun]
fn native_webview_is_visible(wv_id: i64) -> LispResult<bool> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        Ok(webview.visible())
    })
}
#[defun]
fn native_webview_eval_js_sync<'a>(wv_id: i64, js: String) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.eval_js_sync(&js);
    });
    Ok(())
}
#[defun]
fn native_webview_eval_js<'a>(wv_id: i64, js: String, cb: Value) -> LispResult<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.eval_js_cdp(&js, cb);
    });
    Ok(())
}

#[defun]
fn native_webview_close(_env: &Env, wv_id: i64) -> Result<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.close();
        webviews.retain(|w| w.id != wv_id);
    });
    Ok(())
}
#[defun]
fn native_webview_open_task_manager(_env: &Env) -> Result<()> {
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
fn native_webview_process_events(env: &Env) -> Result<i64> {
    EVENTS.with(|events| {
        let events = &mut *events.borrow_mut();
        while let Some(cb) = events.pop() {
            cb(env);
        }
    });
    Ok(0)
}
#[defun]
fn native_webview_add_extension(_env: &Env, wv_id: i64, ext_path: String) -> Result<()> {
    WEBVIEWS.with(|webviews| {
        let mut webviews = webviews.borrow_mut();
        let webview = webviews.iter_mut().find(|w| w.id == wv_id).unwrap();
        webview.add_extension(ext_path);
    });
    Ok(())
}

// faster then (message)
#[defun]
fn native_print(message: String) -> LispResult<()> {
    println!("lisp: {}", message);
    Ok(())
}

/// init compositor for specific frame, will be called from elisp side
#[defun]
fn native_init_for_frame<'a>(hwnd: isize) -> LispResult<()> {
    setup_frame_hook(hwnd);
    Ok(())
}

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(_env: &Env) -> LispResult<()> {
    initialize_com();
    Ok(())
}
