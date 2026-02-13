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
    with_webview(wv_id, |webview| {
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
fn native_webview_process_events(env: &Env) -> LispResult<i64> {
    EVENTS.with(|events| {
        let events = &mut *events.borrow_mut();
        while let Some(cb) = events.pop() {
            cb(env);
        }
    });
    CALLBACKS.with(|events| {
        let events = &mut *events.borrow_mut();
        while let Some(Callback{rcb,  gcb, ..}) = events.pop() {
            rcb(env, gcb.bind(env));
        }
    });
    Ok(0)
}
#[defun]
fn native_webview_add_extension(_env: &Env, wv_id: i64, ext_path: String) -> LispResult<()> {
    with_webview(wv_id, |webview| {
        Ok(webview.add_extension(ext_path))
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
fn native_init_for_frame<'a>(hwnd: isize) -> LispResult<()> {
    setup_frame_hook(hwnd);
    Ok(())
}

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name="ewv")]
fn init(_env: &Env) -> LispResult<()> {
    initialize_com();
    Ok(())
}
