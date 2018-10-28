use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::mem;
use vm::{ArrayValue, CallObject, VMState, Value, ValueBase};

pub trait Gc {
    fn free(&self);
}

impl Gc for HashMap<String, Value> {
    fn free(&self) {
        mem::drop(self);
    }
}
impl Gc for CallObject {
    fn free(&self) {
        mem::drop(self);
    }
}
impl Gc for ArrayValue {
    fn free(&self) {
        mem::drop(self);
    }
}

#[derive(Clone, Debug, Eq)]
pub struct GcPtr(*mut Gc);

impl PartialEq for GcPtr {
    fn eq(&self, other: &GcPtr) -> bool {
        self.0 as *mut u8 == other.0 as *mut u8
    }
}
use std::hash::{Hash, Hasher};
impl Hash for GcPtr {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u64(self.0 as *mut libc::c_void as u64);
        state.finish();
    }
}

// impl Eq for GcPtr {}

thread_local!(pub static GC_MEM: RefCell<HashSet<GcPtr>> = {
    RefCell::new(HashSet::new())
});

pub fn gc_new<X: Gc + 'static>(data: X) -> *mut X {
    let ptr = Box::into_raw(Box::new(data));
    GC_MEM.with(|m| m.borrow_mut().insert(GcPtr(ptr)));
    ptr
}
pub fn gc_free(marked: &HashSet<GcPtr>) {
    GC_MEM.with(|mem| {
        mem.borrow_mut().retain(|p| {
            let mut is_marked = marked.contains(p);
            if !is_marked {
                unsafe {
                    (*p.0).free();
                    Box::from_raw(p.0);
                }
            }
            is_marked
        });
    });
}
pub fn gc_trace(vm_state: &VMState, marked: &mut HashSet<GcPtr>) {
    for val in &vm_state.stack {
        gc_trace_value(val, marked);
    }
    for scope in &vm_state.scope {
        unsafe {
            marked.insert(GcPtr(*scope));
            gc_trace_callobj(&**scope, marked);
        }
    }
}
fn gc_trace_callobj(callobj: &CallObject, marked: &mut HashSet<GcPtr>) {
    unsafe {
        if marked.insert(GcPtr((*callobj).vals)) {
            gc_trace_hm((*callobj).vals, marked);
        }
        for val in &(*callobj).arg_rest_vals {
            gc_trace_value(val, marked);
        }
        if let Some(parent) = (*callobj).parent {
            if marked.insert(GcPtr(parent)) {
                gc_trace_callobj(&*parent, marked);
            }
        }
        gc_trace_value(&*(*callobj).this, marked);
    }
}
fn gc_trace_value(val: &Value, marked: &mut HashSet<GcPtr>) {
    match val.val {
        ValueBase::Undefined => {}
        ValueBase::Bool(_) => {}
        ValueBase::Number(_) => {}
        ValueBase::String(_) => {}
        ValueBase::Function(_, ref obj, ref c) => {
            if marked.insert(GcPtr(*obj)) {
                gc_trace_hm(*obj, marked);
            }
            gc_trace_callobj(c, marked);
        }
        ValueBase::BuiltinFunction(_, ref c) => gc_trace_callobj(c, marked),
        ValueBase::Object(ref obj) => {
            if marked.insert(GcPtr(*obj)) {
                gc_trace_hm(*obj, marked);
            }
        }
        ValueBase::Array(ref a) => {
            if marked.insert(GcPtr(*a)) {
                gc_trace_aryval(*a, marked)
            }
        }
        ValueBase::Arguments => {}
    }
}
fn gc_trace_hm(hm: *mut HashMap<String, Value>, marked: &mut HashSet<GcPtr>) {
    unsafe {
        for (_, val) in &*hm {
            gc_trace_value(val, marked);
        }
    }
}
fn gc_trace_aryval(aryval: *mut ArrayValue, marked: &mut HashSet<GcPtr>) {
    unsafe {
        let aryval = &*aryval;
        for val in &aryval.elems {
            gc_trace_value(val, marked)
        }
        for (_, val) in &aryval.obj {
            gc_trace_value(val, marked)
        }
    }
}
