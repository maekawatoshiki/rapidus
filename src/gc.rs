use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::mem;
use std::sync::atomic::{self, AtomicUsize};
use vm::{ArrayValue, CallObject, VMState, Value, ValueBase};

static ALLOCATED_MEM_SIZE_BYTE: AtomicUsize = AtomicUsize::new(0);

thread_local!(pub static GC_MEM: RefCell<FxHashSet<GcPtr>> = {
    RefCell::new(FxHashSet::default())
});

#[derive(Clone, Debug, Eq)]
pub struct GcPtr(*mut Gc);

impl PartialEq for GcPtr {
    fn eq(&self, other: &GcPtr) -> bool {
        self.0 as *mut u8 == other.0 as *mut u8
    }
}

impl Hash for GcPtr {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u64(self.0 as *mut libc::c_void as u64);
        state.finish();
    }
}

pub trait Gc {
    fn free(&self);
    fn trace(&self, &mut FxHashSet<GcPtr>);
}

impl Gc for Value {
    fn free(&self) {
        mem::drop(self);
    }

    fn trace(&self, marked: &mut FxHashSet<GcPtr>) {
        match self.val {
            ValueBase::Undefined => {}
            ValueBase::Bool(_) => {}
            ValueBase::Number(_) => {}
            ValueBase::String(_) => {}
            ValueBase::Function(box (_, _, ref obj, ref c)) => {
                not_marked_then(*obj, marked, |obj, marked| unsafe {
                    (*obj).trace(marked);
                });
                c.trace(marked);
            }
            // Never trace _xxx
            ValueBase::BuiltinFunction(box (_, _x, ref c)) => c.trace(marked),
            ValueBase::Object(ref obj) => {
                not_marked_then(*obj, marked, |obj, marked| unsafe {
                    (*obj).trace(marked);
                });
            }
            ValueBase::Array(ref a) => {
                not_marked_then(*a, marked, |a, marked| unsafe {
                    (*a).trace(marked);
                });
            }
            ValueBase::Arguments => {}
        }
    }
}

impl Gc for FxHashMap<String, Value> {
    fn free(&self) {
        mem::drop(self);
    }

    fn trace(&self, marked: &mut FxHashSet<GcPtr>) {
        for (_, val) in self {
            val.trace(marked);
        }
    }
}

impl Gc for CallObject {
    fn free(&self) {
        mem::drop(self);
    }

    fn trace(&self, marked: &mut FxHashSet<GcPtr>) {
        unsafe {
            not_marked_then(self.vals, marked, |vals, marked| {
                (*vals).trace(marked);
            });
            for val in &self.arg_rest_vals {
                val.trace(marked);
            }
            if let Some(parent) = self.parent {
                not_marked_then(parent, marked, |parent, marked| {
                    (*parent).trace(marked);
                })
            }
            self.this.trace(marked);
        }
    }
}

impl Gc for ArrayValue {
    fn free(&self) {
        mem::drop(self);
    }

    fn trace(&self, marked: &mut FxHashSet<GcPtr>) {
        for val in &self.elems {
            val.trace(marked)
        }
        for (_, val) in &self.obj {
            val.trace(marked)
        }
    }
}

pub fn new<X: Gc + 'static>(data: X) -> *mut X {
    let data_size = mem::size_of_val(&data);
    let ptr = Box::into_raw(Box::new(data));
    ALLOCATED_MEM_SIZE_BYTE.fetch_add(data_size, atomic::Ordering::SeqCst);
    GC_MEM.with(|m| m.borrow_mut().insert(GcPtr(ptr)));
    ptr
}

// pub fn gc_new_and_free<X: Gc + 'static>(data: X, vm_state: &VMState) -> *mut X {
//     let mut marked = FxHashSet::default();
//     trace(&vm_state, &mut marked);
//     free(&marked);
//
//     let ptr = Box::into_raw(Box::new(data));
//     GC_MEM.with(|m| m.borrow_mut().insert(GcPtr(ptr)));
//     ptr
// }

pub fn mark_and_sweep(vm_state: &VMState) {
    fn over16kb_allocated() -> bool {
        ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst) > 16 * 1024
    }

    if over16kb_allocated() {
        let mut marked = FxHashSet::default();
        trace(&vm_state, &mut marked);
        free(&marked);
    }
}

fn trace(vm_state: &VMState, marked: &mut FxHashSet<GcPtr>) {
    for val in &vm_state.stack {
        val.trace(marked);
    }
    for scope in &vm_state.scope {
        not_marked_then(*scope, marked, |scope, marked| unsafe {
            (*scope).trace(marked)
        });
    }
}

fn free(marked: &FxHashSet<GcPtr>) {
    GC_MEM.with(|mem| {
        mem.borrow_mut().retain(|p| {
            let is_marked = marked.contains(p);
            if !is_marked {
                unsafe {
                    (*p.0).free();
                    let released_size = mem::size_of_val(&*Box::from_raw(p.0));
                    ALLOCATED_MEM_SIZE_BYTE.fetch_sub(released_size, atomic::Ordering::SeqCst);
                }
            }
            is_marked
        });
    });
}

fn not_marked_then<F>(p: *mut Gc, marked: &mut FxHashSet<GcPtr>, mut f: F)
where
    F: FnMut(*mut Gc, &mut FxHashSet<GcPtr>),
{
    if marked.insert(GcPtr(p)) {
        f(p, marked);
    }
}
