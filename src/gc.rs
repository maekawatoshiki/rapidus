use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::mem;
use std::sync::atomic::{self, AtomicUsize};
use vm::{
    callobj::CallObject,
    value::{ArrayValue, ObjectKind, Property, Value},
    vm::VM,
};

static ALLOCATED_MEM_SIZE_BYTE: AtomicUsize = AtomicUsize::new(0);

thread_local!(pub static GC_MEM: RefCell<FxHashSet<GcPtr>> = {
    RefCell::new(FxHashSet::default())
});

pub type GcType<T> = *mut T;

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
    fn free(&self) -> usize;
    fn trace(&mut self, &mut FxHashSet<GcPtr>);
}

impl Gc for Value {
    fn free(&self) -> usize {
        print!(":V");
        mem::drop(self);
        mem::size_of::<Value>()
    }

    fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
        if !mark(self, marked) {
            return;
        };
        match self {
            Value::Empty
            | Value::Null
            | Value::Undefined
            | Value::Bool(_)
            | Value::Number(_)
            | Value::String(_) => {}
            Value::Object(map, ObjectKind::Function(box (_, c))) => unsafe {
                (**map).trace(marked);
                (*c).trace(marked);
            },
            // Never trace _xxx
            Value::Object(_, ObjectKind::BuiltinFunction(box (_, c))) => (*c).trace(marked),

            Value::Object(map, ObjectKind::Array(a)) => unsafe {
                (**map).trace(marked);
                (**a).trace(marked);
            },
            Value::Object(map, ObjectKind::Ordinary) | Value::Object(map, ObjectKind::Date(_)) => unsafe {
                (**map).trace(marked);
            },
            Value::Object(map, ObjectKind::Arguments(c)) => unsafe {
                (**map).trace(marked);
                (**c).trace(marked);
            },
        }
    }
}

impl Gc for FxHashMap<String, Property> {
    fn free(&self) -> usize {
        print!(":M");
        mem::drop(self);
        mem::size_of::<FxHashMap<String, Property>>()
    }

    fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
        if !mark(self, marked) {
            return;
        };
        let map = self;
        for (_, prop) in map {
            prop.val.trace(marked);
        }
    }
}

impl Gc for CallObject {
    fn free(&self) -> usize {
        print!(":C");
        mem::drop(self);
        mem::size_of::<CallObject>()
    }

    fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
        if !mark(self, marked) {
            return;
        };
        unsafe {
            (*self.vals).trace(marked);

            for (_, val) in &mut self.arguments {
                (*val).trace(marked);
            }

            if let Some(parent) = self.parent {
                (*parent).trace(marked);
            }
            self.this.trace(marked);
        }
    }
}

impl Gc for ArrayValue {
    fn free(&self) -> usize {
        print!(":AV");
        mem::drop(self);
        mem::size_of::<ArrayValue>()
    }

    fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
        if !mark(self, marked) {
            return;
        };
        for prop in &mut self.elems {
            prop.val.trace(marked)
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

pub fn mark_and_sweep(vm: &mut VM) {
    fn over16kb_allocated() -> bool {
        //ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst) > 16 * 1024
        true
    }

    if over16kb_allocated() {
        let mut marked = FxHashSet::default();
        let pre_alloc_size = ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst);
        let pre_gc_size = GC_MEM.with(|mem| mem.borrow_mut().len());
        trace(vm, &mut marked);
        free(&marked);
        let post_gc_size = GC_MEM.with(|mem| mem.borrow_mut().len());
        if pre_gc_size != post_gc_size {
            println!(
                "\nallocated_size: {}->{} marked: {} all:{}->{}",
                pre_alloc_size,
                ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst),
                marked.len(),
                pre_gc_size,
                post_gc_size,
            );
        }
    }
}

fn trace(vm: &mut VM, marked: &mut FxHashSet<GcPtr>) {
    for val in &mut vm.const_table.value {
        val.trace(marked);
    }
    let after_const = marked.len();
    for val in &mut vm.state.stack {
        val.trace(marked);
    }
    let after_stack = marked.len();
    for scope in &mut vm.state.scope {
        unsafe {
            (**scope).trace(marked);
        }
    }
    let after_scope = marked.len();
    println!(
        "marked after const: {} stack:{} scope:{}",
        after_const, after_stack, after_scope
    );
}

fn free(marked: &FxHashSet<GcPtr>) {
    GC_MEM.with(|mem| {
        mem.borrow_mut().retain(|p| {
            let is_marked = marked.contains(p);
            if !is_marked {
                unsafe {
                    // SEGV occurs in this point.
                    //let b = Box::from_raw(p.0);
                    //let released_size = mem::size_of_val(&*b);
                    //Box::leak(b);
                    let released_size = (*p.0).free();
                    ALLOCATED_MEM_SIZE_BYTE.fetch_sub(released_size, atomic::Ordering::SeqCst);
                }
            }
            is_marked
        });
    });
}

pub fn free_all() {
    GC_MEM.with(|mem| {
        mem.borrow_mut().retain(|p| {
            unsafe {
                // SEGV occurs in this point.
                //let _ = Box::from_raw(p.0);
                let released_size = (*p.0).free();
                ALLOCATED_MEM_SIZE_BYTE.fetch_sub(released_size, atomic::Ordering::SeqCst);
            }
            false
        });
    });
    println!(
        "\nallocated_size: {} all:{}",
        ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst),
        GC_MEM.with(|mem| mem.borrow_mut().len()),
    );
}

/*
fn not_marked_then<F>(p: *mut Gc, marked: &mut FxHashSet<GcPtr>, mut f: F)
where
    F: FnMut(*mut Gc, &mut FxHashSet<GcPtr>),
{
    if marked.insert(GcPtr(p)) {
        f(p, marked);
    }
}
*/
fn mark(p: *mut Gc, marked: &mut FxHashSet<GcPtr>) -> bool {
    marked.insert(GcPtr(p))
}
