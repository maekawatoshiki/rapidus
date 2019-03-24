// TODO: Support for Incremental GC

use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter, Result};
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{self, AtomicUsize};
use stopwatch::Stopwatch;
use vm::{
    callobj::CallObject,
    constant, frame,
    jsvalue::{
        function, object, prototype,
        value::{BoxedValue, Value2},
    },
    value::{ArrayValue, ObjectKind, Property, Value},
    vm::VM,
};

pub type RawPointer = *mut u8;
pub type MarkMap = FxHashMap<GcTargetKey, MarkState>;
pub type MarkSet = FxHashSet<GcTargetKey>;

#[derive(Debug, Clone, Eq, Copy)]
pub struct GcTargetKey(pub *mut GcTarget);

impl PartialEq for GcTargetKey {
    fn eq(&self, other: &GcTargetKey) -> bool {
        self.0 as *mut u8 == other.0 as *mut u8
    }
}

impl Hash for GcTargetKey {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u64(self.0 as *mut libc::c_void as u64);
        state.finish();
    }
}

#[derive(Debug)]
pub struct MemoryAllocator {
    allocated_memory: MarkMap,
    allocated_size: usize,
    pub roots: MarkSet,
    state: GCState,
    white: MarkState,
}

#[derive(Debug)]
pub enum GCState {
    Initial,
    Marking,
    ReadyToSweep,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MarkState {
    White,
    White2,
    Gray,
    Black,
}

impl MarkState {
    pub fn flip_white(self) -> Self {
        match self {
            MarkState::White => MarkState::White2,
            MarkState::White2 => MarkState::White,
            otherwise => otherwise,
        }
    }
}

impl MemoryAllocator {
    pub fn new() -> Self {
        MemoryAllocator {
            allocated_memory: MarkMap::default(),
            allocated_size: 0,
            roots: MarkSet::default(),
            state: GCState::Initial,
            white: MarkState::White,
        }
    }

    pub fn alloc<T: GcTarget + 'static>(&mut self, data: T) -> *mut T {
        let data_size = mem::size_of_val(&data);
        let ptr = Box::into_raw(Box::new(data));
        self.allocated_size += data_size;
        self.allocated_memory.insert(GcTargetKey(ptr), self.white);
        ptr
    }
}

impl MemoryAllocator {
    pub fn mark(
        &mut self,
        global: frame::LexicalEnvironmentRef,
        object_prototypes: &prototype::ObjectPrototypes,
        constant_table: &constant::ConstantTable,
        stack: &Vec<BoxedValue>,
        cur_frame: &frame::Frame,
        saved_frame: &Vec<frame::Frame>,
    ) {
        let mut markset = MarkSet::default();

        self.state = match self.state {
            GCState::Initial => {
                // println!("initial");
                // println!("before {:?}", self.allocated_memory);

                markset.insert(GcTargetKey(global));
                unsafe { &*global }.initial_trace(&mut markset);

                cur_frame.execution_context.initial_trace(&mut markset);
                cur_frame.this.initial_trace(&mut markset);

                object_prototypes.object.initial_trace(&mut markset);
                object_prototypes.function.initial_trace(&mut markset);
                object_prototypes.string.initial_trace(&mut markset);

                constant_table.initial_trace(&mut markset);

                for val_boxed in stack {
                    let val: Value2 = (*val_boxed).into();
                    val.initial_trace(&mut markset);
                }

                for frame in saved_frame {
                    frame.execution_context.initial_trace(&mut markset);
                    frame.this.initial_trace(&mut markset);
                }

                // println!("initial mark: {:?}", markset);

                self.white = self.white.flip_white();

                self.roots = markset;

                GCState::Marking
            }
            GCState::Marking => {
                // println!("start marking: {:?}", markset);

                for root in self.roots.clone() {
                    self.allocated_memory.insert(root, MarkState::Black);
                    unsafe { &*root.0 }.trace(self, &mut markset);
                }

                // println!("marking: {:?}", markset);

                self.roots = markset;

                if self.roots.len() == 0 {
                    // println!("all marked: {:?}", self.allocated_memory);
                    GCState::ReadyToSweep
                } else {
                    GCState::Marking
                }
            }
            GCState::ReadyToSweep => {
                // println!("before {:?}", self.allocated_memory.len());

                let white = self.white;
                self.allocated_memory.retain(|obj, mark| {
                    if mark == &MarkState::Black {
                        *mark = white;
                        return true;
                    }
                    if mark == &white {
                        return true;
                    }
                    unsafe { Box::from_raw(obj.0).free() };
                    false
                });

                // println!("now allocated objects: {:?}", self.allocated_memory.len());

                GCState::Initial
            }
        }
    }

    pub fn gray(&mut self, object: GcTargetKey) {
        unsafe { &*object.0 }.initial_trace(&mut self.roots);
    }

    pub fn gray2(&mut self, object: Value2) {
        object.initial_trace(&mut self.roots);
    }

    // pub fn write_barrier(&mut self, parent: *mut GcTarget, child: *mut GcTarget) {
    //     let need_write_barrier = self.allocated_memory.get(&parent).unwrap() == &MarkState::Black
    //         && self.allocated_memory.get(&child).unwrap() == &MarkState::White;
    //     if !need_write_barrier {
    //         return;
    //     }
    //     self.allocated_memory
    //         .insert(GcTargetKey(parent), MarkState::Gray);
    //     self.roots.insert(GcTargetKey(parent));
    // }
}

pub trait GcTarget {
    fn initial_trace(&self, &mut MarkSet);
    fn trace(&self, &mut MemoryAllocator, &mut MarkSet);
    fn free(&self) -> usize;
}

macro_rules! mark {
    ($markmap:expr, $val:expr) => {{
        if $val as *mut u8 != 0 as *mut u8 {
            $markmap.insert(GcTargetKey($val));
        }
    }};
}

macro_rules! mark_if_white {
    ($allocator:expr, $markset:expr, $val:expr) => {{
        if $val as *mut u8 != 0 as *mut u8 {
            let mark = $allocator.allocated_memory.get(&GcTargetKey($val)).unwrap();
            if mark == &$allocator.white.flip_white() {
                $markset.insert(GcTargetKey($val));
            }
        }
    }};
}

impl GcTarget for frame::ExecutionContext {
    fn initial_trace(&self, markset: &mut MarkSet) {
        mark!(markset, self.lexical_environment);
        mark!(markset, self.variable_environment);
        for env in &self.saved_lexical_environment {
            mark!(markset, *env);
        }
    }

    fn trace(&self, allocator: &mut MemoryAllocator, markset: &mut MarkSet) {
        mark_if_white!(allocator, markset, self.lexical_environment);
        mark_if_white!(allocator, markset, self.variable_environment);
        for env in &self.saved_lexical_environment {
            mark_if_white!(allocator, markset, *env);
        }
    }

    fn free(&self) -> usize {
        mem::size_of::<frame::ExecutionContext>()
    }
}

impl GcTarget for frame::LexicalEnvironment {
    fn initial_trace(&self, markset: &mut MarkSet) {
        fn trace_record(record: &frame::EnvironmentRecord, markset: &mut MarkSet) {
            match record {
                frame::EnvironmentRecord::Declarative(record) => {
                    for (_, val) in record {
                        val.initial_trace(markset);
                    }
                }
                frame::EnvironmentRecord::Object(obj) | frame::EnvironmentRecord::Global(obj) => {
                    obj.initial_trace(markset)
                }
            }
        }

        trace_record(&self.record, markset);

        if let Some(outer) = self.outer {
            mark!(markset, outer);
        }
    }

    fn trace(&self, allocator: &mut MemoryAllocator, markset: &mut MarkSet) {
        fn trace_record(
            allocator: &mut MemoryAllocator,
            record: &frame::EnvironmentRecord,
            markset: &mut MarkSet,
        ) {
            match record {
                frame::EnvironmentRecord::Declarative(record) => {
                    for (_, val) in record {
                        val.trace(allocator, markset);
                    }
                }
                frame::EnvironmentRecord::Object(obj) | frame::EnvironmentRecord::Global(obj) => {
                    obj.trace(allocator, markset)
                }
            }
        }

        trace_record(allocator, &self.record, markset);

        if let Some(outer) = self.outer {
            mark_if_white!(allocator, markset, outer);
        }
    }

    fn free(&self) -> usize {
        mem::size_of::<frame::LexicalEnvironment>()
    }
}

impl GcTarget for ::std::ffi::CString {
    fn initial_trace(&self, _markset: &mut MarkSet) {}
    fn trace(&self, _allocator: &mut MemoryAllocator, _markset: &mut MarkSet) {}
    fn free(&self) -> usize {
        // mem::drop(self);
        mem::size_of::<::std::ffi::CString>()
    }
}

impl GcTarget for Value2 {
    fn initial_trace(&self, markset: &mut MarkSet) {
        match self {
            Value2::Object(obj) => {
                mark!(markset, *obj);
            }
            Value2::String(s) => {
                mark!(markset, *s);
            }
            _ => {}
        }
    }

    fn trace(&self, allocator: &mut MemoryAllocator, markset: &mut MarkSet) {
        match self {
            Value2::Object(obj) => mark_if_white!(allocator, markset, *obj),
            Value2::String(s) => mark_if_white!(allocator, markset, *s),
            _ => {}
        }
    }

    fn free(&self) -> usize {
        mem::size_of::<Value2>()
    }
}

impl GcTarget for object::ObjectInfo {
    fn initial_trace(&self, markset: &mut MarkSet) {
        self.kind.initial_trace(markset);
        for (_, property) in &self.property {
            property.initial_trace(markset)
        }
    }

    fn trace(&self, allocator: &mut MemoryAllocator, markset: &mut MarkSet) {
        self.kind.trace(allocator, markset);
        for (_, property) in &self.property {
            property.trace(allocator, markset)
        }
    }

    fn free(&self) -> usize {
        mem::size_of::<object::ObjectInfo>()
    }
}

impl GcTarget for object::Property2 {
    fn initial_trace(&self, markset: &mut MarkSet) {
        match self {
            object::Property2::Data(object::DataProperty { val, .. }) => val.initial_trace(markset),
            object::Property2::Accessor(object::AccessorProperty { get, set, .. }) => {
                get.initial_trace(markset);
                set.initial_trace(markset);
            }
        }
    }

    fn trace(&self, allocator: &mut MemoryAllocator, markset: &mut MarkSet) {
        match self {
            object::Property2::Data(object::DataProperty { val, .. }) => {
                val.trace(allocator, markset)
            }
            object::Property2::Accessor(object::AccessorProperty { get, set, .. }) => {
                get.trace(allocator, markset);
                set.trace(allocator, markset);
            }
        }
    }

    fn free(&self) -> usize {
        mem::size_of::<object::Property2>()
    }
}

impl GcTarget for object::ObjectKind2 {
    fn initial_trace(&self, markset: &mut MarkSet) {
        fn trace_user_function_info(
            markset: &mut MarkSet,
            user_func_info: &function::UserFunctionInfo,
        ) {
            for func_decl in &user_func_info.func_decls {
                func_decl.initial_trace(markset);
            }

            if let Some(outer) = user_func_info.outer {
                mark!(markset, outer);
            }
        }

        match self {
            object::ObjectKind2::Function(func_info) => match func_info.kind {
                function::FunctionObjectKind::User(ref user_func_info) => {
                    trace_user_function_info(markset, user_func_info)
                }
                function::FunctionObjectKind::Builtin(_) => {}
            },
            object::ObjectKind2::Array(ary_info) => {
                for elem in &ary_info.elems {
                    elem.initial_trace(markset)
                }
            }
            object::ObjectKind2::Ordinary => {}
        }
    }

    fn trace(&self, allocator: &mut MemoryAllocator, markset: &mut MarkSet) {
        fn trace_user_function_info(
            allocator: &mut MemoryAllocator,
            markset: &mut MarkSet,
            user_func_info: &function::UserFunctionInfo,
        ) {
            for func_decl in &user_func_info.func_decls {
                func_decl.trace(allocator, markset);
            }

            if let Some(outer) = user_func_info.outer {
                mark_if_white!(allocator, markset, outer);
            }
        }

        match self {
            object::ObjectKind2::Function(func_info) => match func_info.kind {
                function::FunctionObjectKind::User(ref user_func_info) => {
                    trace_user_function_info(allocator, markset, user_func_info)
                }
                function::FunctionObjectKind::Builtin(_) => {}
            },
            object::ObjectKind2::Array(ary_info) => {
                for elem in &ary_info.elems {
                    elem.trace(allocator, markset)
                }
            }
            object::ObjectKind2::Ordinary => {}
        }
    }

    fn free(&self) -> usize {
        mem::size_of::<object::ObjectKind2>()
    }
}

impl GcTarget for constant::ConstantTable {
    fn initial_trace(&self, markset: &mut MarkSet) {
        for const_ in &self.table {
            match const_ {
                constant::Constant::Value(val) => val.initial_trace(markset),
                _ => {}
            }
        }
    }

    fn trace(&self, _allocator: &mut MemoryAllocator, _markset: &mut MarkSet) {
        panic!()
    }

    fn free(&self) -> usize {
        panic!()
    }
}

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

/// A struct which holds a pointer to X and a mark bit for GC.
/// Dereference of GcType<X> is automatically converted to &X or &mut X by deref coercion.
///
/// ### Usage
/// ```
/// # extern crate rustc_hash;
/// # use rapidus::gc;
/// # use rapidus::vm::value::{Value, CallObjectRef};
/// # use rapidus::vm::callobj::CallObject;
/// # use rustc_hash::FxHashMap;
/// let callobjectref =
///     gc::new(CallObject {
///         vals: gc::new(FxHashMap::default()),
///         this: Box::new(Value::Undefined),
///         parent: None,
///     }
/// );
///
/// assert_eq!(*callobjectref.this, Value::Undefined);
/// ```
pub struct GcType<X: Gc> {
    inner: usize,
    //gc_mark: bool,
    _phantom: std::marker::PhantomData<X>,
}

impl<X: Gc> Clone for GcType<X> {
    /// return the cloned pointer. (exactly the same value as original pointer.)
    /// both of original and cloned pointer point to the identical X.
    fn clone(&self) -> GcType<X> {
        GcType {
            inner: self.inner,
            //gc_mark: false,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<X: Gc> Deref for GcType<X> {
    type Target = X;

    fn deref(&self) -> &X {
        unsafe {
            let boxed_x = Box::from_raw(self.inner as *mut X);
            let ptr = Box::leak(boxed_x);
            ptr as &X
        }
    }
}

impl<X: Gc> DerefMut for GcType<X> {
    fn deref_mut(&mut self) -> &mut X {
        unsafe {
            let boxed_x = Box::from_raw(self.inner as *mut X);
            Box::leak(boxed_x)
        }
    }
}

impl<X: Gc> PartialEq for GcType<X> {
    fn eq(&self, other: &GcType<X>) -> bool {
        self.inner == other.inner
    }
}

impl<X: Gc> Debug for GcType<X> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, " 0x{:x?} ", self.inner)
    }
}

/// A trait for GC'able object.
pub trait Gc {
    fn free(&self) -> usize;
    fn trace(&mut self, &mut FxHashSet<GcPtr>);
}

// impl Gc for frame::EnvironmentRecord {
//     fn free(&self) -> usize {
//         mem::drop(self);
//         mem::size_of::<frame::EnvironmentRecord>()
//     }
//
//     fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
//         if !mark(self, marked) {
//             return;
//         }
//
//         match self {
//             frame::EnvironmentRecord::Declarative(record) => {
//                 for (_, val) in record {
//                     val.trace(marked);
//                 }
//             }
//         }
//     }
// }
//
// impl Gc for frame::LexicalEnvironment {
//     fn free(&self) -> usize {
//         mem::drop(self);
//         mem::size_of::<frame::LexicalEnvironment>()
//     }
//
//     fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
//         if !mark(self, marked) {
//             return;
//         }
//
//         self.record.trace(marked);
//
//         if let Some(outer) = self.outer {
//             outer.trace(marked);
//         }
//     }
// }
//
// // impl Gc for Value2 {
// //     fn free(&self) -> usize {
// //         mem::drop(self);
// //         mem::size_of::<Value2>()
// //     }
// //     fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
// //         if !mark(self, marked) {
// //             return;
// //         };
// //         match self {
// //             Value::Uninitialized
// //             | Value::Empty
// //             | Value::Null
// //             | Value::Undefined
// //             | Value::Bool(_)
// //             | Value::Number(_)
// //             | Value::String(_) => {}
// //             Value::Object(map, ObjectKind::Function(box (_, c))) => {
// //                 map.trace(marked);
// //                 c.trace(marked);
// //             }
// //             // Never trace _xxx
// //             Value::Object(_, ObjectKind::BuiltinFunction(box (_, c))) => (*c).trace(marked),
// //
// //             Value::Object(map, ObjectKind::Array(a)) => {
// //                 map.trace(marked);
// //                 a.trace(marked);
// //             }
// //             Value::Object(map, ObjectKind::Ordinary) | Value::Object(map, ObjectKind::Date(_)) => {
// //                 map.trace(marked);
// //             }
// //             Value::Object(map, ObjectKind::Arguments(_)) => {
// //                 map.trace(marked);
// //             }
// //         }
// //     }
// // }

impl Gc for Value {
    fn free(&self) -> usize {
        mem::drop(self);
        mem::size_of::<Value>()
    }

    fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
        if !mark(self, marked) {
            return;
        }

        match self {
            Value::Uninitialized
            | Value::Empty
            | Value::Null
            | Value::Undefined
            | Value::Bool(_)
            | Value::Number(_)
            | Value::String(_) => {}
            Value::Object(map, ObjectKind::Function(box (_, c))) => {
                map.trace(marked);
                c.trace(marked);
            }
            // Never trace _xxx
            Value::Object(_, ObjectKind::BuiltinFunction(box (_, c))) => (*c).trace(marked),

            Value::Object(map, ObjectKind::Array(a)) => {
                map.trace(marked);
                a.trace(marked);
            }
            Value::Object(map, ObjectKind::Ordinary) | Value::Object(map, ObjectKind::Date(_)) => {
                map.trace(marked);
            }
            Value::Object(map, ObjectKind::Arguments(_)) => {
                map.trace(marked);
            }
        }
    }
}

impl Gc for FxHashMap<String, Property> {
    fn free(&self) -> usize {
        mem::drop(self);
        mem::size_of::<FxHashMap<String, Property>>()
    }

    fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
        if !mark(self, marked) {
            return;
        }

        let map = self;
        for (_, prop) in map {
            prop.val.trace(marked);
        }
    }
}

impl Gc for CallObject {
    fn free(&self) -> usize {
        mem::drop(self);
        mem::size_of::<CallObject>()
    }

    fn trace(&mut self, marked: &mut FxHashSet<GcPtr>) {
        if !mark(self, marked) {
            return;
        };
        self.vals.trace(marked);
        /*
        for (_, val) in &mut self.arguments {
            val.trace(marked);
        }

        if let Some(ref mut parent) = *self.parent {
            parent.trace(marked);
        }
        */
        self.this.trace(marked);
    }
}

impl Gc for ArrayValue {
    fn free(&self) -> usize {
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

pub fn new<X: Gc + 'static>(data: X) -> GcType<X> {
    let data_size = mem::size_of_val(&data);
    // get a raw pointer which points to the data copied on the heap.
    let ptr = Box::into_raw(Box::new(data));
    let _prev_size = ALLOCATED_MEM_SIZE_BYTE.fetch_add(data_size, atomic::Ordering::SeqCst);
    GC_MEM.with(|m| m.borrow_mut().insert(GcPtr(ptr)));
    GcType {
        inner: ptr as usize,
        //gc_mark: false,
        _phantom: std::marker::PhantomData,
    }
}

pub fn mark_and_sweep(vm: &mut VM) {
    fn over16kb_allocated() -> bool {
        ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst) > 16 * 1024
    }

    if vm.gc_on && over16kb_allocated() {
        let _sw = Stopwatch::start_new();
        let mut marked = FxHashSet::default();
        let pre_alloc_size = ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst);
        let pre_gc_size = GC_MEM.with(|mem| mem.borrow_mut().len());
        trace(vm, &mut marked);
        free(&marked);
        if vm.is_debug {
            println!(
                "GC executed: pause duration {} ms. {} -> {} bytes. {} => {} objects",
                _sw.elapsed_ms(),
                pre_alloc_size,
                ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst),
                pre_gc_size,
                GC_MEM.with(|mem| mem.borrow_mut().len()),
            );
        }
    }
}

fn trace(vm: &mut VM, marked: &mut FxHashSet<GcPtr>) {
    for val in &mut vm.codegen.bytecode_gen.const_table.value {
        val.trace(marked);
    }
    //let after_const = marked.len();
    for val in &mut vm.state.stack {
        val.trace(marked);
    }
    //let after_stack = marked.len();
    for scope in &mut vm.state.scope {
        scope.trace(marked);
    }
    //let after_scope = marked.len();
    /*
    println!(
        "marked after const: {} stack:{} scope:{}",
        after_const, after_stack, after_scope
    );
    */
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
                let released_size = (*p.0).free();
                ALLOCATED_MEM_SIZE_BYTE.fetch_sub(released_size, atomic::Ordering::SeqCst);
            }
            false
        });
    });
    /*
    println!(
        "\nallocated_size: {} all:{}",
        ALLOCATED_MEM_SIZE_BYTE.load(atomic::Ordering::SeqCst),
        GC_MEM.with(|mem| mem.borrow_mut().len()),
    );
    */
}

fn mark(p: *mut Gc, marked: &mut FxHashSet<GcPtr>) -> bool {
    marked.insert(GcPtr(p))
}
