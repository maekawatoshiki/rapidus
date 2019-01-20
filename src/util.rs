use std::mem;

pub fn get_raw_bytes_with_size<T: ?Sized>(p: &T, size: usize) -> Vec<u8> {
    let mut buf = Vec::with_capacity(size);
    let view = p as *const _ as *const u8;
    for i in 0..size {
        buf.push(unsafe { *view.offset(i as isize) });
    }
    buf
}

pub fn get_raw_bytes<T>(p: &T) -> Vec<u8> {
    get_raw_bytes_with_size(p, mem::size_of::<T>())
}

pub fn print_bytes(bytes: Vec<u8>) {
    for (i, x) in bytes.iter().enumerate() {
        if i % 16 == 0 {
            if i != 0 {
                println!();
            }
            print!("{:04x}| ", i);
        }
        print!("{:02x} ", x);
    }
    println!();
}

#[macro_export]
macro_rules! print_raw_bytes {
    ($e: expr) => {
        {
            use util::*;
            let p = & $e;
            println!("----- {:p}: {}", p, stringify!($e));
            print_bytes(get_raw_bytes(p));
        }
    };
    ($e: expr; $n: expr) => {
        {
            use util::*;
            let p = & $e;
            println!("----- {:p}: {}", p, stringify!($e));
            print_bytes(get_raw_bytes_with_size(p, $n));
        }
    };
}
