use std::cell::UnsafeCell;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

const NUM_THREADS: usize = 4;
const NUM_LOOP: usize = 100000;

struct SpinLock<T> {
    lock: AtomicBool,
    data: UnsafeCell<T>
}