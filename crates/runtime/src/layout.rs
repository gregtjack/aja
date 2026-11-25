//! Where compiled code finds things in memory: the parts of a [`Value`] and
//! the fields of the runtime it reads. The tests check each against the
//! Rust types they describe.

use crate::value::Value;

/// The size of a register.
pub const VALUE_SIZE: i32 = 32;
const _: () = assert!(std::mem::size_of::<Value>() == VALUE_SIZE as usize);

/// A value's tag, in its first byte.
pub const TAG_VOID: i64 = 0;
pub const TAG_INT: i64 = 1;
pub const TAG_FLOAT: i64 = 2;
pub const TAG_BOOL: i64 = 3;

/// Where a `Bool`'s value is: one byte, 0 or 1.
pub const BOOL_OFFSET: i32 = 1;
/// Where an `Int`'s or a `Float`'s value is: eight bytes.
pub const NUM_OFFSET: i32 = 8;

/// A slot that holds an Aja function being called is a `Fn`, which leaves
/// the slot's other three words free. The call stack is kept in them, so
/// the runtime can walk it without a list of frames:
///
/// - the call that put the callee there (a [`site_note`]),
pub const CALL_NOTE_OFFSET: i32 = 8;
/// - the caller's register window (an address; 0 for a task's first call),
pub const CALLER_OFFSET: i32 = 16;
/// - and the tail call that has replaced the callee since, if any (a
///   [`site_note`], or 0).
pub const TAIL_NOTE_OFFSET: i32 = 24;

/// A note of instruction `ip` of function number `function`; never 0.
pub fn site_note(function: u32, ip: u32) -> i64 {
    ((function as i64 + 1) << 32) | ip as i64
}

/// The function number and instruction a note is of; `None` for 0.
pub fn site_of(note: i64) -> Option<(u32, u32)> {
    let function = ((note as u64) >> 32) as u32;
    Some((function.checked_sub(1)?, note as u32))
}

/// The fields at the start of [`crate::rt::Rt`]: pointers, then counters.
pub const RT_REGS_LIMIT: i32 = 0;
pub const RT_STACK_LIMIT: i32 = 8;
pub const RT_GLOBALS: i32 = 16;
pub const RT_LITERALS: i32 = 24;
pub const RT_BUDGET: i32 = 32;
pub const RT_DEFERRED_CALLS: i32 = 40;

/// What a compiled function returns: it returned, an error is stopping
/// the program, or its task was cancelled and is leaving its frames.
pub const STATUS_OK: i64 = 0;
pub const STATUS_ERROR: i64 = 1;
pub const STATUS_STOPPED: i64 = 2;

/// What `aja_rt_call` returns is one of the statuses, for a native
/// function it ran, or else the address of an Aja function's code, which
/// is above this.
pub const CALL_MAX_STATUS: i64 = 2;

/// The bit of `aja_rt_call`'s argument count, and of `aja_rt_safepoint`'s
/// flag, that says the instruction is in deferred code.
pub const IN_DEFERRED: i64 = 1 << 8;
/// The bit of `aja_rt_call`'s argument count that says the call is a tail
/// call.
pub const IS_TAIL_CALL: i64 = 1 << 9;

#[cfg(test)]
mod tests {
    use super::*;

    /// The bytes of `v`, with the ones Rust leaves unset as zero.
    fn bytes(v: Value) -> [u8; VALUE_SIZE as usize] {
        let mut out = [0u8; VALUE_SIZE as usize];
        // SAFETY: `out` is as large as a `Value`, and nothing reads the
        // copy as a `Value`.
        unsafe { std::ptr::write_unaligned(out.as_mut_ptr().cast::<Value>(), v) };
        out
    }

    fn num(b: &[u8; 32]) -> [u8; 8] {
        b[NUM_OFFSET as usize..NUM_OFFSET as usize + 8]
            .try_into()
            .unwrap()
    }

    #[test]
    fn values_are_laid_out_as_compiled_code_expects() {
        assert_eq!(bytes(Value::Void)[0] as i64, TAG_VOID);

        let int = bytes(Value::Int(0x0102_0304_0506_0708));
        assert_eq!(int[0] as i64, TAG_INT);
        assert_eq!(i64::from_le_bytes(num(&int)), 0x0102_0304_0506_0708);

        let float = bytes(Value::Float(1.5));
        assert_eq!(float[0] as i64, TAG_FLOAT);
        assert_eq!(f64::from_le_bytes(num(&float)), 1.5);

        // A `Fn` is its tag and a 32-bit handle, so the notes fit after it.
        const { assert!(std::mem::size_of::<crate::heap::Gc>() <= 4) };
        const { assert!(CALL_NOTE_OFFSET >= 8 && TAIL_NOTE_OFFSET + 8 <= VALUE_SIZE) };
        assert_eq!(site_of(site_note(7, 9)), Some((7, 9)));
        assert_eq!(site_of(site_note(0, 0)), Some((0, 0)));
        assert_eq!(site_of(0), None);

        let (t, f) = (bytes(Value::Bool(true)), bytes(Value::Bool(false)));
        assert_eq!(t[0] as i64, TAG_BOOL);
        assert_eq!((t[BOOL_OFFSET as usize], f[BOOL_OFFSET as usize]), (1, 0));
    }
}
