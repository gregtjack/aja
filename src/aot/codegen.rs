//! Compiles bytecode to native code with Cranelift, as an object file.
//!
//! Each bytecode function becomes one native function, which takes the
//! runtime and the address of its register window and returns 0, or 1 if
//! an error is stopping the program (see [`super::rt`]). Instructions are
//! translated one by one. Those on ints, floats, and bools, moves, and
//! jumps become machine code that reads and writes the registers in
//! memory; the rest call the runtime.
//!
//! A bytecode call is a native call, and a bytecode tail call a native
//! tail call, so tail recursion runs in constant space here too. A call
//! of a top-level function by name calls its code directly; any other
//! asks the runtime what the callee is.
//!
//! The object file also holds the program's metadata ([`Meta`]), a table
//! of the functions' addresses, and a `main` that hands both to the
//! runtime.

use std::{collections::HashMap, rc::Rc};

use cranelift_codegen::{
    ir::{
        condcodes::{FloatCC, IntCC},
        types::{F64, I32, I64, I8},
        AbiParam, Block, FuncRef, InstBuilder, MemFlagsData, SigRef, Signature, Value,
    },
    isa::{CallConv, OwnedTargetIsa},
    settings::{self, Configurable},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use super::{
    layout::*,
    meta::{FnMeta, LayoutMeta, Meta},
    rt,
};
use crate::{
    ast::Source,
    diagnostic::Files,
    vm::{
        bytecode::{Layout, Op, Proto, Reg},
        Compiled,
    },
};

/// Compile `program` to an object file for the machine `aja` is running
/// on. `files` are its sources, which go in the object file for error
/// messages to show.
pub fn compile(program: &Compiled, files: &Files) -> Result<Vec<u8>, String> {
    let mut functions = Vec::new();
    for (slot, proto) in &program.functions {
        flatten(proto, Some(*slot), &mut functions);
    }

    let mut module = ObjectModule::new(
        ObjectBuilder::new(host()?, "aja", cranelift_module::default_libcall_names())
            .map_err(|e| e.to_string())?,
    );
    let ids = functions
        .iter()
        .enumerate()
        .map(|(i, _)| {
            let sig = function_signature(&module);
            module
                .declare_function(&format!("aja_fn_{i}"), Linkage::Local, &sig)
                .map_err(|e| e.to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;

    let mut cx = ModuleCx {
        runtime: Runtime::declare(&mut module)?,
        by_slot: functions
            .iter()
            .enumerate()
            .filter_map(|(i, f)| Some((f.slot?, i as u32)))
            .collect(),
        by_proto: functions
            .iter()
            .enumerate()
            .map(|(i, f)| (Rc::as_ptr(&f.proto), i as u32))
            .collect(),
        ids,
        layouts: Vec::new(),
        layout_numbers: HashMap::new(),
        module,
    };

    let mut meta = Meta {
        files: files
            .iter()
            .map(|(path, text)| (path.to_string(), text.to_string()))
            .collect(),
        literals: program
            .literals
            .iter()
            .map(|(n, s)| (*n, s.to_string()))
            .collect(),
        natives: program
            .natives
            .iter()
            .map(|(slot, name)| (*slot, name.to_string()))
            .collect(),
        main: program.main,
        main_result: program.main_result,
        library: program
            .library
            .iter()
            .filter_map(|s| match s {
                Source::File(id) => Some(*id as u32),
                Source::Prelude => None,
            })
            .collect(),
        ..Meta::default()
    };
    let mut globals = program.natives.iter().map(|(slot, _)| slot + 1).max();
    globals = globals.max(functions.iter().filter_map(|f| f.slot).map(|s| s + 1).max());

    let mut context = cx.module.make_context();
    let mut builder_context = FunctionBuilderContext::new();
    for (index, f) in functions.iter().enumerate() {
        let sites = FnCx::translate(&mut cx, &mut context, &mut builder_context, index as u32, f)?;
        cx.module
            .define_function(cx.ids[index], &mut context)
            .map_err(|e| format!("{e:?}"))?;
        cx.module.clear_context(&mut context);

        let p = &f.proto;
        let mut call_names: Vec<_> = p
            .call_names
            .iter()
            .map(|(ip, name)| (*ip as u32, name.to_string()))
            .collect();
        call_names.sort();
        meta.functions.push(FnMeta {
            name: p.name.as_ref().map(|n| n.to_string()),
            signature: p.signature.clone(),
            arity: p.arity,
            num_regs: p.num_regs,
            slot: f.slot,
            captures: p.captures.clone(),
            sites: sites
                .into_iter()
                .map(|ip| (ip, p.spans[ip as usize]))
                .collect(),
            call_names,
        });
    }
    meta.layouts = cx
        .layouts
        .iter()
        .map(|l| LayoutMeta {
            name: l.name.clone(),
            tag: l.tag,
            fields: l
                .fields
                .as_ref()
                .map(|fields| fields.iter().map(|f| f.to_string()).collect()),
        })
        .collect();
    meta.globals = globals.unwrap_or(0);

    cx.entry_points(&meta.encode())?;
    cx.module
        .finish()
        .emit()
        .map_err(|e| format!("could not write the object file: {e}"))
}

/// The machine `aja` is running on, set up to generate optimized,
/// position-independent code.
fn host() -> Result<OwnedTargetIsa, String> {
    let mut flags = settings::builder();
    for (name, value) in [
        ("opt_level", "speed"),
        ("is_pic", "true"),
        // Tail calls need them.
        ("preserve_frame_pointers", "true"),
    ] {
        flags.set(name, value).map_err(|e| e.to_string())?;
    }
    cranelift_native::builder()
        .map_err(|e| format!("cannot generate code for this machine: {e}"))?
        .finish(settings::Flags::new(flags))
        .map_err(|e| e.to_string())
}

/// A function to compile: a top-level one, with its global slot, or a
/// closure.
struct Function {
    proto: Rc<Proto>,
    slot: Option<u32>,
}

/// List `proto` and the closures inside it, outermost first.
fn flatten(proto: &Rc<Proto>, slot: Option<u32>, out: &mut Vec<Function>) {
    out.push(Function {
        proto: proto.clone(),
        slot,
    });
    for inner in &proto.protos {
        flatten(inner, None, out);
    }
}

/// The signature of every compiled Aja function: the runtime and the
/// function's register window, to a status. The `tail` calling convention
/// is what lets one tail-call another.
fn function_signature(module: &ObjectModule) -> Signature {
    let ptr = module.target_config().pointer_type();
    let mut sig = Signature::new(CallConv::Tail);
    sig.params = vec![AbiParam::new(ptr), AbiParam::new(ptr)];
    sig.returns = vec![AbiParam::new(I64)];
    sig
}

/// The runtime's functions that compiled code calls (see [`super::rt`]).
struct Runtime {
    error: FuncId,
    at: FuncId,
    trace: FuncId,
    call: FuncId,
    new_cell: FuncId,
    get_cell: FuncId,
    set_cell: FuncId,
    get_upvalue: FuncId,
    get_upvalue_cell: FuncId,
    set_upvalue_cell: FuncId,
    closure: FuncId,
    new_data: FuncId,
    new_list: FuncId,
    index: FuncId,
    get_field: FuncId,
    test_tag: FuncId,
    add_str: FuncId,
    equal: FuncId,
    less: FuncId,
    less_or_equal: FuncId,
    rem_float: FuncId,
    safepoint: FuncId,
    main: FuncId,
}

impl Runtime {
    fn declare(module: &mut ObjectModule) -> Result<Runtime, String> {
        use cranelift_codegen::ir::Type;
        let p = module.target_config().pointer_type();
        let mut import = |name: &str, params: &[Type], returns: &[Type]| {
            let mut sig = module.make_signature();
            sig.params = params.iter().map(|&t| AbiParam::new(t)).collect();
            sig.returns = returns.iter().map(|&t| AbiParam::new(t)).collect();
            module
                .declare_function(name, Linkage::Import, &sig)
                .map_err(|e| e.to_string())
        };
        Ok(Runtime {
            error: import("aja_rt_error", &[p, I64, I32, I32], &[])?,
            at: import("aja_rt_at", &[p, I32, I32], &[])?,
            trace: import("aja_rt_trace", &[p, I32, I32, p], &[])?,
            call: import("aja_rt_call", &[p, p, I64, p, I64, p], &[p])?,
            new_cell: import("aja_rt_new_cell", &[p, p, p, p], &[])?,
            get_cell: import("aja_rt_get_cell", &[p, p, p], &[])?,
            set_cell: import("aja_rt_set_cell", &[p, p, p], &[])?,
            get_upvalue: import("aja_rt_get_upvalue", &[p, p, p, I64], &[])?,
            get_upvalue_cell: import("aja_rt_get_upvalue_cell", &[p, p, p, I64], &[])?,
            set_upvalue_cell: import("aja_rt_set_upvalue_cell", &[p, p, I64, p], &[])?,
            closure: import("aja_rt_closure", &[p, p, p, I32, p], &[])?,
            new_data: import("aja_rt_new_data", &[p, p, p, I64, I32, p], &[])?,
            new_list: import("aja_rt_new_list", &[p, p, p, I64, p], &[])?,
            index: import("aja_rt_index", &[p, p, p, p], &[I64])?,
            get_field: import("aja_rt_get_field", &[p, p, p, I64], &[])?,
            test_tag: import("aja_rt_test_tag", &[p, p, I32], &[I64])?,
            add_str: import("aja_rt_add_str", &[p, p, p, p, p], &[I64])?,
            equal: import("aja_rt_equal", &[p, p, p], &[I64])?,
            less: import("aja_rt_less", &[p, p, p], &[I64])?,
            less_or_equal: import("aja_rt_less_or_equal", &[p, p, p], &[I64])?,
            rem_float: import("aja_rt_rem_float", &[F64, F64], &[F64])?,
            safepoint: import("aja_rt_safepoint", &[p, p, I64], &[I64])?,
            main: import("aja_rt_main", &[I32, p, p, I64, p, p], &[I32])?,
        })
    }
}

/// What compiling a program's functions shares.
struct ModuleCx {
    module: ObjectModule,
    runtime: Runtime,
    /// Each function's id, by its number.
    ids: Vec<FuncId>,
    /// The number of the top-level function in each global slot.
    by_slot: HashMap<u32, u32>,
    /// Each function's number.
    by_proto: HashMap<*const Proto, u32>,
    /// The layouts `NewData` instructions use, by number.
    layouts: Vec<Rc<Layout>>,
    layout_numbers: HashMap<*const Layout, u32>,
}

impl ModuleCx {
    fn layout(&mut self, layout: &Rc<Layout>) -> u32 {
        *self
            .layout_numbers
            .entry(Rc::as_ptr(layout))
            .or_insert_with(|| {
                self.layouts.push(layout.clone());
                self.layouts.len() as u32 - 1
            })
    }

    /// Define the metadata, the table of the functions' addresses, the
    /// function the runtime enters compiled code through, and `main`.
    fn entry_points(&mut self, meta: &[u8]) -> Result<(), String> {
        let err = |e: cranelift_module::ModuleError| format!("{e:?}");
        let ptr = self.module.target_config().pointer_type();

        let meta_id = self
            .module
            .declare_data("aja_meta", Linkage::Local, false, false)
            .map_err(err)?;
        let mut data = DataDescription::new();
        data.define(meta.into());
        self.module.define_data(meta_id, &data).map_err(err)?;

        // Written to when the program is loaded, to point at the code.
        let table_id = self
            .module
            .declare_data("aja_code", Linkage::Local, true, false)
            .map_err(err)?;
        let mut data = DataDescription::new();
        let size = ptr.bytes() as usize;
        data.define(vec![0; self.ids.len() * size].into());
        for (i, &id) in self.ids.iter().enumerate() {
            let f = self.module.declare_func_in_data(id, &mut data);
            data.write_function_addr((i * size) as u32, f);
        }
        self.module.define_data(table_id, &data).map_err(err)?;

        let mut context = self.module.make_context();
        let mut builder_context = FunctionBuilderContext::new();

        // enter(rt, base, code) -> status: call `code` the way compiled
        // functions call each other.
        let mut sig = self.module.make_signature();
        sig.params = vec![AbiParam::new(ptr); 3];
        sig.returns = vec![AbiParam::new(I64)];
        let enter_id = self
            .module
            .declare_function("aja_enter", Linkage::Local, &sig)
            .map_err(err)?;
        context.func.signature = sig;
        {
            let callee_sig = function_signature(&self.module);
            let mut b = FunctionBuilder::new(&mut context.func, &mut builder_context);
            let block = b.create_block();
            b.append_block_params_for_function_params(block);
            b.switch_to_block(block);
            let (rt, base, code) = {
                let p = b.block_params(block);
                (p[0], p[1], p[2])
            };
            let sig = b.import_signature(callee_sig);
            let call = b.ins().call_indirect(sig, code, &[rt, base]);
            let status = b.inst_results(call)[0];
            b.ins().return_(&[status]);
            b.seal_all_blocks();
            b.finalize(self.module.target_config());
        }
        self.module
            .define_function(enter_id, &mut context)
            .map_err(err)?;
        self.module.clear_context(&mut context);

        // main(argc, argv) -> status
        let mut sig = self.module.make_signature();
        sig.params = vec![AbiParam::new(I32), AbiParam::new(ptr)];
        sig.returns = vec![AbiParam::new(I32)];
        let main_id = self
            .module
            .declare_function("main", Linkage::Export, &sig)
            .map_err(err)?;
        context.func.signature = sig;
        {
            let meta_value = self.module.declare_data_in_func(meta_id, &mut context.func);
            let table = self
                .module
                .declare_data_in_func(table_id, &mut context.func);
            let enter = self
                .module
                .declare_func_in_func(enter_id, &mut context.func);
            let run = self
                .module
                .declare_func_in_func(self.runtime.main, &mut context.func);
            let mut b = FunctionBuilder::new(&mut context.func, &mut builder_context);
            let block = b.create_block();
            b.append_block_params_for_function_params(block);
            b.switch_to_block(block);
            let (argc, argv) = {
                let p = b.block_params(block);
                (p[0], p[1])
            };
            let meta_addr = b.ins().symbol_value(ptr, meta_value);
            let meta_len = b.ins().iconst(I64, meta.len() as i64);
            let table = b.ins().symbol_value(ptr, table);
            let enter = b.ins().func_addr(ptr, enter);
            let call = b
                .ins()
                .call(run, &[argc, argv, meta_addr, meta_len, table, enter]);
            let status = b.inst_results(call)[0];
            b.ins().return_(&[status]);
            b.seal_all_blocks();
            b.finalize(self.module.target_config());
        }
        self.module
            .define_function(main_id, &mut context)
            .map_err(err)?;
        Ok(())
    }
}

/// How a function leaves when an error is stopping the program: what it
/// tells the runtime first.
#[derive(Clone, Copy)]
enum Exit {
    /// The instruction failed for this reason (an `rt::ERR_*`).
    Error(i64),
    /// A runtime function the instruction called reported an error.
    At,
    /// A function the instruction called, whose slot is this register,
    /// returned with an error.
    Trace(Reg),
}

/// The runtime's functions, as one function refers to them.
#[derive(Clone, Copy)]
struct RuntimeRefs {
    error: FuncRef,
    at: FuncRef,
    trace: FuncRef,
    call: FuncRef,
    new_cell: FuncRef,
    get_cell: FuncRef,
    set_cell: FuncRef,
    get_upvalue: FuncRef,
    get_upvalue_cell: FuncRef,
    set_upvalue_cell: FuncRef,
    closure: FuncRef,
    new_data: FuncRef,
    new_list: FuncRef,
    index: FuncRef,
    get_field: FuncRef,
    test_tag: FuncRef,
    add_str: FuncRef,
    equal: FuncRef,
    less: FuncRef,
    less_or_equal: FuncRef,
    rem_float: FuncRef,
    safepoint: FuncRef,
}

/// Compiles one function.
struct FnCx<'a, 'b> {
    cx: &'a mut ModuleCx,
    b: FunctionBuilder<'b>,
    proto: &'a Proto,
    /// This function's number.
    index: u32,
    rt: Value,
    base: Value,
    runtime: RuntimeRefs,
    /// The signature of compiled functions, for indirect calls.
    sig: SigRef,
    /// The other compiled functions this one calls directly.
    callees: HashMap<u32, FuncRef>,
    /// The block each jump target starts.
    blocks: HashMap<u32, Block>,
    /// Error exits to generate once the function's own code is done: the
    /// block, what to report, and for which instruction.
    exits: Vec<(Block, Exit, u32)>,
    /// The instructions that have an exit, for the metadata.
    sites: Vec<u32>,
    /// The registers known to hold a top-level function, with its number:
    /// those loaded from its global since the last jump target.
    known: HashMap<Reg, u32>,
    /// The block that returns because the task was cancelled, if any
    /// instruction needs it.
    stopped: Option<Block>,
}

/// Memory compiled code reads and writes is always there and aligned.
fn mem() -> MemFlagsData {
    MemFlagsData::trusted()
}

impl<'a, 'b> FnCx<'a, 'b> {
    /// Fill `context` with the native code of `f`, and return the
    /// instructions whose place in the source the runtime may ask for.
    fn translate(
        cx: &'a mut ModuleCx,
        context: &'b mut Context,
        builder_context: &'b mut FunctionBuilderContext,
        index: u32,
        f: &'a Function,
    ) -> Result<Vec<u32>, String> {
        context.func.signature = function_signature(&cx.module);
        let func = &mut context.func;
        let r = &cx.runtime;
        let m = &mut cx.module;
        let runtime = RuntimeRefs {
            error: m.declare_func_in_func(r.error, func),
            at: m.declare_func_in_func(r.at, func),
            trace: m.declare_func_in_func(r.trace, func),
            call: m.declare_func_in_func(r.call, func),
            new_cell: m.declare_func_in_func(r.new_cell, func),
            get_cell: m.declare_func_in_func(r.get_cell, func),
            set_cell: m.declare_func_in_func(r.set_cell, func),
            get_upvalue: m.declare_func_in_func(r.get_upvalue, func),
            get_upvalue_cell: m.declare_func_in_func(r.get_upvalue_cell, func),
            set_upvalue_cell: m.declare_func_in_func(r.set_upvalue_cell, func),
            closure: m.declare_func_in_func(r.closure, func),
            new_data: m.declare_func_in_func(r.new_data, func),
            new_list: m.declare_func_in_func(r.new_list, func),
            index: m.declare_func_in_func(r.index, func),
            get_field: m.declare_func_in_func(r.get_field, func),
            test_tag: m.declare_func_in_func(r.test_tag, func),
            add_str: m.declare_func_in_func(r.add_str, func),
            equal: m.declare_func_in_func(r.equal, func),
            less: m.declare_func_in_func(r.less, func),
            less_or_equal: m.declare_func_in_func(r.less_or_equal, func),
            rem_float: m.declare_func_in_func(r.rem_float, func),
            safepoint: m.declare_func_in_func(r.safepoint, func),
        };
        let callee_sig = function_signature(&cx.module);

        let mut b = FunctionBuilder::new(&mut context.func, builder_context);
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        let (rt, base) = {
            let p = b.block_params(entry);
            (p[0], p[1])
        };
        let sig = b.import_signature(callee_sig);

        let mut fc = FnCx {
            cx,
            b,
            proto: &f.proto,
            index,
            rt,
            base,
            runtime,
            sig,
            callees: HashMap::new(),
            blocks: HashMap::new(),
            exits: Vec::new(),
            sites: Vec::new(),
            known: HashMap::new(),
            stopped: None,
        };
        fc.body()?;
        fc.b.seal_all_blocks();
        let config = fc.cx.module.target_config();
        fc.b.finalize(config);
        fc.sites.sort();
        fc.sites.dedup();
        Ok(fc.sites)
    }

    fn body(&mut self) -> Result<(), String> {
        let proto: &'a Proto = self.proto;
        let code = &proto.code;
        for op in code {
            if let Op::Jump { target }
            | Op::JumpIfFalse { target, .. }
            | Op::JumpIfTrue { target, .. } = *op
            {
                let block = self.b.create_block();
                self.blocks.entry(target).or_insert(block);
            }
        }
        // Where a cancelled task runs the function's pending deferred code.
        for &(_, _, cleanup) in &proto.cleanups {
            let block = self.b.create_block();
            self.blocks.entry(cleanup).or_insert(block);
        }

        // The window must fit in the registers, and the machine's stack
        // must have room for this call. The call that led here is what
        // the error points at.
        let ptr = self.ptr();
        let top = self.top();
        let limit = self.b.ins().load(ptr, mem(), self.rt, RT_REGS_LIMIT);
        let no_regs = self.b.ins().icmp(IntCC::UnsignedGreaterThan, top, limit);
        let sp = self.b.ins().get_stack_pointer(ptr);
        let limit = self.b.ins().load(ptr, mem(), self.rt, RT_STACK_LIMIT);
        let no_stack = self.b.ins().icmp(IntCC::UnsignedLessThan, sp, limit);
        let overflow = self.b.ins().bor(no_regs, no_stack);
        self.exit_if(overflow, Exit::Error(rt::ERR_STACK), u32::MAX);
        // Every call is a safepoint, so recursion gives other tasks turns.
        self.safepoint(None);

        // Whether the instruction being translated can be reached: not
        // after a jump or a return, until the next jump target.
        let mut reachable = true;
        for (ip, op) in code.iter().enumerate() {
            let ip = ip as u32;
            if let Some(&block) = self.blocks.get(&ip) {
                if reachable {
                    self.b.ins().jump(block, &[]);
                }
                self.b.switch_to_block(block);
                self.known.clear();
                reachable = true;
            }
            if reachable {
                reachable = self.op(ip, *op)?;
            }
        }
        if reachable {
            return Err("internal error: a function's code runs off its end".to_string());
        }

        for (block, exit, ip) in std::mem::take(&mut self.exits) {
            self.b.switch_to_block(block);
            self.b.set_cold_block(block);
            let function = self.b.ins().iconst(I32, self.index as i64);
            // `u32::MAX`, for no instruction, is -1 as an `I32` constant.
            let at = self.b.ins().iconst(I32, ip as i32 as i64);
            match exit {
                Exit::Error(code) => {
                    let code = self.b.ins().iconst(I64, code);
                    self.b
                        .ins()
                        .call(self.runtime.error, &[self.rt, code, function, at]);
                }
                Exit::At => {
                    self.b.ins().call(self.runtime.at, &[self.rt, function, at]);
                }
                Exit::Trace(callee) => {
                    let slot = self.addr(callee);
                    self.b
                        .ins()
                        .call(self.runtime.trace, &[self.rt, function, at, slot]);
                }
            }
            let failed = self.b.ins().iconst(I64, STATUS_ERROR);
            self.b.ins().return_(&[failed]);
        }
        if let Some(block) = self.stopped {
            self.b.switch_to_block(block);
            self.b.set_cold_block(block);
            let stopped = self.b.ins().iconst(I64, STATUS_STOPPED);
            self.b.ins().return_(&[stopped]);
        }
        Ok(())
    }

    fn ptr(&self) -> cranelift_codegen::ir::Type {
        self.cx.module.target_config().pointer_type()
    }

    // ---- Registers ----------------------------------------------------------

    /// Where register `r` is, as an offset from `base`.
    fn at(r: Reg) -> i32 {
        r as i32 * VALUE_SIZE
    }

    /// The address of register `r`.
    fn addr(&mut self, r: Reg) -> Value {
        self.b.ins().iadd_imm_s(self.base, Self::at(r) as i64)
    }

    /// The end of this function's window.
    fn top(&mut self) -> Value {
        self.addr(self.proto.num_regs)
    }

    /// Copy the value at `src + src_offset` to `dst + dst_offset`.
    fn copy(&mut self, dst: Value, dst_offset: i32, src: Value, src_offset: i32) {
        for word in (0..VALUE_SIZE).step_by(8) {
            let v = self.b.ins().load(I64, mem(), src, src_offset + word);
            self.b.ins().store(mem(), v, dst, dst_offset + word);
        }
    }

    fn move_(&mut self, dst: Reg, src: Reg) {
        if dst != src {
            self.copy(self.base, Self::at(dst), self.base, Self::at(src));
        }
    }

    fn int(&mut self, r: Reg) -> Value {
        self.b
            .ins()
            .load(I64, mem(), self.base, Self::at(r) + NUM_OFFSET)
    }

    fn float(&mut self, r: Reg) -> Value {
        self.b
            .ins()
            .load(F64, mem(), self.base, Self::at(r) + NUM_OFFSET)
    }

    /// The bool in `r`, as a byte that is 0 or 1.
    fn bool(&mut self, r: Reg) -> Value {
        self.b
            .ins()
            .load(I8, mem(), self.base, Self::at(r) + BOOL_OFFSET)
    }

    /// Write `tag`, and with it the rest of the value's first eight bytes
    /// (which hold no more than a bool), to `dst`.
    fn set_tag(&mut self, dst: Reg, tag: Value) {
        self.b.ins().store(mem(), tag, self.base, Self::at(dst));
    }

    fn set_int(&mut self, dst: Reg, v: Value) {
        let tag = self.b.ins().iconst(I64, TAG_INT);
        self.set_tag(dst, tag);
        self.b
            .ins()
            .store(mem(), v, self.base, Self::at(dst) + NUM_OFFSET);
    }

    fn set_float(&mut self, dst: Reg, v: Value) {
        let tag = self.b.ins().iconst(I64, TAG_FLOAT);
        self.set_tag(dst, tag);
        self.b
            .ins()
            .store(mem(), v, self.base, Self::at(dst) + NUM_OFFSET);
    }

    /// Store a bool: `v` is any integer that is 0 or 1.
    fn set_bool(&mut self, dst: Reg, v: Value) {
        let v = match self.b.func.dfg.value_type(v) {
            I64 => v,
            _ => self.b.ins().uextend(I64, v),
        };
        let v = self.b.ins().ishl_imm_s(v, BOOL_OFFSET as i64 * 8);
        let word = self.b.ins().bor_imm_s(v, TAG_BOOL);
        self.set_tag(dst, word);
    }

    // ---- Leaving on an error ------------------------------------------------

    /// A block that reports `exit` for instruction `ip` and returns.
    fn exit(&mut self, exit: Exit, ip: u32) -> Block {
        let block = self.b.create_block();
        self.exits.push((block, exit, ip));
        if ip != u32::MAX {
            self.sites.push(ip);
        }
        block
    }

    /// Leave by `exit` if `cond` isn't 0; otherwise go on.
    fn exit_if(&mut self, cond: Value, exit: Exit, ip: u32) {
        let failed = self.exit(exit, ip);
        let ok = self.b.create_block();
        self.b.ins().brif(cond, failed, &[], ok, &[]);
        self.b.switch_to_block(ok);
    }

    /// Where a task that was cancelled goes when it stops at instruction
    /// `ip` (`None` for the function's start): to the cleanup block that
    /// runs the deferred code pending there, or straight back to the
    /// caller.
    fn stop_target(&mut self, ip: Option<u32>) -> Block {
        let proto: &'a Proto = self.proto;
        match ip.and_then(|ip| proto.cleanup_at(ip as usize)) {
            Some(cleanup) => self.blocks[&(cleanup as u32)],
            None => match self.stopped {
                Some(block) => block,
                None => {
                    let block = self.b.create_block();
                    self.stopped = Some(block);
                    block
                }
            },
        }
    }

    /// After the call at `ip` gave `status`: go on if it is OK, leave by
    /// `exit` if an error is stopping the program, and otherwise the task
    /// was cancelled, and stops here.
    fn check_status(&mut self, status: Value, ip: u32, exit: Exit) {
        let (bad, ok) = (self.b.create_block(), self.b.create_block());
        self.b.ins().brif(status, bad, &[], ok, &[]);

        self.b.switch_to_block(bad);
        self.b.set_cold_block(bad);
        let is_error = self.b.ins().icmp_imm_s(IntCC::Equal, status, STATUS_ERROR);
        let failed = self.exit(exit, ip);
        let stop = self.stop_target(Some(ip));
        self.b.ins().brif(is_error, failed, &[], stop, &[]);

        self.b.switch_to_block(ok);
    }

    /// A safepoint, at instruction `ip` (`None` for the function's start):
    /// count it against the task's turn, and when the count runs out ask
    /// the runtime, which may run other tasks first, or say that this one
    /// was cancelled.
    fn safepoint(&mut self, ip: Option<u32>) {
        let left = self.b.ins().load(I64, mem(), self.rt, RT_BUDGET);
        let left = self.b.ins().iadd_imm_s(left, -1);
        self.b.ins().store(mem(), left, self.rt, RT_BUDGET);
        let out = self
            .b
            .ins()
            .icmp_imm_s(IntCC::SignedLessThanOrEqual, left, 0);
        let (ask, next) = (self.b.create_block(), self.b.create_block());
        self.b.ins().brif(out, ask, &[], next, &[]);

        self.b.switch_to_block(ask);
        self.b.set_cold_block(ask);
        let top = self.top();
        let flags = self.deferred_flag(ip);
        let flags = self.b.ins().iconst(I64, flags);
        let call = self
            .b
            .ins()
            .call(self.runtime.safepoint, &[self.rt, top, flags]);
        let stopped = self.b.inst_results(call)[0];
        let stop = self.stop_target(ip);
        self.b.ins().brif(stopped, stop, &[], next, &[]);

        self.b.switch_to_block(next);
    }

    /// [`IN_DEFERRED`] if instruction `ip` is in deferred code that a
    /// block runs on its way out, which a cancelled task finishes before
    /// it stops; otherwise 0.
    fn deferred_flag(&self, ip: Option<u32>) -> i64 {
        match ip {
            Some(ip) if self.proto.in_deferred(ip as usize) => IN_DEFERRED,
            _ => 0,
        }
    }

    /// Count a call that deferred code is making, or (with -1) one that
    /// has returned.
    fn count_deferred_call(&mut self, by: i64) {
        let calls = self.b.ins().load(I64, mem(), self.rt, RT_DEFERRED_CALLS);
        let calls = self.b.ins().iadd_imm_s(calls, by);
        self.b.ins().store(mem(), calls, self.rt, RT_DEFERRED_CALLS);
    }

    // ---- Instructions -------------------------------------------------------

    /// Translate the instruction at `ip`. Returns whether the one after
    /// it can be reached from it.
    fn op(&mut self, ip: u32, op: Op) -> Result<bool, String> {
        let r = self.runtime;
        let ptr = self.ptr();
        let (rt, base) = (self.rt, self.base);
        match op {
            Op::LoadVoid { dst } => {
                let tag = self.b.ins().iconst(I64, TAG_VOID);
                self.set_tag(dst, tag);
            }
            Op::LoadBool { dst, value } => {
                let v = self.b.ins().iconst(I64, value as i64);
                self.set_bool(dst, v);
            }
            Op::LoadInt { dst, value } => {
                let v = self.b.ins().iconst(I64, value);
                self.set_int(dst, v);
            }
            Op::LoadFloat { dst, value } => {
                let v = self.b.ins().f64const(value);
                self.set_float(dst, v);
            }
            Op::LoadStr { dst, index } => {
                let literals = self.b.ins().load(ptr, mem(), rt, RT_LITERALS);
                self.copy(base, Self::at(dst), literals, index as i32 * VALUE_SIZE);
            }
            Op::Move { dst, src } => self.move_(dst, src),

            Op::GetGlobal { dst, index } => {
                let globals = self.b.ins().load(ptr, mem(), rt, RT_GLOBALS);
                self.copy(base, Self::at(dst), globals, index as i32 * VALUE_SIZE);
                self.known.remove(&dst);
                if let Some(&f) = self.cx.by_slot.get(&index) {
                    self.known.insert(dst, f);
                }
                return Ok(true);
            }
            // Only the REPL's variables are globals that change.
            Op::SetGlobal { .. } => {
                return Err("internal error: a program assigns to a global".to_string())
            }

            Op::NewCell { dst, src } => {
                let (d, s, top) = (self.addr(dst), self.addr(src), self.top());
                self.b.ins().call(r.new_cell, &[rt, d, s, top]);
            }
            Op::GetCell { dst, cell } => {
                let (d, c) = (self.addr(dst), self.addr(cell));
                self.b.ins().call(r.get_cell, &[rt, d, c]);
            }
            Op::SetCell { cell, src } => {
                let (c, s) = (self.addr(cell), self.addr(src));
                self.b.ins().call(r.set_cell, &[rt, c, s]);
            }
            Op::GetUpvalue { dst, index } => {
                let (d, f) = (self.addr(dst), self.closure());
                let i = self.b.ins().iconst(I64, index as i64);
                self.b.ins().call(r.get_upvalue, &[rt, d, f, i]);
            }
            Op::GetUpvalueCell { dst, index } => {
                let (d, f) = (self.addr(dst), self.closure());
                let i = self.b.ins().iconst(I64, index as i64);
                self.b.ins().call(r.get_upvalue_cell, &[rt, d, f, i]);
            }
            Op::SetUpvalueCell { index, src } => {
                let (f, s) = (self.closure(), self.addr(src));
                let i = self.b.ins().iconst(I64, index as i64);
                self.b.ins().call(r.set_upvalue_cell, &[rt, f, i, s]);
            }
            Op::Closure { dst, proto } => {
                let inner = Rc::as_ptr(&self.proto.protos[proto as usize]);
                let number = self.cx.by_proto[&inner];
                let (d, top) = (self.addr(dst), self.top());
                let f = self.b.ins().iconst(I32, number as i64);
                self.b.ins().call(r.closure, &[rt, d, base, f, top]);
            }

            Op::NewData {
                dst,
                layout,
                start,
                count,
            } => {
                let number = self.cx.layout(&self.proto.layouts[layout as usize]);
                let (d, s, top) = (self.addr(dst), self.addr(start), self.top());
                let n = self.b.ins().iconst(I64, count as i64);
                let l = self.b.ins().iconst(I32, number as i64);
                self.b.ins().call(r.new_data, &[rt, d, s, n, l, top]);
            }
            Op::GetField { dst, obj, index } => {
                let (d, o) = (self.addr(dst), self.addr(obj));
                let i = self.b.ins().iconst(I64, index as i64);
                self.b.ins().call(r.get_field, &[rt, d, o, i]);
            }
            Op::NewList { dst, start, count } => {
                let (d, s, top) = (self.addr(dst), self.addr(start), self.top());
                let n = self.b.ins().iconst(I64, count as i64);
                self.b.ins().call(r.new_list, &[rt, d, s, n, top]);
            }
            Op::Index { dst, list, index } => {
                let (d, l, i) = (self.addr(dst), self.addr(list), self.addr(index));
                let call = self.b.ins().call(r.index, &[rt, d, l, i]);
                let failed = self.b.inst_results(call)[0];
                self.exit_if(failed, Exit::At, ip);
            }
            Op::TestTag { dst, obj, tag } => {
                let o = self.addr(obj);
                let t = self.b.ins().iconst(I32, tag as i64);
                let call = self.b.ins().call(r.test_tag, &[rt, o, t]);
                let is = self.b.inst_results(call)[0];
                self.set_bool(dst, is);
            }

            Op::AddI { dst, a, b } => self.checked(ip, dst, a, b, rt::ERR_ADD),
            Op::SubI { dst, a, b } => self.checked(ip, dst, a, b, rt::ERR_SUB),
            Op::MulI { dst, a, b } => self.checked(ip, dst, a, b, rt::ERR_MUL),
            Op::DivI { dst, a, b } => {
                let (x, y) = (self.int(a), self.int(b));
                let zero = self.b.ins().icmp_imm_s(IntCC::Equal, y, 0);
                self.exit_if(zero, Exit::Error(rt::ERR_DIV_ZERO), ip);
                // The only quotient that doesn't fit is `int::MIN / -1`.
                let min = self.b.ins().icmp_imm_s(IntCC::Equal, x, i64::MIN);
                let minus_one = self.b.ins().icmp_imm_s(IntCC::Equal, y, -1);
                let overflow = self.b.ins().band(min, minus_one);
                self.exit_if(overflow, Exit::Error(rt::ERR_DIV), ip);
                let v = self.b.ins().sdiv(x, y);
                self.set_int(dst, v);
            }
            Op::RemI { dst, a, b } => {
                let (x, y) = (self.int(a), self.int(b));
                let zero = self.b.ins().icmp_imm_s(IntCC::Equal, y, 0);
                self.exit_if(zero, Exit::Error(rt::ERR_MOD_ZERO), ip);
                // `int::MIN % -1` is 0, but some machines can't divide to
                // find that out: anything `% -1` is 0, so divide by 1.
                let minus_one = self.b.ins().icmp_imm_s(IntCC::Equal, y, -1);
                let one = self.b.ins().iconst(I64, 1);
                let y = self.b.ins().select(minus_one, one, y);
                let v = self.b.ins().srem(x, y);
                self.set_int(dst, v);
            }
            Op::NegI { dst, src } => {
                let x = self.int(src);
                let min = self.b.ins().icmp_imm_s(IntCC::Equal, x, i64::MIN);
                self.exit_if(min, Exit::Error(rt::ERR_NEG), ip);
                let v = self.b.ins().ineg(x);
                self.set_int(dst, v);
            }
            Op::AddF { dst, a, b } => {
                let (x, y) = (self.float(a), self.float(b));
                let v = self.b.ins().fadd(x, y);
                self.set_float(dst, v);
            }
            Op::SubF { dst, a, b } => {
                let (x, y) = (self.float(a), self.float(b));
                let v = self.b.ins().fsub(x, y);
                self.set_float(dst, v);
            }
            Op::MulF { dst, a, b } => {
                let (x, y) = (self.float(a), self.float(b));
                let v = self.b.ins().fmul(x, y);
                self.set_float(dst, v);
            }
            Op::DivF { dst, a, b } => {
                let (x, y) = (self.float(a), self.float(b));
                let v = self.b.ins().fdiv(x, y);
                self.set_float(dst, v);
            }
            Op::RemF { dst, a, b } => {
                let (x, y) = (self.float(a), self.float(b));
                let call = self.b.ins().call(r.rem_float, &[x, y]);
                let v = self.b.inst_results(call)[0];
                self.set_float(dst, v);
            }
            Op::NegF { dst, src } => {
                let x = self.float(src);
                let v = self.b.ins().fneg(x);
                self.set_float(dst, v);
            }
            Op::AddS { dst, a, b } => {
                let (d, x, y, top) = (self.addr(dst), self.addr(a), self.addr(b), self.top());
                let call = self.b.ins().call(r.add_str, &[rt, d, x, y, top]);
                let failed = self.b.inst_results(call)[0];
                self.exit_if(failed, Exit::At, ip);
            }

            Op::LtI { dst, a, b } => self.compare_ints(dst, a, b, IntCC::SignedLessThan),
            Op::LeI { dst, a, b } => self.compare_ints(dst, a, b, IntCC::SignedLessThanOrEqual),
            Op::EqI { dst, a, b } => self.compare_ints(dst, a, b, IntCC::Equal),
            Op::LtF { dst, a, b } => self.compare_floats(dst, a, b, FloatCC::LessThan),
            Op::LeF { dst, a, b } => self.compare_floats(dst, a, b, FloatCC::LessThanOrEqual),
            Op::EqF { dst, a, b } => self.compare_floats(dst, a, b, FloatCC::Equal),
            Op::EqB { dst, a, b } => {
                let (x, y) = (self.bool(a), self.bool(b));
                let v = self.b.ins().icmp(IntCC::Equal, x, y);
                self.set_bool(dst, v);
            }
            Op::EqS { dst, a, b } | Op::EqD { dst, a, b } => self.compare(dst, a, b, r.equal),
            Op::LtD { dst, a, b } => self.compare(dst, a, b, r.less),
            Op::LeD { dst, a, b } => self.compare(dst, a, b, r.less_or_equal),
            Op::Not { dst, src } => {
                let x = self.bool(src);
                let v = self.b.ins().bxor_imm_s(x, 1);
                self.set_bool(dst, v);
            }

            Op::Jump { target } => {
                // Every loop jumps back to its start: a safepoint, so a
                // loop gives other tasks turns.
                if target <= ip {
                    self.safepoint(Some(ip));
                }
                self.b.ins().jump(self.blocks[&target], &[]);
                return Ok(false);
            }
            Op::JumpIfFalse { cond, target } => {
                let c = self.bool(cond);
                let next = self.b.create_block();
                self.b.ins().brif(c, next, &[], self.blocks[&target], &[]);
                self.b.switch_to_block(next);
                return Ok(true);
            }
            Op::JumpIfTrue { cond, target } => {
                let c = self.bool(cond);
                let next = self.b.create_block();
                self.b.ins().brif(c, self.blocks[&target], &[], next, &[]);
                self.b.switch_to_block(next);
                return Ok(true);
            }
            Op::Call { dst, callee, argc } => {
                self.call(ip, dst, callee, argc);
                // The callee used the registers from its own slot up.
                self.known.retain(|&r, _| r < callee);
            }
            Op::TailCall { callee, argc } => {
                self.tail_call(ip, callee, argc);
                return Ok(false);
            }
            Op::Return { src } => {
                self.copy(base, -VALUE_SIZE, base, Self::at(src));
                let ok = self.b.ins().iconst(I64, STATUS_OK);
                self.b.ins().return_(&[ok]);
                return Ok(false);
            }
            Op::NoMatch => {
                let failed = self.exit(Exit::Error(rt::ERR_NO_MATCH), ip);
                self.b.ins().jump(failed, &[]);
                return Ok(false);
            }
            // The end of a cleanup block: the cancelled task has run this
            // frame's deferred code, and goes on stopping in the caller.
            Op::Unwind => {
                let stopped = self.b.ins().iconst(I64, STATUS_STOPPED);
                self.b.ins().return_(&[stopped]);
                return Ok(false);
            }
        }
        if let Some(dst) = written(op) {
            self.known.remove(&dst);
        }
        Ok(true)
    }

    /// The address of the slot holding the closure that is running.
    fn closure(&mut self) -> Value {
        self.b.ins().iadd_imm_s(self.base, -VALUE_SIZE as i64)
    }

    /// `+`, `-`, or `*` on ints, which stops the program on overflow.
    fn checked(&mut self, ip: u32, dst: Reg, a: Reg, b: Reg, error: i64) {
        let (x, y) = (self.int(a), self.int(b));
        let (v, overflow) = match error {
            rt::ERR_ADD => self.b.ins().sadd_overflow(x, y),
            rt::ERR_SUB => self.b.ins().ssub_overflow(x, y),
            _ => self.b.ins().smul_overflow(x, y),
        };
        self.exit_if(overflow, Exit::Error(error), ip);
        self.set_int(dst, v);
    }

    fn compare_ints(&mut self, dst: Reg, a: Reg, b: Reg, cc: IntCC) {
        let (x, y) = (self.int(a), self.int(b));
        let v = self.b.ins().icmp(cc, x, y);
        self.set_bool(dst, v);
    }

    fn compare_floats(&mut self, dst: Reg, a: Reg, b: Reg, cc: FloatCC) {
        let (x, y) = (self.float(a), self.float(b));
        let v = self.b.ins().fcmp(cc, x, y);
        self.set_bool(dst, v);
    }

    /// A comparison the runtime makes, which returns 0 or 1.
    fn compare(&mut self, dst: Reg, a: Reg, b: Reg, f: FuncRef) {
        let (x, y) = (self.addr(a), self.addr(b));
        let call = self.b.ins().call(f, &[self.rt, x, y]);
        let v = self.b.inst_results(call)[0];
        self.set_bool(dst, v);
    }

    // ---- Calls --------------------------------------------------------------

    /// How this function refers to compiled function number `f`.
    fn callee(&mut self, f: u32) -> FuncRef {
        *self.callees.entry(f).or_insert_with(|| {
            self.cx
                .module
                .declare_func_in_func(self.cx.ids[f as usize], self.b.func)
        })
    }

    /// Ask the runtime what the value in register `callee`, which the
    /// instruction at `ip` calls, is, running it if it is a native
    /// function: the address of an Aja function's code, or a status (see
    /// `aja_rt_call`). `argc` may have flags set.
    fn start_call(&mut self, ip: u32, callee: Reg, argc: i64) -> Value {
        self.sites.push(ip);
        let (slot, top) = (self.addr(callee), self.top());
        let argc = self.b.ins().iconst(I64, argc);
        let note = self.b.ins().iconst(I64, site_note(self.index, ip));
        let call = self.b.ins().call(
            self.runtime.call,
            &[self.rt, slot, argc, top, note, self.base],
        );
        self.b.inst_results(call)[0]
    }

    fn call(&mut self, ip: u32, dst: Reg, callee: Reg, argc: u8) {
        let window = self.addr(callee + 1);
        let deferred = self.deferred_flag(Some(ip));
        if let Some(&f) = self.known.get(&callee) {
            let f = self.callee(f);
            // Beside the callee: this call, this window, and that no tail
            // call has replaced the frame the call makes (see `layout`).
            // The runtime notes the same for a callee it has to look up.
            self.sites.push(ip);
            let slot = Self::at(callee);
            let note = self.b.ins().iconst(I64, site_note(self.index, ip));
            self.b
                .ins()
                .store(mem(), note, self.base, slot + CALL_NOTE_OFFSET);
            self.b
                .ins()
                .store(mem(), self.base, self.base, slot + CALLER_OFFSET);
            let none = self.b.ins().iconst(I64, 0);
            self.b
                .ins()
                .store(mem(), none, self.base, slot + TAIL_NOTE_OFFSET);
            if deferred != 0 {
                self.count_deferred_call(1);
            }
            let call = self.b.ins().call(f, &[self.rt, window]);
            let status = self.b.inst_results(call)[0];
            self.check_status(status, ip, Exit::Trace(callee));
            if deferred != 0 {
                self.count_deferred_call(-1);
            }
        } else {
            let code = self.start_call(ip, callee, argc as i64 | deferred);
            let (aja, native, done) = (
                self.b.create_block(),
                self.b.create_block(),
                self.b.create_block(),
            );
            let is_code =
                self.b
                    .ins()
                    .icmp_imm_u(IntCC::UnsignedGreaterThan, code, CALL_MAX_STATUS);
            self.b.ins().brif(is_code, aja, &[], native, &[]);

            self.b.switch_to_block(aja);
            if deferred != 0 {
                self.count_deferred_call(1);
            }
            let call = self
                .b
                .ins()
                .call_indirect(self.sig, code, &[self.rt, window]);
            let status = self.b.inst_results(call)[0];
            self.check_status(status, ip, Exit::Trace(callee));
            if deferred != 0 {
                self.count_deferred_call(-1);
            }
            self.b.ins().jump(done, &[]);

            // A native function has run.
            self.b.switch_to_block(native);
            self.check_status(code, ip, Exit::At);
            self.b.ins().jump(done, &[]);

            self.b.switch_to_block(done);
        }
        // Either way, the result is where the callee was.
        self.move_(dst, callee);
    }

    fn tail_call(&mut self, ip: u32, callee: Reg, argc: u8) {
        if let Some(&f) = self.known.get(&callee) {
            let f = self.callee(f);
            self.replace_frame(ip, callee, argc);
            self.b.ins().return_call(f, &[self.rt, self.base]);
            return;
        }
        let flags = self.deferred_flag(Some(ip)) | IS_TAIL_CALL;
        let code = self.start_call(ip, callee, argc as i64 | flags);
        let (aja, native) = (self.b.create_block(), self.b.create_block());
        let is_code = self
            .b
            .ins()
            .icmp_imm_u(IntCC::UnsignedGreaterThan, code, CALL_MAX_STATUS);
        self.b.ins().brif(is_code, aja, &[], native, &[]);

        self.b.switch_to_block(aja);
        self.replace_frame(ip, callee, argc);
        self.b
            .ins()
            .return_call_indirect(self.sig, code, &[self.rt, self.base]);

        // A native function has run: its result is this function's.
        self.b.switch_to_block(native);
        self.check_status(code, ip, Exit::At);
        self.copy(self.base, -VALUE_SIZE, self.base, Self::at(callee));
        let ok = self.b.ins().iconst(I64, STATUS_OK);
        self.b.ins().return_(&[ok]);
    }

    /// Before a tail call, make this function's window the callee's: put
    /// the callee where the running closure is and its arguments where
    /// the parameters are. What is beside the running closure stays,
    /// since the call that made this frame is still the one in progress,
    /// and the tail call, at `ip`, is noted there too, for an error's
    /// trace.
    fn replace_frame(&mut self, ip: u32, callee: Reg, argc: u8) {
        // A function value is all in its first eight bytes.
        let f = self.b.ins().load(I64, mem(), self.base, Self::at(callee));
        self.b.ins().store(mem(), f, self.base, -VALUE_SIZE);
        self.sites.push(ip);
        let note = self.b.ins().iconst(I64, site_note(self.index, ip));
        self.b
            .ins()
            .store(mem(), note, self.base, -VALUE_SIZE + TAIL_NOTE_OFFSET);
        // Each argument moves down, to a register no later argument is
        // still in.
        for i in 0..argc as Reg {
            self.move_(i, callee + 1 + i);
        }
    }
}

/// The register `op` writes its result to, if it has one.
fn written(op: Op) -> Option<Reg> {
    match op {
        Op::LoadVoid { dst }
        | Op::LoadBool { dst, .. }
        | Op::LoadInt { dst, .. }
        | Op::LoadFloat { dst, .. }
        | Op::LoadStr { dst, .. }
        | Op::Move { dst, .. }
        | Op::GetGlobal { dst, .. }
        | Op::NewCell { dst, .. }
        | Op::GetCell { dst, .. }
        | Op::GetUpvalue { dst, .. }
        | Op::GetUpvalueCell { dst, .. }
        | Op::Closure { dst, .. }
        | Op::NewData { dst, .. }
        | Op::GetField { dst, .. }
        | Op::NewList { dst, .. }
        | Op::Index { dst, .. }
        | Op::TestTag { dst, .. }
        | Op::AddI { dst, .. }
        | Op::SubI { dst, .. }
        | Op::MulI { dst, .. }
        | Op::DivI { dst, .. }
        | Op::RemI { dst, .. }
        | Op::NegI { dst, .. }
        | Op::AddF { dst, .. }
        | Op::SubF { dst, .. }
        | Op::MulF { dst, .. }
        | Op::DivF { dst, .. }
        | Op::RemF { dst, .. }
        | Op::NegF { dst, .. }
        | Op::AddS { dst, .. }
        | Op::LtI { dst, .. }
        | Op::LeI { dst, .. }
        | Op::LtF { dst, .. }
        | Op::LeF { dst, .. }
        | Op::EqI { dst, .. }
        | Op::EqF { dst, .. }
        | Op::EqB { dst, .. }
        | Op::EqS { dst, .. }
        | Op::EqD { dst, .. }
        | Op::LtD { dst, .. }
        | Op::LeD { dst, .. }
        | Op::Not { dst, .. }
        | Op::Call { dst, .. } => Some(dst),
        Op::SetGlobal { .. }
        | Op::SetCell { .. }
        | Op::SetUpvalueCell { .. }
        | Op::Jump { .. }
        | Op::JumpIfFalse { .. }
        | Op::JumpIfTrue { .. }
        | Op::TailCall { .. }
        | Op::Return { .. }
        | Op::NoMatch
        | Op::Unwind => None,
    }
}
