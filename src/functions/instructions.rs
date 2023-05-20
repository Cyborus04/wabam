use crate::{encode::WasmDecode, RefType, ValType, WasmEncode};

mod instr_macro;

/// An series of [`Instruction`]s.
/// 
/// `Expr`s are used in function bodies, element and data segment offsets, and 
/// global variable values.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub instructions: Vec<Instruction>,
}

impl WasmEncode for Expr {
    fn size(&self) -> usize {
        self.instructions.iter().map(|x| x.size()).sum::<usize>() + 1
    }

    fn encode(&self, v: &mut Vec<u8>) {
        for i in &self.instructions {
            i.encode(v);
        }
        Instruction::End.encode(v);
    }
}

impl WasmDecode for Expr {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::ErrorKind> {
        use Instruction::*;

        let mut instrs = Vec::new();
        let mut block_depth = 1;
        loop {
            let next_instr = Instruction::decode(buf)?;
            match &next_instr {
                Block(_) | Loop(_) | If(_) => block_depth += 1,
                End => block_depth -= 1,
                _ => (),
            }
            if block_depth == 0 {
                break;
            }
            instrs.push(next_instr);
        }
        Ok(Self {
            instructions: instrs,
        })
    }
}

impl From<Vec<Instruction>> for Expr {
    fn from(instructions: Vec<Instruction>) -> Self {
        Self { instructions }
    }
}

impl<const N: usize> From<[Instruction; N]> for Expr {
    fn from(value: [Instruction; N]) -> Self {
        Self { instructions: value.into() }
    }
}

impl From<Instruction> for Expr {
    fn from(instr: Instruction) -> Self {
        Self {
            instructions: vec![instr],
        }
    }
}

/// The core of it all, a single instruction.
/// 
/// Each variant has its own documentation.
/// 
/// It is recommended to create these with the [`instr`](crate::instr) or 
/// [`instrs`](crate::instrs) macros.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Control Instructions
    /// `unreachable`. Signature: `() -> trap`
    /// 
    /// Immediately traps.
    #[doc(alias("unreachable"))]
    Unreachable,
    /// `nop`. Signature: `() -> ()`
    /// 
    /// Does nothing
    #[doc(alias("nop"))]
    NoOp,
    /// `block`. Signature: `() -> ()`
    /// 
    /// Enters a new block. Branching instructions skip to the end of the block.
    #[doc(alias("block"))]
    Block(Option<ValType>),
    /// `loop`. Signature: `() -> ()`
    /// 
    /// Enters a new looping block. Branching instructions return to the start
    /// of the loop, reaching `end` exits the loop
    #[doc(alias("loop"))]
    Loop(Option<ValType>),
    /// `if`. Signature: `(cond: i32) -> ()`
    /// 
    /// - If `cond` is zero, skip to the corresponding `Else` instruction if 
    /// there is one, or the end of the block if not.
    /// - If `cond` is non-zero, continue executing. Jump to the end of the 
    /// block once the corresponding `Else` instruction is reached
    #[doc(alias("if"))]
    If(Option<ValType>),
    /// `else`. Signature: `() -> ()`
    /// 
    /// See [`If`](Instruction::If)
    #[doc(alias("else"))]
    Else,
    /// `end`. Signature: `(block signature..) -> (block signature..)`
    /// 
    /// The end of the block. This always exits the block, no matter if it was a
    /// loop or not
    #[doc(alias("end"))]
    End,
    /// `br`. Signature: `(block signature..) -> branch`
    /// 
    /// If this is at the end of the expression, it exits the expression. 
    /// Otherwise, it jumps to the continuation of the block at the specified 
    /// depth. For most blocks, that is the corresponding `end` instruction. For 
    /// loops, that is the start of the loop.
    #[doc(alias("br"))]
    Branch(u32),
    /// `br_if`. Signature: `(cond: i32) -> ()`
    /// 
    /// If `cond` is non-zero, this executes the `br` instruction. Otherwise, 
    /// it continues normally.
    #[doc(alias("br_if"))]
    BranchIf(u32),
    /// `br_table`. Signature: `(idx: i32) -> ()`
    /// 
    /// Executes the `br` at depth `depths[idx]`. If `idx` is out of bounds, 
    /// uses the failsafe depth instead.
    #[doc(alias("br_table"))]
    BranchTable {
        depths: Vec<u32>,
        failsafe: u32,
    },
    /// `return`. Signature: `(expr signature..) -> exits`
    /// 
    /// Returns from the current expression.
    #[doc(alias("return"))]
    Return,
    /// `call`. Signature is that of the called function.
    /// 
    /// Calls the function at the given index. Imported functions start at 0,
    /// functions defined in the module start after that.
    #[doc(alias("call"))]
    Call(u32),
    /// `call_indirect`. Signature: `(offset: i32, types[type_idx] inputs..) -> (types[type_idx] outputs..)`
    /// 
    /// Calls the function reference stored in `table_idx`-th table, at the 
    /// index of the top of the stack
    #[doc(alias("call_indirect"))]
    CallIndirect {
        type_idx: u32,
        table_idx: u32,
    },

    // Reference Instructions
    /// `ref.null`. Signature: `() -> (ref)`
    /// 
    /// Pushes a null reference of the given type to the stack.
    #[doc(alias("ref.null"))]
    RefNull(RefType),
    /// `ref.is_null`. Signature: `(val: reftype) -> (i32)`
    /// 
    /// Checks `val` is null. If it is, pushs 1. If not, pushs 0.
    #[doc(alias("ref.is_null"))]
    RefIsNull,
    /// `ref.func`. Signature: `() -> (funcref)`
    /// 
    /// Creates a reference to the given function, if it has been allowed by
    /// declarative data segments.
    #[doc(alias("ref.func"))]
    RefFunc(u32),

    // Parametric Instructions
    /// `drop`. Signature: `(x: valtype) -> ()`
    /// 
    /// Gets rid of `x`.
    #[doc(alias("drop"))]
    Drop,
    /// `select`. Signature: `(cond: i32, a: numtype, b: numtype) -> (numtype)`
    /// 
    /// If `cond` is 0, returns `b`. Otherwise, returns `a`.
    #[doc(alias("select"))]
    Select,

    // Variable Instructions
    /// `local.get`. Signature: `() -> (valtype)`
    /// 
    /// Returns the value in the local variable.
    #[doc(alias("local.get"))]
    LocalGet(u32),
    /// `local.set`. Signature: `(x: valtype) -> ()`
    /// 
    /// Sets the value of the local variable to `x`. The type of `x` must match
    /// the type of the local.
    #[doc(alias("local.set"))]
    LocalSet(u32),
    /// `local.tee`. Signature: `(x: valtype) -> (valtype)`
    /// 
    /// Like `local.set`, but returns `x` back.
    #[doc(alias("local.tee"))]
    LocalTee(u32),
    /// `global.get`. Signature: `() -> (valtype)`
    /// 
    /// Returns the value in the global variable.
    #[doc(alias("global.get"))]
    GlobalGet(u32),
    /// `global.set`. Signature: `(valtype) -> ()`
    /// 
    /// Sets the value of the global variable to `x`. The type of `x` must match
    /// the type of the global.
    #[doc(alias("global.set"))]
    GlobalSet(u32),

    // Table Instructions
    /// `table.get`. Signature: `(idx: i32) -> (reftype)`
    /// 
    /// Returns the value at the `idx`-th position in the table.
    #[doc(alias("table.get"))]
    TableGet(u32),
    /// `table.set`. Signature: `(val: reftype, idx: i32) -> ()`
    /// 
    /// Set the value at the `idx`-th position in the table to `val`.
    #[doc(alias("table.set"))]
    TableSet(u32),
    /// `table.size`. Signature: `() -> (i32)`
    /// 
    /// Returns the size of the table.
    #[doc(alias("table.size"))]
    TableSize(u32),
    /// `table.grow`. Signature: `(count: i32) -> ()`
    /// 
    /// Grows the given table by `count` values, filled with `ref.null`.
    #[doc(alias("table.grow"))]
    TableGrow(u32),
    /// `table.fill`. Signature: `(count: i32, val: reftype, offset: i32) -> ()`
    /// 
    /// Fills `offset..offset + count` of the table with `val`.
    #[doc(alias("table.fill"))]
    TableFill(u32),
    /// `table.copy`. Signature: `(count: i32, source: i32, destination: i32) -> ()`
    /// 
    /// Copies `source..source + count` from table A to 
    /// `destination..destination + count` of table B.
    #[doc(alias("table.copy"))]
    TableCopy {
        /// The index of table B
        dest_index: u32,
        /// The index of table A
        src_index: u32,
    },
    /// `table.init`. Signature: `(count: i32, source: i32, destination: i32) -> ()`
    /// 
    /// Copies `source..source + count` from the element segment to 
    /// `destination..destination + count` of the table.
    #[doc(alias("table.init"))]
    TableInit {
        table_index: u32,
        elem_index: u32,
    },
    /// `elem.drop`. Signature: `() -> ()`
    /// 
    /// Gets rid of the given element segment.
    #[doc(alias("elem.drop"))]
    ElemDrop(u32),

    // Memory Instructions
    /// `i32.load`. Signature: `(ptr: i32) -> (i32)`
    /// 
    /// Loads an `i32` from linear memory at `ptr + offset`.
    #[doc(alias("i32.load"))]
    I32Load {
        align: u32,
        offset: u32,
    },
    /// `i64.load`. Signature: `(ptr: i32) -> (i64)`
    /// 
    /// Loads an `i64` from linear memory at `ptr + offset`.
    #[doc(alias("i64.load"))]
    I64Load {
        align: u32,
        offset: u32,
    },
    /// `f32.load`. Signature: `(ptr: i32) -> (f32)`
    /// 
    /// Loads an `f32` from linear memory at `ptr + offset`.
    #[doc(alias("f32.load"))]
    F32Load {
        align: u32,
        offset: u32,
    },
    /// `f64.load`. Signature: `(ptr: i32) -> (f64)`
    /// 
    /// Loads an `f64` from linear memory at `ptr + offset`.
    #[doc(alias("f64.load"))]
    F64Load {
        align: u32,
        offset: u32,
    },

    /// `i32.load8_s`. Signature: `(ptr: i32) -> (i32)`
    /// 
    /// Loads a `u8` from linear memory at `ptr + offset` and sign extends it to 
    /// 32 bits.
    #[doc(alias("i32.load8_s"))]
    I32LoadS8 {
        align: u32,
        offset: u32,
    },
    /// `i32.load8_u`. Signature: `(ptr: i32) -> (i32)`
    /// 
    /// Loads a `u8` from linear memory at `ptr + offset` and extends it to 32 bits.
    #[doc(alias("i32.load8_u"))]
    I32LoadU8 {
        align: u32,
        offset: u32,
    },
    /// `i32.load16_s`. Signature: `(ptr: i32) -> (i32)`
    /// 
    /// Loads a `u16` from linear memory at `ptr + offset` and sign extends it 
    /// to 32 bits.
    #[doc(alias("i32.load16_s"))]
    I32LoadS16 {
        align: u32,
        offset: u32,
    },
    /// `i32.load16_u`. Signature: `(ptr: i32) -> (i32)`
    /// 
    /// Loads a `u16` from linear memory at `ptr + offset` and extends it to 32 bits.
    #[doc(alias("i32.load16_u"))]
    I32LoadU16 {
        align: u32,
        offset: u32,
    },

    /// `i64.load8_s`. Signature: `(i32) -> (i64)`
    /// 
    /// Loads a `u8` from linear memory at `ptr + offset` and sign extends it to 
    /// 64 bits.
    #[doc(alias("i64.load8_s"))]
    I64LoadS8 {
        align: u32,
        offset: u32,
    },
    /// `i64.load8_u`. Signature: `(i32) -> (i64)`
    /// 
    /// Loads a `u8` from linear memory at `ptr + offset` and extends it to 64 bits.
    #[doc(alias("i64.load8_u"))]
    I64LoadU8 {
        align: u32,
        offset: u32,
    },
    /// `i64.load16_s`. Signature: `(i32) -> (i64)`
    /// 
    /// Loads a `u16` from linear memory at `ptr + offset` and sign extends it 
    /// to 64 bits.
    #[doc(alias("i64.load16_s"))]
    I64LoadS16 {
        align: u32,
        offset: u32,
    },
    /// `i64.load16_u`. Signature: `(i32) -> (i64)`
    /// 
    /// Loads a `u16` from linear memory at `ptr + offset` and extends it to 64 bits.
    #[doc(alias("i64.load16_u"))]
    I64LoadU16 {
        align: u32,
        offset: u32,
    },
    /// `i64.load32_s`. Signature: `(i32) -> (i64)`
    /// 
    /// Loads a `u32` from linear memory at `ptr + offset` and sign extends it 
    /// to 64 bits.
    #[doc(alias("i64.load32_s"))]
    I64LoadS32 {
        align: u32,
        offset: u32,
    },
    /// `i64.load32_u`. Signature: `(i32) -> (i64)`
    /// 
    /// Loads a `u32` from linear memory at `ptr + offset` and extends it to 64 bits.
    #[doc(alias("i64.load32_u"))]
    I64LoadU32 {
        align: u32,
        offset: u32,
    },

    /// `i32.store`. Signature: `(val: i32, ptr: i32) -> ()`
    /// 
    /// Writes `val` to linear memory at `ptr + offset`.
    #[doc(alias("i32.store"))]
    I32Store {
        align: u32,
        offset: u32,
    },
    /// `i64.store`. Signature: `(val: i64, ptr: i32) -> ()`
    /// 
    /// Writes `val` to linear memory at `ptr + offset`.
    #[doc(alias("i64.store"))]
    I64Store {
        align: u32,
        offset: u32,
    },
    /// `f32.store`. Signature: `(val: f32, ptr: i32) -> ()`
    /// 
    /// Writes `val` to linear memory at `ptr + offset`.
    #[doc(alias("f32.store"))]
    F32Store {
        align: u32,
        offset: u32,
    },
    /// `f64.store`. Signature: `(val: f64, ptr: i32) -> ()`
    /// 
    /// Writes `val` to linear memory at `ptr + offset`.
    #[doc(alias("f64.store"))]
    F64Store {
        align: u32,
        offset: u32,
    },

    /// `i32.store8`. Signature: `(val: i32, ptr: i32) -> ()`
    /// 
    /// Writes the lower 8 bits of `val` to linear memory at `ptr + offset`.
    #[doc(alias("i32.store8"))]
    I32StoreI8 {
        align: u32,
        offset: u32,
    },
    /// `i32.store16`. Signature: `(val: i32, ptr: i32) -> ()`
    /// 
    /// Writes the lower 16 bits of `val` to linear memory at `ptr + offset`.
    #[doc(alias("i32.store16"))]
    I32StoreI16 {
        align: u32,
        offset: u32,
    },
    /// `i64.store8`. Signature: `(val: i64, ptr: i32) -> ()`
    /// 
    /// Writes the lower 8 bits of `val` to linear memory at `ptr + offset`.
    #[doc(alias("i64.store8"))]
    I64StoreI8 {
        align: u32,
        offset: u32,
    },
    /// `i64.store16`. Signature: `(val: i64, ptr: i32) -> ()`
    /// 
    /// Writes the lower 16 bits of `val` to linear memory at `ptr + offset`.
    #[doc(alias("i64.store16"))]
    I64StoreI16 {
        align: u32,
        offset: u32,
    },
    /// `i64.store32`. Signature: `(val: i64, ptr: i32) -> ()`
    /// 
    /// Writes the lower 32 bits of `val` to linear memory at `ptr + offset`.
    #[doc(alias("i64.store32"))]
    I64StoreI32 {
        align: u32,
        offset: u32,
    },

    /// `memory.size`. Signature: `() -> (i32)`
    /// 
    /// Returns the size of linear memory, in pages. A page is 64KiB.
    #[doc(alias("memory.size"))]
    MemorySize,
    /// `memory.grow`. Signature: `(extra: i32) -> (i32)`
    /// 
    /// Grows linear memory by `extra` pages, then returns the previous size in 
    /// pages. A page is 64KiB. 
    #[doc(alias("memory.grow"))]
    MemoryGrow,
    /// `memory.init`. Signature: `(ptr: i32) -> (i32)`
    /// 
    /// Copies the data segment into linear memory, starting at `ptr`.
    #[doc(alias("memory.init"))]
    MemoryInit(u32),
    /// `data.drop`. Signature: `() -> ()`
    /// 
    /// Gets rid of the given data segment.
    #[doc(alias("data.drop"))]
    DataDrop(u32),
    /// `memory.copy`. Signature: `(count: i32, src: i32, dest: i32) -> ()`
    /// 
    /// Copies the bytes at `src..src + count` to `dest..dest + count`.
    #[doc(alias("memory.copy"))]
    MemoryCopy,
    /// `memory.fill`. Signature: `(count: i32, val: i32, ptr: i32) -> ()`
    /// 
    /// Fills `ptr..ptr + count` of linear memory with the lower 8 bits of `val`
    #[doc(alias("memory.fill"))]
    MemoryFill,

    // Numeric Instructions
    /// `i32.const`. Signature: `() -> (i32)`
    /// 
    /// Returns the immediate value.
    #[doc(alias("i32.const"))]
    I32Const(i32),
    /// `i64.const`. Signature: `() -> (i64)`
    /// 
    /// Returns the immediate value.
    #[doc(alias("i64.const"))]
    I64Const(i64),
    /// `f32.const`. Signature: `() -> (f32)`
    /// 
    /// Returns the immediate value.
    #[doc(alias("f32.const"))]
    F32Const(f32),
    /// `f64.const`. Signature: `() -> (f64)`
    /// 
    /// Returns the immediate value.
    #[doc(alias("f64.const"))]
    F64Const(f64),

    /// `i32.eqz`. Signature: `(a: i32) -> (i32)`
    /// 
    /// Returns `a == 0`
    #[doc(alias("i32.eqz"))]
    I32EqualsZero,
    /// `i32.eq`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a == b`
    #[doc(alias("i32.eq"))]
    I32Equal,
    /// `i32.ne`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a != b`
    #[doc(alias("i32.ne"))]
    I32NotEqual,
    /// `i32.lt_s`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as i32 < b as i32`
    #[doc(alias("i32.lt_s"))]
    S32LessThan,
    /// `i32.lt_u`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as u32 < b as u32`
    #[doc(alias("i32.lt_u"))]
    U32LessThan,
    /// `i32.gt_s`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as i32 > b as i32`
    #[doc(alias("i32.gt_s"))]
    S32GreaterThan,
    /// `i32.gt_u`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as u32 > b as u32`
    #[doc(alias("i32.gt_u"))]
    U32GreaterThan,
    /// `i32.le_s`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as i32 <= b as i32`
    #[doc(alias("i32.le_s"))]
    S32LessThanOrEqual,
    /// `i32.le_u`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as u32 <= b as u32`
    #[doc(alias("i32.le_u"))]
    U32LessThanOrEqual,
    /// `i32.ge_s`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as i32 >= b as i32`
    #[doc(alias("i32.ge_s"))]
    S32GreaterThanOrEqual,
    /// `i32.ge_u`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as u32 >= b as u32`
    #[doc(alias("i32.ge_u"))]
    U32GreaterThanOrEqual,

    /// `i64.eqz`. Signature: `(a: i64) -> (i32)`
    /// 
    /// Returns `a == 0`
    #[doc(alias("i64.eqz"))]
    I64EqualsZero,
    /// `i64.eq`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a == b`
    #[doc(alias("i64.eq"))]
    I64Equal,
    /// `i64.ne`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a != b`
    #[doc(alias("i64.ne"))]
    I64NotEqual,
    /// `i64.lt_s`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as i64 < b as i64`
    #[doc(alias("i64.lt_s"))]
    S64LessThan,
    /// `i64.lt_u`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as u64 < b as u64`
    #[doc(alias("i64.lt_u"))]
    U64LessThan,
    /// `i64.gt_s`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as i64 > b as i64`
    #[doc(alias("i64.gt_s"))]
    S64GreaterThan,
    /// `i64.gt_u`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as u64 > b as u64`
    #[doc(alias("i64.gt_u"))]
    U64GreaterThan,
    /// `i64.le_s`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as i64 <= b as i64`
    #[doc(alias("i64.le_s"))]
    S64LessThanOrEqual,
    /// `i64.le_u`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as u64 <= b as u64`
    #[doc(alias("i64.le_u"))]
    U64LessThanOrEqual,
    /// `i64.ge_s`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as i64 >= b as i64`
    #[doc(alias("i64.ge_s"))]
    S64GreaterThanOrEqual,
    /// `i64.ge_u`. Signature: `(b: i64, a: i64) -> (i32)`
    /// 
    /// Returns `a as u64 >= b as u64`
    #[doc(alias("i64.ge_u"))]
    U64GreaterThanOrEqual,

    /// `f32.eq`. Signature: `(b: f32, a: f32) -> (i32)`
    /// 
    /// Returns `a == b`
    #[doc(alias("f32.eq"))]
    F32Equal,
    /// `f32.ne`. Signature: `(b: f32, a: f32) -> (i32)`
    /// 
    /// Returns `a != b`
    #[doc(alias("f32.ne"))]
    F32NotEqual,
    /// `f32.lt`. Signature: `(b: f32, a: f32) -> (i32)`
    /// 
    /// Returns `a < b`
    #[doc(alias("f32.lt"))]
    F32LessThan,
    /// `f32.gt`. Signature: `(b: f32, a: f32) -> (i32)`
    /// 
    /// Returns `a > b`
    #[doc(alias("f32.gt"))]
    F32GreaterThan,
    /// `f32.le`. Signature: `(b: f32, a: f32) -> (i32)`
    /// 
    /// Returns `a <= b`
    #[doc(alias("f32.le"))]
    F32LessThanOrEqual,
    /// `f32.ge`. Signature: `(b: f32, a: f32) -> (i32)`
    /// 
    /// Returns `a >= b`
    #[doc(alias("f32.ge"))]
    F32GreaterThanOrEqual,

    /// `f64.eq`. Signature: `(b: f64, a: f64) -> (i32)`
    /// 
    /// Returns `a == b`
    #[doc(alias("f64.eq"))]
    F64Equal,
    /// `f64.ne`. Signature: `(b: f64, a: f64) -> (i32)`
    /// 
    /// Returns `a != b`
    #[doc(alias("f64.ne"))]
    F64NotEqual,
    /// `f64.lt`. Signature: `(b: f64, a: f64) -> (i32)`
    /// 
    /// Returns `a < b`
    #[doc(alias("f64.lt"))]
    F64LessThan,
    /// `f64.gt`. Signature: `(b: f64, a: f64) -> (i32)`
    /// 
    /// Returns `a > b`
    #[doc(alias("f64.gt"))]
    F64GreaterThan,
    /// `f64.le`. Signature: `(b: f64, a: f64) -> (i32)`
    /// 
    /// Returns `a <= b`
    #[doc(alias("f64.le"))]
    F64LessThanOrEqual,
    /// `f64.ge`. Signature: `(b: f64, a: f64) -> (i32)`
    /// 
    /// Returns `a >= b`
    #[doc(alias("f64.ge"))]
    F64GreaterThanOrEqual,

    /// `i32.clz`. Signature: `(a: i32) -> (i32)`
    /// 
    /// Returns `a.leading_zeroes()`
    #[doc(alias("i32.clz"))]
    I32CountLeadingZeroes,
    /// `i32.ctz`. Signature: `(a: i32) -> (i32)`
    /// 
    /// Returns `a.trailing_zeroes()`
    #[doc(alias("i32.ctz"))]
    I32CountTrailingZeroes,
    /// `i32.popcnt`. Signature: `(a: i32) -> (i32)`
    /// 
    /// Returns `a.count_ones()`
    #[doc(alias("i32.popcnt"))]
    I32CountOnes,
    /// `i32.add`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a + b`
    #[doc(alias("i32.add"))]
    I32Add,
    /// `i32.sub`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a - b`
    #[doc(alias("i32.sub"))]
    I32Sub,
    /// `i32.mul`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a - b`
    #[doc(alias("i32.mul"))]
    I32Mul,
    /// `i32.div_s`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as i32 / b as i32`
    #[doc(alias("i32.div_s"))]
    S32Div,
    /// `i32.div_u`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as u32 / b as u32`
    #[doc(alias("i32.div_u"))]
    U32Div,
    /// `i32.rem_s`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as i32 % b as i32`
    #[doc(alias("i32.rem_s"))]
    S32Rem,
    /// `i32.rem_u`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as u32 % b as u32`
    #[doc(alias("i32.rem_u"))]
    U32Rem,
    /// `i32.and`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a & b`
    #[doc(alias("i32.and"))]
    I32And,
    /// `i32.or`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a | b`
    #[doc(alias("i32.or"))]
    I32Or,
    /// `i32.xor`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a ^ b`
    #[doc(alias("i32.xor"))]
    I32Xor,
    /// `i32.shl`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a << b`
    #[doc(alias("i32.shl"))]
    I32ShiftLeft,
    /// `i32.shr_s`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as i32 >> b`
    #[doc(alias("i32.shr_s"))]
    S32ShiftRight,
    /// `i32.shr_u`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a as u32 >> b`
    #[doc(alias("i32.shr_u"))]
    U32ShiftRight,
    /// `i32.rotl`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a.rotate_left(b)`
    #[doc(alias("i32.rotl"))]
    I32RotateLeft,
    /// `i32.rotr`. Signature: `(b: i32, a: i32) -> (i32)`
    /// 
    /// Returns `a.rotate_right(b)`
    #[doc(alias("i32.rotr"))]
    I32RotateRight,

    /// `i64.clz`. Signature: `(a: i64) -> (i64)`
    /// 
    /// Returns `a.leading_zeroes()`
    #[doc(alias("i64.clz"))]
    I64CountLeadingZeroes,
    /// `i64.ctz`. Signature: `(a: i64) -> (i64)`
    /// 
    /// Returns `a.trailing_zeroes()`
    #[doc(alias("i64.ctz"))]
    I64CountTrailingZeroes,
    /// `i64.popcnt`nt`. Signature: `(a: i64) -> (i64)`
    /// 
    /// Returns `a.count_ones()`
    #[doc(alias("i64.popcnt"))]
    I64CountOnes,
    /// `i64.add`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a + b`
    #[doc(alias("i64.add"))]
    I64Add,
    /// `i64.sub`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a - b`
    #[doc(alias("i64.sub"))]
    I64Sub,
    /// `i64.mul`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a - b`
    #[doc(alias("i64.mul"))]
    I64Mul,
    /// `i64.div_s`s`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a as i64 / b as i64`
    #[doc(alias("i64.div_s"))]
    S64Div,
    /// `i64.div_u`u`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a as u64 / b as u64`
    #[doc(alias("i64.div_u"))]
    U64Div,
    /// `i64.rem_s`s`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a as i64 % b as i64`
    #[doc(alias("i64.rem_s"))]
    S64Rem,
    /// `i64.rem_u`u`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a as u64 % b as u64`
    #[doc(alias("i64.rem_u"))]
    U64Rem,
    /// `i64.and`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a & b`
    #[doc(alias("i64.and"))]
    I64And,
    /// `i64.or` Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a | b`
    #[doc(alias("i64.or"))]
    I64Or,
    /// `i64.xor`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a ^ b`
    #[doc(alias("i64.xor"))]
    I64Xor,
    /// `i64.shl`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a << b`
    #[doc(alias("i64.shl"))]
    I64ShiftLeft,
    /// `i64.shr_s`s`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a as i64 >> b`
    #[doc(alias("i64.shr_s"))]
    S64ShiftRight,
    /// `i64.shr_u`u`. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a as u64 >> b`
    #[doc(alias("i64.shr_u"))]
    U64ShiftRight,
    /// `i64.rotl``. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a.rotate_left(b)`
    #[doc(alias("i64.rotl"))]
    I64RotateLeft,
    /// `i64.rotr``. Signature: `(b: i64, a: i64) -> (i64)`
    /// 
    /// Returns `a.rotate_right(b)`
    #[doc(alias("i64.rotr"))]
    I64RotateRight,

    /// `f32.abs`. Signature: `(a: f32) -> (f32)`
    /// 
    /// Returns `a.abs()`
    #[doc(alias("f32.abs"))]
    F32AbsoluteValue,
    /// `f32.neg`. Signature: `(a: f32) -> (f32)`
    /// 
    /// Returns `-a`
    #[doc(alias("f32.neg"))]
    F32Negate,
    /// `f32.ceil`. Signature: `(a: f32) -> (f32)`
    /// 
    /// Returns `a.ceil()`
    #[doc(alias("f32.ceil"))]
    F32Ceiling,
    /// `f32.floor`. Signature: `(a: f32) -> (f32)`
    /// 
    /// Returns `a.floor()`
    #[doc(alias("f32.floor"))]
    F32Floor,
    /// `f32.trunc`. Signature: `(a: f32) -> (f32)`
    /// 
    /// Returns `a.trunc()`
    #[doc(alias("f32.trunc"))]
    F32Truncate,
    /// `f32.nearest`. Signature: `(a: f32) -> (f32)`
    /// 
    /// Returns `a.round()`
    #[doc(alias("f32.nearest"))]
    F32Nearest,
    /// `f32.sqrt`. Signature: `(a: f32) -> (f32)`
    /// 
    /// Returns `a.sqrt()`
    #[doc(alias("f32.sqrt"))]
    F32SquareRoot,
    /// `f32.add`. Signature: `(b: f32, a: f32) -> (f32)`
    /// 
    /// Returns `a + b`
    #[doc(alias("f32.add"))]
    F32Add,
    /// `f32.sub`. Signature: `(b: f32, a: f32) -> (f32)`
    /// 
    /// Returns `a - b`
    #[doc(alias("f32.sub"))]
    F32Sub,
    /// `f32.mul`. Signature: `(b: f32, a: f32) -> (f32)`
    /// 
    /// Returns `a * b`
    #[doc(alias("f32.mul"))]
    F32Mul,
    /// `f32.div`. Signature: `(b: f32, a: f32) -> (f32)`
    /// 
    /// Returns `a / b`
    #[doc(alias("f32.div"))]
    F32Div,
    /// `f32.min`. Signature: `(b: f32, a: f32) -> (f32)`
    /// 
    /// Returns `a.min(b)`
    #[doc(alias("f32.min"))]
    F32Min,
    /// `f32.max`. Signature: `(b: f32, a: f32) -> (f32)`
    /// 
    /// Returns `a.max(b)`
    #[doc(alias("f32.max"))]
    F32Max,
    /// `f32.copysign`. Signature: `(b: f32, a: f32) -> (f32)`
    /// 
    /// Returns `a.copy_sign(b)`
    #[doc(alias("f32.copysign"))]
    F32CopySign,

    /// `f64.abs`. Signature: `(a: f64) -> (f64)`
    /// 
    /// Returns `a.abs()`
    #[doc(alias("f64.abs"))]
    F64AbsoluteValue,
    /// `f64.neg`. Signature: `(a: f64) -> (f64)`
    /// 
    /// Returns `-a`
    #[doc(alias("f64.neg"))]
    F64Negate,
    /// `f64.ceil`. Signature: `(a: f64) -> (f64)`
    /// 
    /// Returns `a.ceil()`
    #[doc(alias("f64.ceil"))]
    F64Ceiling,
    /// `f64.floor`. Signature: `(a: f64) -> (f64)`
    /// 
    /// Returns `a.floor()`
    #[doc(alias("f64.floor"))]
    F64Floor,
    /// `f64.trunc`. Signature: `(a: f64) -> (f64)`
    /// 
    /// Returns `a.trunc()`
    #[doc(alias("f64.trunc"))]
    F64Truncate,
    /// `f64.nearest`. Signature: `(a: f64) -> (f64)`
    /// 
    /// Returns `a.round()`
    #[doc(alias("f64.nearest"))]
    F64Nearest,
    /// `f64.sqrt`. Signature: `(a: f64) -> (f64)`
    /// 
    /// Returns `a.sqrt()`
    #[doc(alias("f64.sqrt"))]
    F64SquareRoot,
    /// `f64.add`. Signature: `(b: f64, a: f64) -> (f64)`
    /// 
    /// Returns `a + b`
    #[doc(alias("f64.add"))]
    F64Add,
    /// `f64.sub`. Signature: `(b: f64, a: f64) -> (f64)`
    /// 
    /// Returns `a - b`
    #[doc(alias("f64.sub"))]
    F64Sub,
    /// `f64.mul`. Signature: `(b: f64, a: f64) -> (f64)`
    /// 
    /// Returns `a * b`
    #[doc(alias("f64.mul"))]
    F64Mul,
    /// `f64.div`. Signature: `(b: f64, a: f64) -> (f64)`
    /// 
    /// Returns `a / b`
    #[doc(alias("f64.div"))]
    F64Div,
    /// `f64.min`. Signature: `(b: f64, a: f64) -> (f64)`
    /// 
    /// Returns `a.min(b)`
    #[doc(alias("f64.min"))]
    F64Min,
    /// `f64.max`. Signature: `(b: f64, a: f64) -> (f64)`
    /// 
    /// Returns `a.max(b)`
    #[doc(alias("f64.max"))]
    F64Max,
    /// `f64.copysign`. Signature: `(b: f64, a: f64) -> (f64)`
    /// 
    /// Returns `a.copy_sign(b)`
    #[doc(alias("f64.copysign"))]
    F64CopySign,

    /// `i32.wrap_i64`. Signature: `(a: i64) -> (i32)`
    /// 
    /// Returns `a` truncated to 32 bits.
    #[doc(alias("i32.wrap_i64"))]
    I32WrapI64,
    /// `i32.trunc_f32_s`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer.
    #[doc(alias("i32.trunc_f32_s"))]
    S32TruncateF32,
    /// `i32.trunc_f32_u`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer.
    #[doc(alias("i32.trunc_f32_u"))]
    U32TruncateF32,
    /// `i32.trunc_f64_s`. Signature: `(a: f64) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer.
    #[doc(alias("i32.trunc_f64_s"))]
    S32TruncateF64,
    /// `i32.trunc_f64_u`. Signature: `(a: f64) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer.
    #[doc(alias("i32.trunc_f64_u"))]
    U32TruncateF64,
    /// `i64.extend_i32_s`. Signature: `(a: i32) -> (i64)`
    /// 
    /// Sign extends `a` to 64 bits.
    #[doc(alias("i64.extend_i32_s"))]
    I64ExtendS32,
    /// `i64.extend_i32_u`. Signature: `(a: i32) -> (i64)`
    /// 
    /// Extends `a` to 64 bits.
    #[doc(alias("i64.extend_i32_u"))]
    I64ExtendU32,
    /// `i64.trunc_f32_s`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer.
    #[doc(alias("i64.trunc_f32_s"))]
    S64TruncateF32,
    /// `i64.trunc_f32_u`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer.
    #[doc(alias("i64.trunc_f32_u"))]
    U64TruncateF32,
    /// `i64.trunc_f64_s`. Signature: `(a: f64) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer.
    #[doc(alias("i64.trunc_f64_s"))]
    S64TruncateF64,
    /// `i64.trunc_f64_u`. Signature: `(a: f64) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer.
    #[doc(alias("i64.trunc_f64_u"))]
    U64TruncateF64,
    /// `f32.convert_i32_s`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Returns `a as i32 as f32`.
    #[doc(alias("f32.convert_i32_s"))]
    F32ConvertS32,
    /// `f32.convert_i32_u`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Returns `a as u32 as f32`.
    #[doc(alias("f32.convert_i32_u"))]
    F32ConvertU32,
    /// `f32.convert_i64_s`. Signature: `(a: i64) -> (f32)`
    /// 
    /// Returns `a as i64 as f32`.
    #[doc(alias("f32.convert_i64_s"))]
    F32ConvertS64,
    /// `f32.convert_i64_u`. Signature: `(a: i64) -> (f32)`
    /// 
    /// Returns `a as u64 as f32`.
    #[doc(alias("f32.convert_i64_u"))]
    F32ConvertU64,
    /// `f32.demote_f64`. Signature: `(a: f64) -> (f32)`
    /// 
    /// Returns `a as f32`.
    #[doc(alias("f32.demote_f64"))]
    F32DemoteF64,
    /// `f64.convert_i32_s`. Signature: `(a: i32) -> (f64)`
    /// 
    /// Returns `a as i32 as f64`.
    #[doc(alias("f64.convert_i32_s"))]
    F64ConvertS32,
    /// `f64.convert_i32_u`. Signature: `(a: i32) -> (f64)`
    /// 
    /// Returns `a as u32 as f64`.
    #[doc(alias("f64.convert_i32_u"))]
    F64ConvertU32,
    /// `f64.convert_i64_s`. Signature: `(a: i64) -> (f64)`
    /// 
    /// Returns `a as i64 as f64`.
    #[doc(alias("f64.convert_i64_s"))]
    F64ConvertS64,
    /// `f64.convert_i64_u`. Signature: `(a: i64) -> (f64)`
    /// 
    /// Returns `a as u64 as f64`.
    #[doc(alias("f64.convert_i64_u"))]
    F64ConvertU64,
    /// `f64.promote_f32`. Signature: `(a: f32) -> (f64)`
    /// 
    /// Returns `a as f64`.
    #[doc(alias("f64.promote_f32"))]
    F64PromoteF32,
    /// `i32.reinterpret_f32`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a.to_bits()`.
    #[doc(alias("i32.reinterpret_f32"))]
    I32ReinterpretF32,
    /// `i64.reinterpret_f64`. Signature: `(a: f64) -> (i64)`
    /// 
    /// Returns `a.to_bits()`.
    #[doc(alias("i64.reinterpret_f64"))]
    I64ReinterpretF64,
    /// `f32.reinterpret_i32`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Returns `f32::from_bits(a)`.
    #[doc(alias("f32.reinterpret_i32"))]
    F32ReinterpretI32,
    /// `f64.reinterpret_i64`. Signature: `(a: i64) -> (f64)`
    /// 
    /// Returns `f64::from_bits(a)`.
    #[doc(alias("f64.reinterpret_i64"))]
    F64ReinterpretI64,

    /// `i32.extend8_s`. Signature: `(a: i32) -> (i32)`
    /// 
    /// Sign extends the lower 8 bits of `a`
    #[doc(alias("i32.extend8_s"))]
    S32Extend8,
    /// `i32.extend16_s`. Signature: `(a: i32) -> (i32)`
    /// 
    /// Sign extends the lower 16 bits of `a`
    #[doc(alias("i32.extend16_s"))]
    S32Extend16,
    /// `i64.extend8_s`. Signature: `(a: i64) -> (i64)`
    /// 
    /// Sign extends the lower 8 bits of `a`
    #[doc(alias("i64.extend8_s"))]
    S64Extend8,
    /// `i64.extend16_s`. Signature: `(a: i64) -> (i64)`
    /// 
    /// Sign extends the lower 16 bits of `a`
    #[doc(alias("i64.extend16_s"))]
    S64Extend16,
    /// `i64.extend32_s`. Signature: `(a: i64) -> (i64)`
    /// 
    /// Sign extends the lower 32 bits of `a`
    #[doc(alias("i64.extend32_s"))]
    S64Extend32,

    /// `i32.trunc_sat_f32_s`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer, saturating to `i32::MAX`
    #[doc(alias("i32.trunc_sat_f32_s"))]
    S32SaturatingTruncateF32,
    /// `i32.trunc_sat_f32_u`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer, saturating to `u32::MAX`
    #[doc(alias("i32.trunc_sat_f32_u"))]
    U32SaturatingTruncateF32,
    /// `i32.trunc_sat_f64_s`. Signature: `(a: f64) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer, saturating to `i32::MAX`
    #[doc(alias("i32.trunc_sat_f64_s"))]
    S32SaturatingTruncateF64,
    /// `i32.trunc_sat_f64_u`. Signature: `(a: f64) -> (i32)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer, saturating to `i32::MAX`
    #[doc(alias("i32.trunc_sat_f64_u"))]
    U32SaturatingTruncateF64,
    /// `i64.trunc_sat_f32_s`. Signature: `(a: f32) -> (i64)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer, saturating to `i64::MAX`
    #[doc(alias("i64.trunc_sat_f32_s"))]
    S64SaturatingTruncateF32,
    /// `i64.trunc_sat_f32_u`. Signature: `(a: f32) -> (i64)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer, saturating to `u64::MAX`
    #[doc(alias("i64.trunc_sat_f32_u"))]
    U64SaturatingTruncateF32,
    /// `i64.trunc_sat_f64_s`. Signature: `(a: f64) -> (i64)`
    /// 
    /// Returns `a` rounded towards zero, as a signed integer, saturating to `i64::MAX`
    #[doc(alias("i64.trunc_sat_f64_s"))]
    S64SaturatingTruncateF64,
    /// `i64.trunc_sat_f64_u`. Signature: `(a: f64) -> (i64)`
    /// 
    /// Returns `a` rounded towards zero, as an unsigned integer, saturating to `i64::MAX`
    #[doc(alias("i64.trunc_sat_f64_u"))]
    U64SaturatingTruncateF64,

    /// `v128.load`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads a `v128` from linear memory at `ptr + offset`.
    #[doc(alias("v128.load"))]
    V128Load {
        align: u32,
        offset: u32,
    },
    /// `v128.load8x8_s`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads 8 contiguous 8-bit values from linear memory at `ptr`, sign 
    /// extends them to 16 bits, then puts those in the lanes of an `i16x8`.
    #[doc(alias("v128.load8x8_s"))]
    V128LoadS8x8 {
        align: u32,
        offset: u32,
    },
    /// `v128.load8x8_u`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads 8 contiguous 8-bit values from linear memory at `ptr`, extends 
    /// them to 16 bits, then puts those in the lanes of an `i16x8`.
    #[doc(alias("v128.load8x8_u"))]
    V128LoadU8x8 {
        align: u32,
        offset: u32,
    },
    /// `v128.load16x4_s`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads 4 contiguous 16-bit values from linear memory at `ptr`, sign 
    /// extends them to 32 bits, then puts those in the lanes of an `i32x4`.
    #[doc(alias("v128.load16x4_s"))]
    V128LoadS16x4 {
        align: u32,
        offset: u32,
    },
    /// `v128.load16x4_u`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads 4 contiguous 32-bit values from linear memory at `ptr`, extends 
    /// them to 32 bits, then puts those in the lanes of an `i32x4`.
    #[doc(alias("v128.load16x4_u"))]
    V128LoadU16x4 {
        align: u32,
        offset: u32,
    },
    /// `v128.load32x2_s`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads 2 contiguous 32-bit values from linear memory at `ptr`, sign 
    /// extends them to 64 bits, then puts those in the lanes of an `i64x2`.
    #[doc(alias("v128.load32x2_s"))]
    V128LoadS32x2 {
        align: u32,
        offset: u32,
    },
    /// `v128.load32x2_u`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads 2 contiguous 32-bit values from linear memory at `ptr`, extends 
    /// them to 64 bits, then puts those in the lanes of an `i64x2`.
    #[doc(alias("v128.load32x2_u"))]
    V128LoadU32x2 {
        align: u32,
        offset: u32,
    },
    /// `v128.load8_splat`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads an 8-bit value from linear memory at `ptr`, and copies it into 
    /// every lane of an `i8x16`.
    #[doc(alias("v128.load8_splat"))]
    V128LoadSplatI8 {
        align: u32,
        offset: u32,
    },
    /// `v128.load16_splat`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads a 16-bit value from linear memory at `ptr`, and copies it into 
    /// every lane of an `i16x8`.
    #[doc(alias("v128.load16_splat"))]
    V128LoadSplatI16 {
        align: u32,
        offset: u32,
    },
    /// `v128.load32_splat`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads a 32-bit value from linear memory at `ptr`, and copies it into 
    /// every lane of an `i32x4`.
    #[doc(alias("v128.load32_splat"))]
    V128LoadSplatI32 {
        align: u32,
        offset: u32,
    },
    /// `v128.load64_splat`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads a 64-bit value from linear memory at `ptr`, and copies it into 
    /// both lanes of an `i64x2`.
    #[doc(alias("v128.load64_splat"))]
    V128LoadSplatI64 {
        align: u32,
        offset: u32,
    },
    /// `v128.load32_zero`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads a 32-bit value from linear memory at `ptr`, and puts it into the 
    /// first lane of an `i32x4`. Every other lane is zero.
    #[doc(alias("v128.load32_zero"))]
    V128LoadZeroI32 {
        align: u32,
        offset: u32,
    },
    /// `v128.load64_zero`. Signature: `(ptr: i32) -> (v128)`
    /// 
    /// Loads a 64-bit value from linear memory at `ptr`, and puts it into the 
    /// first lane of an `i64x2`. The other lane is zero.
    #[doc(alias("v128.load64_zero"))]
    V128LoadZeroI64 {
        align: u32,
        offset: u32,
    },
    /// `v128.store`. Signature: `(vec: v128, ptr: i32) -> ()`
    /// 
    /// Stores `vec` into linear memoery at `ptr`.
    #[doc(alias("v128.store"))]
    V128Store {
        align: u32,
        offset: u32,
    },
    /// `v128.load8_lane`. Signature: `(vec: v128, ptr: i32) -> (v128)`
    /// 
    /// Loads an 8-bit value from linear memory at `ptr`, and puts it into the
    /// specified lane of `vec`.
    #[doc(alias("v128.load8_lane"))]
    V128Load8Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    /// `v128.load16_lane`. Signature: `(vec: v128, ptr: i32) -> (v128)`
    /// 
    /// Loads an 16-bit value from linear memory at `ptr`, and puts it into the
    /// specified lane of `vec`.
    #[doc(alias("v128.load16_lane"))]
    V128Load16Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    /// `v128.load32_lane`. Signature: `(vec: v128, ptr: i32) -> (v128)`
    /// 
    /// Loads an 32-bit value from linear memory at `ptr`, and puts it into the
    /// specified lane of `vec`.
    #[doc(alias("v128.load32_lane"))]
    V128Load32Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    /// `v128.load64_lane`. Signature: `(vec: v128, ptr: i32) -> (v128)`
    /// 
    /// Loads an 64-bit value from linear memory at `ptr`, and puts it into the
    /// specified lane of `vec`.
    #[doc(alias("v128.load64_lane"))]
    V128Load64Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    /// `v128.store8_lane`. Signature: `(vec: v128, ptr: i32) -> ()`
    /// 
    /// Stores the 8-bit in the specified lane of `vec` into linear memory at `ptr`
    #[doc(alias("v128.store8_lane"))]
    V128Store8Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    /// `v128.store16_lane`. Signature: `(vec: v128, ptr: i32) -> ()`
    /// 
    /// Stores the 16-bit in the specified lane of `vec` into linear memory at `ptr`
    #[doc(alias("v128.store16_lane"))]
    V128Store16Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    /// `v128.store32_lane`. Signature: `(vec: v128, ptr: i32) -> ()`
    /// 
    /// Stores the 32-bit in the specified lane of `vec` into linear memory at `ptr`
    #[doc(alias("v128.store32_lane"))]
    V128Store32Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    /// `v128.store64_lane`. Signature: `(vec: v128, ptr: i32) -> ()`
    /// 
    /// Stores the 64-bit in the specified lane of `vec` into linear memory at `ptr`
    #[doc(alias("v128.store64_lane"))]
    V128Store64Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },

    /// `v128.const`. Signature: `() -> (v128)`
    /// 
    /// Returns the immediate little-endian bytes as a `v128`
    #[doc(alias("v128.const"))]
    V128Const([u8; 16]),

    /// `i8x16.shuffle`. Signature: `(vec: v128) -> (v128)`
    /// 
    /// Replaces `vec[i]` with `vec[lanes[i]]`.
    #[doc(alias("i8x16.shuffle"))]
    I8x16Shuffle {
        lanes: [u8; 16],
    },
    /// `i8x16.extract_lane_s`. Signature: `(vec: v128) -> (i32)`
    /// 
    /// Returns the value in the given lane of `vec`, sign extended to 32 bits.
    #[doc(alias("i8x16.extract_lane_s"))]
    S8x16ExtractLane {
        lane: u8,
    },
    /// `i8x16.extract_lane_u`. Signature: `(vec: v128) -> (i32)`
    /// 
    /// Returns the value in the given lane of `vec`, extended to 32 bits.
    #[doc(alias("i8x16.extract_lane_u"))]
    U8x16ExtractLane {
        lane: u8,
    },
    /// `i8x16.replace_lane`. Signature: `(x: i32, vec: v128) -> (v128)`
    /// 
    /// Replaces the value in the given lane of `vec` with the lower 8 bits of `x`
    #[doc(alias("i8x16.replace_lane"))]
    I8x16ReplaceLane {
        lane: u8,
    },
    /// `i16x8.extract_lane_s`. Signature: `(vec: v128) -> (i32)`
    /// 
    /// Returns the value in the given lane, sign extended to 32 bits.
    #[doc(alias("i16x8.extract_lane_s"))]
    S16x8ExtractLane {
        lane: u8,
    },
    /// `i16x8.extract_lane_u`. Signature: `(vec: v128) -> (i32)`
    /// 
    /// Returns the value in the given lane, extended to 32 bits.
    #[doc(alias("i16x8.extract_lane_u"))]
    U16x8ExtractLane {
        lane: u8,
    },
    /// `i16x8.replace_lane`. Signature: `(x: i32, vec: v128) -> (v128)`
    /// 
    /// Replaces the value in the given lane of `vec` with the lower 16 bits of `x`
    #[doc(alias("i16x8.replace_lane"))]
    I16x8ReplaceLane {
        lane: u8,
    },
    /// `i32x4.extract_lane`. Signature: `(vec: v128) -> (i32)`
    /// 
    /// Returns the value in the given lane.
    #[doc(alias("i32x4.extract_lane"))]
    I32x4ExtractLane {
        lane: u8,
    },
    /// `i32x4.replace_lane`. Signature: `(x: i32, vec: v128) -> (v128)`
    /// 
    /// Replaces the value in the given lane of `vec` with `x`
    #[doc(alias("i32x4.replace_lane"))]
    I32x4ReplaceLane {
        lane: u8,
    },
    /// `i64x2.extract_lane`. Signature: `(vec: v128) -> (i64)`
    /// 
    /// Returns the value in the given lane.
    #[doc(alias("i64x2.extract_lane"))]
    I64x2ExtractLane {
        lane: u8,
    },
    /// `i64x2.replace_lane`. Signature: `(x: i64, vec: v128) -> (v128)`
    /// 
    /// Replaces the value in the given lane of `vec` with `x`
    #[doc(alias("i64x2.replace_lane"))]
    I64x2ReplaceLane {
        lane: u8,
    },
    /// `f32x4.extract_lane`. Signature: `(vec: v128) -> (f32)`
    /// 
    /// Returns the value in the given lane.
    #[doc(alias("f32x4.extract_lane"))]
    F32x4ExtractLane {
        lane: u8,
    },
    /// `f32x4.replace_lane`. Signature: `(x: f32, vec: v128) -> (v128)`
    /// 
    /// Replaces the value in the given lane of `vec` with `x`
    #[doc(alias("f32x4.replace_lane"))]
    F32x4ReplaceLane {
        lane: u8,
    },
    /// `f64x2.extract_lane`. Signature: `(vec: v128) -> (f64)`
    /// 
    /// Returns the value in the given lane.
    #[doc(alias("f64x2.extract_lane"))]
    F64x2ExtractLane {
        lane: u8,
    },
    /// `f64x2.replace_lane`. Signature: `(x: f64, vec: v128) -> (v128)`
    /// 
    /// Replaces the value in the given lane of `vec` with `x`
    #[doc(alias("f64x2.replace_lane"))]
    F64x2ReplaceLane {
        lane: u8,
    },
    /// `i8x16.swizzle`. Signature: `(a: v128, b: v128) -> (v128)`
    /// 
    /// Replaces `a[i]` with `a[b[i]]`, then returns `a`. If `b[i]` is > 16, it
    /// yields 0.
    #[doc(alias("i8x16.swizzle"))]
    I8x16Swizzle,
    /// `i8x16.splat`. Signature: `(x: i32) -> (v128)`
    /// 
    /// Creates a new `v128` with every lane set to the lower 8 bits of `x`.
    #[doc(alias("i8x16.splat"))]
    I8x16Splat,
    /// `i16x8.splat`. Signature: `(x: i32) -> (v128)`
    /// 
    /// Creates a new `v128` with every lane set to the lower 16 bits of `x`.
    #[doc(alias("i16x8.splat"))]
    I16x8Splat,
    /// `i32x4.splat`. Signature: `(x: i32) -> (v128)`
    /// 
    /// Creates a new `v128` with every lane set to `x`.
    #[doc(alias("i32x4.splat"))]
    I32x4Splat,
    /// `i64x2.splat`. Signature: `(x: i64) -> (v128)`
    /// 
    /// Creates a new `v128` with every lane set to `x`.
    #[doc(alias("i64x2.splat"))]
    I64x2Splat,
    /// `f32x4.splat`. Signature: `(x: f32) -> (v128)`
    /// 
    /// Creates a new `v128` with every lane set to `x`.
    #[doc(alias("i32x4.splat"))]
    F32x4Splat,
    /// `f64x2.splat`. Signature: `(x: f64) -> (v128)`
    /// 
    /// Creates a new `v128` with every lane set to`x`.
    #[doc(alias("i64x2.splat"))]
    F64x2Splat,

    /// `i8x16.eq`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] == b[i]` and puts the results into the lanes of a new `v128`
    #[doc(alias("i8x16.eq"))]
    I8x16Equal,
    /// `i8x16.ne`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] != b[i]` and puts the results into the lanes of a new `v128`
    #[doc(alias("i8x16.ne"))]
    I8x16NotEqual,
    /// `i8x16.lt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i8 < b[i] as i8` and puts the results into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.lt"))]
    S8x16LessThan,
    /// `i8x16.lt_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u8 < b[i] as u8` and puts the results into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.eq"))]
    U8x16LessThan,
    /// `i8x16.gt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i8 > b[i] as i8` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.gt_s"))]
    S8x16GreaterThan,
    /// `i8x16.gt_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u8 > b[i] as u8` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.gt_u"))]
    U8x16GreaterThan,
    /// `i8x16.le_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i8 <= b[i] as i8` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.le_s"))]
    S8x16LessThanOrEqual,
    /// `i8x16.le_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u8 <= b[i] as u8` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.le_u"))]
    U8x16LessThanOrEqual,
    /// `i8x16.ge_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i8 >= b[i] as i8` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.ge_s"))]
    S8x16GreaterThanOrEqual,
    /// `i8x16.ge_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u8 >= b[i] as u8` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i8x16.ge_u"))]
    U8x16GreaterThanOrEqual,

    /// `i16x8.eq`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] == b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i16x8.eq"))]
    I16x8Equal,
    /// `i16x8.ne`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] != b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i16x8.ne"))]
    I16x8NotEqual,
    /// `i16x8.lt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i16 < b[i] as i16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.lt_s"))]
    S16x8LessThan,
    /// `i16x8.lt_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u16 < b[i] as u16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.lt_u"))]
    U16x8LessThan,
    /// `i16x8.gt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i16 > b[i] as i16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.gt_s"))]
    S16x8GreaterThan,
    /// `i16x8.gt_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u16 > b[i] as u16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.gt_u"))]
    U16x8GreaterThan,
    /// `i16x8.le_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i16 <= b[i] as i16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.le_s"))]
    S16x8LessThanOrEqual,
    /// `i16x8.le_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u16 <= b[i] as u16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.le_u"))]
    U16x8LessThanOrEqual,
    /// `i16x8.ge_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i16 >= b[i] as i16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.ge_s"))]
    S16x8GreaterThanOrEqual,
    /// `i16x8.ge_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u16 >= b[i] as u16` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i16x8.ge_u"))]
    U16x8GreaterThanOrEqual,

    /// `i32x4.eq`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] == b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i32x4.eq"))]
    I32x4Equal,
    /// `i32x4.ne`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] != b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i32x4.ne"))]
    I32x4NotEqual,
    /// `i32x4.lt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i32 < b[i] as i32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.lt_s"))]
    S32x4LessThan,
    /// `i32x4.lt_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u32 < b[i] as u32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.lt_u"))]
    U32x4LessThan,
    /// `i32x4.gt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i32 > b[i] as i32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.gt_s"))]
    S32x4GreaterThan,
    /// `i32x4.gt_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u32 > b[i] as u32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.gt_u"))]
    U32x4GreaterThan,
    /// `i32x4.le_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i32 <= b[i] as i32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.le_s"))]
    S32x4LessThanOrEqual,
    /// `i32x4.le_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u32 <= b[i] as u32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.le_u"))]
    U32x4LessThanOrEqual,
    /// `i32x4.ge_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i32 >= b[i] as i32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.ge_s"))]
    S32x4GreaterThanOrEqual,
    /// `i32x4.ge_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as u32 >= b[i] as u32` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i32x4.ge_u"))]
    U32x4GreaterThanOrEqual,

    /// `i64x2.eq`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] == b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i64x2.eq"))]
    I64x2Equal,
    /// `i64x2.ne`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] != b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i64x2.ne"))]
    I64x2NotEqual,
    /// `i64x2.lt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i64 < b[i] as i64` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i64x2.lt_s"))]
    S64x2LessThan,
    /// `i64x2.gt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i64 > b[i] as i64` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i64x2.gt_s"))]
    S64x2GreaterThan,
    /// `i64x2.le_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i64 <= b[i] as i64` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i64x2.le_s"))]
    S64x2LessThanOrEqual,
    /// `i64x2.ge_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] as i64 >= b[i] as i64` and puts the result into the lanes 
    /// of a new `v128`
    #[doc(alias("i64x2.ge_s"))]
    S64x2GreaterThanOrEqual,

    /// `f32x4.eq`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] == b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("f32x4.eq"))]
    F32x4Equal,
    /// `f32x4.ne`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] != b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("f32x4.ne"))]
    F32x4NotEqual,
    /// `f32x4.lt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] < b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i3fx4.lt_s"))]
    F32x4LessThan,
    /// `f32x4.gt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] > b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i3fx4.gt_s"))]
    F32x4GreaterThan,
    /// `f32x4.le_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] <= b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i3fx4.le_s"))]
    F32x4LessThanOrEqual,
    /// `f32x4.ge_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] >= b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i3fx4.ge_s"))]
    F32x4GreaterThanOrEqual,

    /// `f64x2.eq`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] == b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("f64x2.eq"))]
    F64x2Equal,
    /// `f64x2.ne`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] != b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("f64x2.ne"))]
    F64x2NotEqual,
    /// `f64x2.lt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] < b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i64x2.lt_s"))]
    F64x2LessThan,
    /// `f64x2.gt_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] > b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i64x2.gt_s"))]
    F64x2GreaterThan,
    /// `f64x2.le_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] <= b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i64x2.le_s"))]
    F64x2LessThanOrEqual,
    /// `f64x2.ge_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] >= b[i]` and puts the result into the lanes of a new `v128`
    #[doc(alias("i64x2.ge_s"))]
    F64x2GreaterThanOrEqual,

    /// `v128.not`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes bitwise NOT of `a`
    #[doc(alias("v128.not"))]
    V128Not,
    /// `v128.and`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes bitwise AND of `a` and `b`
    #[doc(alias("v128.and"))]
    V128And,
    /// `v128.andnot`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes bitwise NAND of `a` and `b`
    #[doc(alias("v128.andnot"))]
    V128AndNot,
    /// `v128.or`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes bitwise OR of `a` and `b`
    #[doc(alias("v128.or"))]
    V128Or,
    /// `v128.xor`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes bitwise XOR of `a` and `b`
    #[doc(alias("v128.xor"))]
    V128Xor,
    /// `v128.bitselect`. Signature: `(c: v128, b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a ^ c) | (b ^ !c)`.
    /// 
    /// This has the effect of selecting between the bits of `a` and `b` based
    /// on the bits of `c`, hence "bitselect".
    #[doc(alias("v128.bitselect"))]
    V128BitSelect,
    /// `v128.`. Signature: `(a: v128) -> (i32)`
    /// 
    /// Checks if any bit of `a` is 1, returning 1 if so, and 0 if not.
    #[doc(alias("v128.any_true"))]
    V128AnyTrue,

    /// `i8x16.abs`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a[i].abs()`.
    #[doc(alias("i8x16.abs"))]
    I8x16Abs,
    /// `i8x16.neg`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `-a[i]`.
    #[doc(alias("i8x16.neg"))]
    I8x16Neg,
    /// `i8x16.popcnt`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a[i].count_ones()`.
    #[doc(alias("i8x16.popcnt"))]
    I8x16CountOnes,
    /// `i8x16.abs`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Checks that all lanes are non-zero. If they are, returns 1, if not,
    /// returns 0.
    #[doc(alias("i8x16.all_true"))]
    I8x16AllTrue,
    /// `i8x16.bitmask`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Returns an integer with each bit set if the corresponding lane of `a` is non-zero.
    #[doc(alias("i8x16.bitmask"))]
    I8x16Bitmask,
    /// `i8x16.narrow_i16x8_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Shortens each 16-bit lane of `a` and `b` to a signed 8-bit integer, 
    /// saturating, then returns a new `v128` with the lower 8 lanes being the 
    /// shortened `a`, and the upper 8 lanes being the shortened `b`
    #[doc(alias("i8x16.narrow_i16x8_s"))]
    S8x16NarrowI16x8,
    /// `i8x16.narrow_i16x8_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Shortens each 16-bit lane of `a` and `b` to an unsigned 8-bit integer, 
    /// saturating, then returns a new `v128` with the lower 8 lanes being the 
    /// shortened `a`, and the upper 8 lanes being the shortened `b`
    #[doc(alias("i8x16.narrow_i16x8_u"))]
    U8x16NarrowI16x8,
    /// `i8x16.shl`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` left by `x` bits.
    #[doc(alias("i8x16.shl"))]
    I8x16Shl,
    /// `i8x16.shr_s`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` right by `x` bits, signed.
    #[doc(alias("i8x16.shr_s"))]
    S8x16Shr,
    /// `i8x16.shr_u`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` right by `x` bits, unsigned.
    #[doc(alias("i8x16.shr_u"))]
    U8x16Shr,
    /// `i8x16.add`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`.
    #[doc(alias("i8x16.add"))]
    I8x16Add,
    /// `i8x16.add_sat_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`, saturating at the signed max value.
    #[doc(alias("i8x16.add_sat_s"))]
    S8x16AddSaturate,
    /// `i8x16.add_sat_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`, saturating at the unsigned max value.
    #[doc(alias("i8x16.add_sat_u"))]
    U8x16AddSaturate,
    /// `i8x16.sub`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`.
    #[doc(alias("i8x16.sub"))]
    I8x16Sub,
    /// `i8x16.sub_sat_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`, saturating at the signed minimum value.
    #[doc(alias("i8x16.sub_sat_s"))]
    S8x16SubSaturate,
    /// `i8x16.sub_sat_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`, saturating at zero.
    #[doc(alias("i8x16.sub_sat_u"))]
    U8x16SubSaturate,
    /// `i8x16.min_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as i8).min(b[i] as i8)`.
    #[doc(alias("i8x16.min_s"))]
    S8x16Min,
    /// `i8x16.min_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as u8).min(b[i] as u8)`.
    #[doc(alias("i8x16.min_u"))]
    U8x16Min,
    /// `i8x16.max_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as i8).max(b[i] as i8)`.
    #[doc(alias("i8x16.max_s"))]
    S8x16Max,
    /// `i8x16.max_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as u8).max(b[i] as u8)`.
    #[doc(alias("i8x16.max_u"))]
    U8x16Max,
    /// `i8x16.avgr_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes the average of `a[i]` and `b[i]`, rounding towards zero.
    #[doc(alias("i8x16.avgr_u"))]
    U8x16Avgr,

    /// `i16x8.extadd_pairwise_i8x16_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends each lane to 16 bits, then adds each pair.
    #[doc(alias("i16x8.extadd_pairwise_i8x16_s"))]
    I16x8ExtendAddPairwiseS8x16,
    /// `i16x8.extadd_pairwise_i8x16_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends each lane to 16 bits, then adds each pair.
    #[doc(alias("i16x8.extadd_pairwise_i8x16_u"))]
    I16x8ExtendAddPairwiseU8x16,
    /// `i16x8.abs`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a[i].abs()`
    #[doc(alias("i16x8.abs"))]
    I16x8Abs,
    /// `i16x8.neg`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `-a[i]`
    #[doc(alias("i16x8.neg"))]
    I16x8Neg,
    /// `i16x8.q15mulr_sat_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(((a[i] as i32 * b[i] as i32) + (2.pow(14))) >> 15) as i16`, saturating.
    #[doc(alias("i16x8.q15mulr_sat_s"))]
    S16x8Q15MulRSat,
    /// `i16x8.all_true`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Checks that all lanes are non-zero. If they are, returns 1, if not,
    /// returns 0.
    #[doc(alias("i16x8.all_true"))]
    I16x8AllTrue,
    /// `i16x8.bitmask`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Returns an integer with each bit set if the corresponding lane of `a` is non-zero.
    #[doc(alias("i16x8.bitmask"))]
    I16x8Bitmask,
    /// `i16x8.narrow_i32x4_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Shortens each 32-bit lane of `a` and `b` to a signed 32-bit integer, 
    /// saturating, then returns a new `v128` with the lower 4 lanes being the 
    /// shortened `a`, and the upper 4 lanes being the shortened `b`
    #[doc(alias("i16x8.narrow_i32x4_s"))]
    S16x8NarrowI32x4,
    /// `i16x8.narrow_i32x4_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Shortens each 32-bit lane of `a` and `b` to an unsigned 32-bit integer, 
    /// saturating, then returns a new `v128` with the lower 4 lanes being the 
    /// shortened `a`, and the upper 4 lanes being the shortened `b`
    #[doc(alias("i16x8.narrow_i32x4_u"))]
    U16x8NarrowI32x4,
    /// `i16x8.extend_low_i8x16_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends the lower 8 lanes of `a` to 16 bits
    #[doc(alias("i16x8.extend_low_i8x16_s"))]
    I16x8ExtendLowS8x16,
    /// `i16x8.extend_high_i8x16_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends the upper 8 lanes of `a` to 16 bits
    #[doc(alias("i16x8.extend_high_i8x16_s"))]
    I16x8ExtendHighS8x16,
    /// `i16x8.extend_low_i8x16_u`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends the lower 8 lanes of `a` to 16 bits
    #[doc(alias("i16x8.extend_low_i8x16_u"))]
    I16x8ExtendLowU8x16,
    /// `i16x8.extend_high_i8x16_u`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends the upper 8 lanes of `a` to 16 bits
    #[doc(alias("i16x8.extend_high_i8x16_u"))]
    I16x8ExtendHighU8x16,
    /// `i16x8.shl`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` left by `x` bits.
    #[doc(alias("i16x8.shl"))]
    I16x8Shl,
    /// `i16x8.shr_s`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` right by `x` bits, signed.
    #[doc(alias("i16x8.shr_s"))]
    S16x8Shr,
    /// `i16x8.shr_u`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` right by `x` bits, unsigned.
    #[doc(alias("i16x8.shr_u"))]
    U16x8Shr,
    /// `i16x8.add`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`.
    #[doc(alias("i16x8.add"))]
    I16x8Add,
    /// `i16x8.add_sat_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`, saturating at the signed max value.
    #[doc(alias("i16x8.add_sat_s"))]
    S16x8AddSaturate,
    /// `i16x8.add_sat_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`, saturating at the unsigned max value.
    #[doc(alias("i16x8.add_sat_u"))]
    U16x8AddSaturate,
    /// `i16x8.sub`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`.
    #[doc(alias("i16x8.sub"))]
    I16x8Sub,
    /// `i16x8.sub_sat_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`, saturating at the signed minimum value.
    #[doc(alias("i16x8.sub_sat_s"))]
    S16x8SubSaturate,
    /// `i16x8.sub_sat_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`, saturating at zero.
    #[doc(alias("i16x8.sub_sat_u"))]
    U16x8SubSaturate,
    /// `i16x8.mul`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] * b[i]`.
    #[doc(alias("i16x8.mul"))]
    I16x8Mul,
    /// `i16x8.min_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as i16).min(b[i] as i16)`.
    #[doc(alias("i16x8.min_s"))]
    S16x8Min,
    /// `i16x8.min_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as u16).min(b[i] as u16)`.
    #[doc(alias("i16x8.min_u"))]
    U16x8Min,
    /// `i16x8.max_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as i16).max(b[i] as i16)`.
    #[doc(alias("i16x8.max_s"))]
    S16x8Max,
    /// `i16x8.max_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as u16).max(b[i] as u16)`.
    #[doc(alias("i16x8.max_u"))]
    U16x8Max,
    /// `i16x8.avgr_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes the average of `a[i]` and `b[i]`, rounding towards zero.
    #[doc(alias("i16x8.avgr_u"))]
    U16x8Avgr,
    /// `i16x8.extmul_low_i8x16_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Sign extends the lower 8 lanes of `a` and `b` to 16 bits, then 
    /// multiplies them together.
    #[doc(alias("i16x8.extmul_low_i8x16_s"))]
    I16x8ExtMulLowS8x16,
    /// `i16x8.extmul_low_i8x16_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Sign extends the higher 8 lanes of `a` and `b` to 16 bits, then 
    /// multiplies them together.
    #[doc(alias("i16x8.extmul_high_i8x16_s"))]
    I16x8ExtMulHighS8x16,
    /// `i16x8.extmul_low_i8x16_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Extends the lower 8 lanes of `a` and `b` to 16 bits, then multiplies 
    /// them together.
    #[doc(alias("i16x8.extmul_low_i8x16_u"))]
    I16x8ExtMulLowU8x16,
    /// `i16x8.extmul_low_i8x16_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Extends the higher 8 lanes of `a` and `b` to 16 bits, then multiplies 
    /// them together.
    #[doc(alias("i16x8.extmul_high_i8x16_u"))]
    I16x8ExtMulHighU8x16,

    /// `i32x4.extadd_pairwise_i8x16_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends each lane to 16 bits, then adds each pair.
    #[doc(alias("i32x4.extadd_pairwise_i8x16_s"))]
    I32x4ExtendAddPairwiseS16x8,
    /// `i32x4.extadd_pairwise_i8x16_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends each lane to 16 bits, then adds each pair.
    #[doc(alias("i32x4.extadd_pairwise_i8x16_u"))]
    I32x4ExtendAddPairwiseU16x8,
    /// `i32x4.abs`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a[i].abs()`
    #[doc(alias("i32x4.abs"))]
    I32x4Abs,
    /// `i32x4.neg`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `-a[i]`
    #[doc(alias("i32x4.neg"))]
    I32x4Neg,
    /// `i32x4.all_true`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Checks that all lanes are non-zero. If they are, returns 1, if not,
    /// returns 0.
    #[doc(alias("i32x4.all_true"))]
    I32x4AllTrue,
    /// `i32x4.bitmask`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Returns an integer with each bit set if the corresponding lane of `a` is non-zero.
    #[doc(alias("i32x4.bitmask"))]
    I32x4Bitmask,
    /// `i32x4.extend_low_i16x8_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends the lower 4 lanes of `a` to 32 bits
    #[doc(alias("i32x4.extend_low_i16x8_s"))]
    I32x4ExtendLowS16x8,
    /// `i32x4.extend_high_i16x8_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends the upper 4 lanes of `a` to 32 bits
    #[doc(alias("i32x4.extend_high_i16x8_s"))]
    I32x4ExtendHighS16x8,
    /// `i32x4.extend_low_i16x8_u`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends the lower 4 lanes of `a` to 32 bits
    #[doc(alias("i32x4.extend_low_i16x8_u"))]
    I32x4ExtendLowU16x8,
    /// `i32x4.extend_high_i16x8_u`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends the upper 4 lanes of `a` to 32 bits
    #[doc(alias("i32x4.extend_high_i16x8_u"))]
    I32x4ExtendHighU16x8,
    /// `i32x4.shl`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` left by `x` bits.
    #[doc(alias("i32x4.shl"))]
    I32x4Shl,
    /// `i32x4.shr_s`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` right by `x` bits, signed.
    #[doc(alias("i32x4.shr_s"))]
    S32x4Shr,
    /// `i32x4.shr_u`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts every lane of `v` right by `x` bits, unsigned.
    #[doc(alias("i32x4.shr_u"))]
    U32x4Shr,
    /// `i32x4.add`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`.
    #[doc(alias("i32x4.add"))]
    I32x4Add,
    /// `i32x4.sub`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`.
    #[doc(alias("i32x4.sub"))]
    I32x4Sub,
    /// `i32x4.mul`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] * b[i]`.
    #[doc(alias("i32x4.mul"))]
    I32x4Mul,
    /// `i32x4.min_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as i32).min(b[i] as i32)`.
    #[doc(alias("i32x4.min_s"))]
    S32x4Min,
    /// `i32x4.min_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as u32).min(b[i] as u32)`.
    #[doc(alias("i32x4.min_u"))]
    U32x4Min,
    /// `i32x4.max_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as i32).max(b[i] as i32)`.
    #[doc(alias("i32x4.max_s"))]
    S32x4Max,
    /// `i32x4.max_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `(a[i] as u32).max(b[i] as u32)`.
    #[doc(alias("i32x4.max_u"))]
    U32x4Max,
    /// `i32x4.dot_i16x8`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Pretends `a` and `b` are each arrays of four 2D vectors, and computes
    /// the dot product of each pair, sign extending the result.
    #[doc(alias("i32x4.dot_i16x8"))]
    I32x4DotProductS16x8,
    /// `i32x4.extmul_low_i16x8_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Sign extends the lower 4 lanes of `a` and `b` to 32 bits, then 
    /// multiplies them together.
    #[doc(alias("i32x4.extmul_low_i16x8_s"))]
    I32x4ExtMulLowS16x8,
    /// `i32x4.extmul_low_i16x8_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Sign extends the higher 4 lanes of `a` and `b` to 32 bits, then 
    /// multiplies them together.
    #[doc(alias("i32x4.extmul_high_i16x8_s"))]
    I32x4ExtMulHighS16x8,
    /// `i32x4.extmul_low_i16x8_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Extends the lower 4 lanes of `a` and `b` to 32 bits, then multiplies 
    /// them together.
    #[doc(alias("i32x4.extmul_low_i16x8_u"))]
    I32x4ExtMulLowU16x8,
    /// `i32x4.extmul_low_i16x8_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Extends the higher 4 lanes of `a` and `b` to 32 bits, then multiplies 
    /// them together.
    #[doc(alias("i32x4.extmul_high_i16x8_u"))]
    I32x4ExtMulHighU16x8,

    /// `i64x2.abs`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a[i].abs()`
    #[doc(alias("i64x2.abs"))]
    I64x2Abs,
    /// `i64x2.neg`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `-a[i]`
    #[doc(alias("i64x2.neg"))]
    I64x2Neg,
    /// `i64x2.all_true`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Checks that both lanes are non-zero. If they are, returns 1, if not,
    /// returns 0.
    #[doc(alias("i64x2.all_true"))]
    I64x2AllTrue,
    /// `i64x2.bitmask`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Returns an integer with each bit set if the corresponding lane of `a` is non-zero.
    #[doc(alias("i64x2.bitmask"))]
    I64x2Bitmask,
    /// `i64x2.extend_low_i16x8_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends the lower 2 lanes of `a` to 64 bits
    #[doc(alias("i64x2.extend_low_i32x4_s"))]
    I64x2ExtendLowS32x4,
    /// `i64x2.extend_high_i16x8_s`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Sign extends the upper 2 lanes of `a` to 64 bits
    #[doc(alias("i64x2.extend_high_i32x4_s"))]
    I64x2ExtendHighS32x4,
    /// `i64x2.extend_low_i16x8_u`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends the lower 2 lanes of `a` to 64 bits
    #[doc(alias("i64x2.extend_low_i32x4_u"))]
    I64x2ExtendLowU32x4,
    /// `i64x2.extend_high_i16x8_u`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Extends the upper 2 lanes of `a` to 64 bits
    #[doc(alias("i64x2.extend_high_i32x4_u"))]
    I64x2ExtendHighU32x4,
    /// `i64x2.shl`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts both lanes of `v` left by `x` bits.
    #[doc(alias("i64x2.shl"))]
    I64x2Shl,
    /// `i64x2.shr_s`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts both lanes of `v` right by `x` bits, signed.
    #[doc(alias("i64x2.shr_s"))]
    S64x2Shr,
    /// `i64x2.shr_u`. Signature: `(x: i32, v: v128) -> (v128)`
    /// 
    /// Shifts both lanes of `v` right by `x` bits, unsigned.
    #[doc(alias("i64x2.shr_u"))]
    U64x2Shr,
    /// `i64x2.add`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`.
    #[doc(alias("i64x2.add"))]
    I64x2Add,
    /// `i64x2.sub`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`.
    #[doc(alias("i64x2.sub"))]
    I64x2Sub,
    /// `i64x2.mul`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] * b[i]`.
    #[doc(alias("i64x2.mul"))]
    I64x2Mul,
    /// `i64x2.extmul_low_i16x8_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Sign extends the lower 2 lanes of `a` and `b` to 64 bits, then 
    /// multiplies them together.
    #[doc(alias("i64x2.extmul_low_i32x4_s"))]
    I64x2ExtMulLowS32x4,
    /// `i64x2.extmul_low_i16x8_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Sign extends the higher 2 lanes of `a` and `b` to 64 bits, then 
    /// multiplies them together.
    #[doc(alias("i64x2.extmul_high_i32x4_s"))]
    I64x2ExtMulHighS32x4,
    /// `i64x2.extmul_low_i16x8_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Extends the lower 2 lanes of `a` and `b` to 64 bits, then multiplies 
    /// them together.
    #[doc(alias("i64x2.extmul_low_i32x4_u"))]
    I64x2ExtMulLowU32x4,
    /// `i64x2.extmul_low_i16x8_u`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Extends the higher 2 lanes of `a` and `b` to 64 bits, then multiplies 
    /// them together.
    #[doc(alias("i64x2.extmul_high_i32x4_u"))]
    I64x2ExtMulHighU32x4,

    /// `f32x4.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` up, towards infinity.
    #[doc(alias("f32x4.ceil"))]
    F32x4Ceil,
    /// `f32x4.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` down, towards negative infinity.
    #[doc(alias("f32x4.ceil"))]
    F32x4Floor,
    /// `f32x4.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` towards zero.
    #[doc(alias("f32x4.ceil"))]
    F32x4Trunc,
    /// `f32x4.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` towards the nearest integer.
    #[doc(alias("f32x4.ceil"))]
    F32x4Nearest,
    /// `f32x4.abs`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a[i].abs()`
    #[doc(alias("f32x4.abs"))]
    F32x4Abs,
    /// `f32x4.neg`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `-a[i]`
    #[doc(alias("f32x4.neg"))]
    F32x4Neg,
    /// `f32x4.sqrt`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a.sqrt()`
    #[doc(alias("f32x4.sqrt"))]
    F32x4Sqrt,
    /// `f32x4.add`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`.
    #[doc(alias("f32x4.add"))]
    F32x4Add,
    /// `f32x4.sub`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`.
    #[doc(alias("f32x4.sub"))]
    F32x4Sub,
    /// `f32x4.mul`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] * b[i]`.
    #[doc(alias("f32x4.mul"))]
    F32x4Mul,
    /// `f32x4.div`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] / b[i]`.
    #[doc(alias("f32x4.div"))]
    F32x4Div,
    /// `i32x4.min`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i].min(b[i])`.
    #[doc(alias("i32x4.min"))]
    F32x4Min,
    /// `i32x4.max`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i].max(b[i])`.
    #[doc(alias("i32x4.max"))]
    F32x4Max,
    /// `i32x4.min_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `if a[i] < b[i] { a[i] } else { b[i] }`.
    #[doc(alias("i32x4.min_s"))]
    F32x4PMin,
    /// `i32x4.max_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `if a[i] < b[i] { b[i] } else { a[i] }`.
    #[doc(alias("i32x4.max_s"))]
    F32x4PMax,

    /// `f64x2.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` up, towards infinity.
    #[doc(alias("f6424.ceil"))]
    F64x2Ceil,
    /// `f64x2.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` down, towards negative infinity.
    #[doc(alias("f6424.ceil"))]
    F64x2Floor,
    /// `f64x2.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` towards zero.
    #[doc(alias("f6424.ceil"))]
    F64x2Trunc,
    /// `f64x2.ceil`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Rounds each lane of `a` towards the nearest integer.
    #[doc(alias("f6424.ceil"))]
    F64x2Nearest,
    /// `f64x2.abs`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a[i].abs()`
    #[doc(alias("f6424.abs"))]
    F64x2Abs,
    /// `f64x2.neg`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `-a[i]`
    #[doc(alias("f6424.neg"))]
    F64x2Neg,
    /// `f64x2.sqrt`. Signature: `(a: v128) -> (v128)`
    /// 
    /// Computes `a.sqrt()`
    #[doc(alias("f6424.sqrt"))]
    F64x2Sqrt,
    /// `f64x2.add`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] + b[i]`.
    #[doc(alias("f6424.add"))]
    F64x2Add,
    /// `f64x2.sub`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] - b[i]`.
    #[doc(alias("f6424.sub"))]
    F64x2Sub,
    /// `f64x2.mul`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] * b[i]`.
    #[doc(alias("f6424.mul"))]
    F64x2Mul,
    /// `f64x2.div`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i] / b[i]`.
    #[doc(alias("f6424.div"))]
    F64x2Div,
    /// `i64x2.min`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i].min(b[i])`.
    #[doc(alias("i6424.min"))]
    F64x2Min,
    /// `i64x2.max`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `a[i].max(b[i])`.
    #[doc(alias("i6424.max"))]
    F64x2Max,
    /// `i64x2.min_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `if a[i] < b[i] { a[i] } else { b[i] }`.
    #[doc(alias("i6424.min_s"))]
    F64x2PMin,
    /// `i64x2.max_s`. Signature: `(b: v128, a: v128) -> (v128)`
    /// 
    /// Computes `if a[i] < b[i] { b[i] } else { a[i] }`.
    #[doc(alias("i6424.max_s"))]
    F64x2PMax,
    
    /// `i32x4.trunc_sat_f32x4_s`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Computes `a[i]` rounded towards zero, as a signed integer, saturating to `i32::MAX`
    #[doc(alias("i32x4.trunc_sat_f32x4_s"))]
    S32x4TruncSatF32x4,
    /// `i32x4.trunc_sat_f32x4_u`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a[i]` rounded towards zero, as an unsigned integer, saturating 
    /// to `u32::MAX`
    #[doc(alias("i32x4.trunc_sat_f32x4_u"))]
    U32x4TruncSatF32x4,
    /// `f32x4.convert_i32x4_s`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Computes `a[i] as i32 as f32`.
    #[doc(alias("f32x4.convert_i32x4_s"))]
    F32x4ConvertS32x4,
    /// `f32x4.convert_i32x4_u`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Computes `a[i] as u32 as f32`.
    #[doc(alias("f32x4.convert_i32x4_u"))]
    F32x4ConvertU32x4,
    /// `i32x4.trunc_sat_f64x2_s_zero`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Computes `a[i]` rounded towards zero, as a signed integer, saturating to 
    /// `i32::MAX`. The upper two lanes are zero.
    #[doc(alias("i32x4.trunc_sat_f64x2_s_zero"))]
    S32x4TruncSatZeroF64x2,
    /// `i32x4.trunc_sat_f64x2_u_zero`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Returns `a[i]` rounded towards zero, as an unsigned integer, saturating 
    /// to `u32::MAX`. The upper two lanes are zero.
    #[doc(alias("i32x4.trunc_sat_f64x2_u_zero"))]
    U32x4TruncSatZeroF64x2,
    /// `f64x2.convert_low_i32x4_s`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Computes `a[i] as i32 as f64` for the lower two lanes.
    #[doc(alias("f64x2.convert_low_i32x4_s"))]
    F64x2ConvertLowS32x4,
    /// `f64x2.convert_low_i32x4_u`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Computes `a[i] as u32 as f64` for the lower two lanes.
    #[doc(alias("f64x2.convert_low_i32x4_u"))]
    F64x2ConvertLowU32x4,
    /// `f32x4.demote_f64x2_zero`. Signature: `(a: f32) -> (i32)`
    /// 
    /// Computes `a[i] as f32`. The upper two lanes are zero.
    #[doc(alias("f32x4.demote_f64x2_zero"))]
    F32x4DemoteF64x2Zero,
    /// `f64x2.promote_low_f32x4`. Signature: `(a: i32) -> (f32)`
    /// 
    /// Computes `a[i] as f64` for the lower two lanes.
    #[doc(alias("f64x2.promote_low_f32x4"))]
    F64x2PromoteLowF32x4,
}

impl Instruction {
    fn instr_id(&self) -> InstrId {
        use InstrId::*;
        use Instruction::*;
        match self {
            Unreachable => Single(0),
            NoOp => Single(1),
            Block(_) => Single(2),
            Loop(_) => Single(3),
            If(_) => Single(4),
            Else => Single(5),
            End => Single(0xB),
            Branch(_) => Single(0xC),
            BranchIf(_) => Single(0xD),
            BranchTable {
                depths: _,
                failsafe: _,
            } => Single(0xE),
            Return => Single(0xF),
            Call(_) => Single(0x10),
            CallIndirect {
                type_idx: _,
                table_idx: _,
            } => Single(0x11),
            RefNull(_) => Single(0xD0),
            RefIsNull => Single(0xD1),
            RefFunc(_) => Single(0xD2),
            Drop => Single(0x1A),
            Select => Single(0x1B),
            LocalGet(_) => Single(0x20),
            LocalSet(_) => Single(0x21),
            LocalTee(_) => Single(0x22),
            GlobalGet(_) => Single(0x23),
            GlobalSet(_) => Single(0x24),
            TableGet(_) => Single(0x25),
            TableSet(_) => Single(0x26),
            TableInit {
                table_index: _,
                elem_index: _,
            } => Extended(0xFCu8, 12u32),
            ElemDrop(_) => Extended(0xFCu8, 13u32),
            TableCopy {
                dest_index: _,
                src_index: _,
            } => Extended(0xFCu8, 14u32),
            TableGrow(_) => Extended(0xFCu8, 15u32),
            TableSize(_) => Extended(0xFCu8, 16u32),
            TableFill(_) => Extended(0xFCu8, 17u32),
            I32Load {
                align: _,
                offset: _,
            } => Single(0x28),
            I64Load {
                align: _,
                offset: _,
            } => Single(0x29),
            F32Load {
                align: _,
                offset: _,
            } => Single(0x2A),
            F64Load {
                align: _,
                offset: _,
            } => Single(0x2B),
            I32LoadS8 {
                align: _,
                offset: _,
            } => Single(0x2C),
            I32LoadU8 {
                align: _,
                offset: _,
            } => Single(0x2D),
            I32LoadS16 {
                align: _,
                offset: _,
            } => Single(0x2E),
            I32LoadU16 {
                align: _,
                offset: _,
            } => Single(0x2F),
            I64LoadS8 {
                align: _,
                offset: _,
            } => Single(0x30),
            I64LoadU8 {
                align: _,
                offset: _,
            } => Single(0x31),
            I64LoadS16 {
                align: _,
                offset: _,
            } => Single(0x32),
            I64LoadU16 {
                align: _,
                offset: _,
            } => Single(0x33),
            I64LoadS32 {
                align: _,
                offset: _,
            } => Single(0x34),
            I64LoadU32 {
                align: _,
                offset: _,
            } => Single(0x35),
            I32Store {
                align: _,
                offset: _,
            } => Single(0x36),
            I64Store {
                align: _,
                offset: _,
            } => Single(0x37),
            F32Store {
                align: _,
                offset: _,
            } => Single(0x38),
            F64Store {
                align: _,
                offset: _,
            } => Single(0x39),
            I32StoreI8 {
                align: _,
                offset: _,
            } => Single(0x3A),
            I32StoreI16 {
                align: _,
                offset: _,
            } => Single(0x3B),
            I64StoreI8 {
                align: _,
                offset: _,
            } => Single(0x3C),
            I64StoreI16 {
                align: _,
                offset: _,
            } => Single(0x3D),
            I64StoreI32 {
                align: _,
                offset: _,
            } => Single(0x3E),
            MemorySize => Single(0x3F),
            MemoryGrow => Single(0x40),
            MemoryInit(_) => Extended(0xFC, 8),
            DataDrop(_) => Extended(0x40, 9),
            MemoryCopy => Extended(0xFC, 10),
            MemoryFill => Extended(0xFC, 11),
            I32Const(_) => Single(0x41),
            I64Const(_) => Single(0x42),
            F32Const(_) => Single(0x43),
            F64Const(_) => Single(0x44),
            I32EqualsZero => Single(0x45),
            I32Equal => Single(0x46),
            I32NotEqual => Single(0x47),
            S32LessThan => Single(0x48),
            U32LessThan => Single(0x49),
            S32GreaterThan => Single(0x4A),
            U32GreaterThan => Single(0x4B),
            S32LessThanOrEqual => Single(0x4C),
            U32LessThanOrEqual => Single(0x4D),
            S32GreaterThanOrEqual => Single(0x4E),
            U32GreaterThanOrEqual => Single(0x4F),
            I64EqualsZero => Single(0x50),
            I64Equal => Single(0x51),
            I64NotEqual => Single(0x52),
            S64LessThan => Single(0x53),
            U64LessThan => Single(0x54),
            S64GreaterThan => Single(0x55),
            U64GreaterThan => Single(0x56),
            S64LessThanOrEqual => Single(0x57),
            U64LessThanOrEqual => Single(0x58),
            S64GreaterThanOrEqual => Single(0x59),
            U64GreaterThanOrEqual => Single(0x5A),
            F32Equal => Single(0x5B),
            F32NotEqual => Single(0x5C),
            F32LessThan => Single(0x5D),
            F32GreaterThan => Single(0x5E),
            F32LessThanOrEqual => Single(0x5F),
            F32GreaterThanOrEqual => Single(0x60),
            F64Equal => Single(0x61),
            F64NotEqual => Single(0x62),
            F64LessThan => Single(0x63),
            F64GreaterThan => Single(0x64),
            F64LessThanOrEqual => Single(0x65),
            F64GreaterThanOrEqual => Single(0x66),
            I32CountLeadingZeroes => Single(0x67),
            I32CountTrailingZeroes => Single(0x68),
            I32CountOnes => Single(0x69),
            I32Add => Single(0x6A),
            I32Sub => Single(0x6B),
            I32Mul => Single(0x6C),
            S32Div => Single(0x6D),
            U32Div => Single(0x6E),
            S32Rem => Single(0x6F),
            U32Rem => Single(0x70),
            I32And => Single(0x71),
            I32Or => Single(0x72),
            I32Xor => Single(0x73),
            I32ShiftLeft => Single(0x74),
            S32ShiftRight => Single(0x75),
            U32ShiftRight => Single(0x76),
            I32RotateLeft => Single(0x77),
            I32RotateRight => Single(0x78),
            I64CountLeadingZeroes => Single(0x79),
            I64CountTrailingZeroes => Single(0x7A),
            I64CountOnes => Single(0x7B),
            I64Add => Single(0x7C),
            I64Sub => Single(0x7D),
            I64Mul => Single(0x7E),
            S64Div => Single(0x7F),
            U64Div => Single(0x80),
            S64Rem => Single(0x81),
            U64Rem => Single(0x82),
            I64And => Single(0x83),
            I64Or => Single(0x84),
            I64Xor => Single(0x85),
            I64ShiftLeft => Single(0x86),
            S64ShiftRight => Single(0x87),
            U64ShiftRight => Single(0x88),
            I64RotateLeft => Single(0x89),
            I64RotateRight => Single(0x8A),
            F32AbsoluteValue => Single(0x8B),
            F32Negate => Single(0x8C),
            F32Ceiling => Single(0x8D),
            F32Floor => Single(0x8E),
            F32Truncate => Single(0x8F),
            F32Nearest => Single(0x90),
            F32SquareRoot => Single(0x91),
            F32Add => Single(0x92),
            F32Sub => Single(0x93),
            F32Mul => Single(0x94),
            F32Div => Single(0x95),
            F32Min => Single(0x96),
            F32Max => Single(0x97),
            F32CopySign => Single(0x98),
            F64AbsoluteValue => Single(0x99),
            F64Negate => Single(0x9A),
            F64Ceiling => Single(0x9B),
            F64Floor => Single(0x9C),
            F64Truncate => Single(0x9D),
            F64Nearest => Single(0x9E),
            F64SquareRoot => Single(0x9F),
            F64Add => Single(0xA0),
            F64Sub => Single(0xA1),
            F64Mul => Single(0xA2),
            F64Div => Single(0xA3),
            F64Min => Single(0xA4),
            F64Max => Single(0xA5),
            F64CopySign => Single(0xA6),
            I32WrapI64 => Single(0xA7),
            S32TruncateF32 => Single(0xA8),
            U32TruncateF32 => Single(0xA9),
            S32TruncateF64 => Single(0xAA),
            U32TruncateF64 => Single(0xAB),
            I64ExtendS32 => Single(0xAC),
            I64ExtendU32 => Single(0xAD),
            S64TruncateF32 => Single(0xAE),
            U64TruncateF32 => Single(0xAF),
            S64TruncateF64 => Single(0xB0),
            U64TruncateF64 => Single(0xB1),
            F32ConvertS32 => Single(0xB2),
            F32ConvertU32 => Single(0xB3),
            F32ConvertS64 => Single(0xB4),
            F32ConvertU64 => Single(0xB5),
            F32DemoteF64 => Single(0xB6),
            F64ConvertS32 => Single(0xB7),
            F64ConvertU32 => Single(0xB8),
            F64ConvertS64 => Single(0xB9),
            F64ConvertU64 => Single(0xBA),
            F64PromoteF32 => Single(0xBB),
            I32ReinterpretF32 => Single(0xBC),
            I64ReinterpretF64 => Single(0xBD),
            F32ReinterpretI32 => Single(0xBE),
            F64ReinterpretI64 => Single(0xBF),
            S32Extend8 => Single(0xC0),
            S32Extend16 => Single(0xC1),
            S64Extend8 => Single(0xC2),
            S64Extend16 => Single(0xC3),
            S64Extend32 => Single(0xC4),
            S32SaturatingTruncateF32 => Extended(0xFC, 0),
            U32SaturatingTruncateF32 => Extended(0xFC, 1),
            S32SaturatingTruncateF64 => Extended(0xFC, 2),
            U32SaturatingTruncateF64 => Extended(0xFC, 3),
            S64SaturatingTruncateF32 => Extended(0xFC, 4),
            U64SaturatingTruncateF32 => Extended(0xFC, 5),
            S64SaturatingTruncateF64 => Extended(0xFC, 6),
            U64SaturatingTruncateF64 => Extended(0xFC, 7),
            V128Load {
                align: _,
                offset: _,
            } => Extended(0xFD, 0),
            V128LoadS8x8 {
                align: _,
                offset: _,
            } => Extended(0xFD, 1),
            V128LoadU8x8 {
                align: _,
                offset: _,
            } => Extended(0xFD, 2),
            V128LoadS16x4 {
                align: _,
                offset: _,
            } => Extended(0xFD, 3),
            V128LoadU16x4 {
                align: _,
                offset: _,
            } => Extended(0xFD, 4),
            V128LoadS32x2 {
                align: _,
                offset: _,
            } => Extended(0xFD, 5),
            V128LoadU32x2 {
                align: _,
                offset: _,
            } => Extended(0xFD, 6),
            V128LoadSplatI8 {
                align: _,
                offset: _,
            } => Extended(0xFD, 7),
            V128LoadSplatI16 {
                align: _,
                offset: _,
            } => Extended(0xFD, 8),
            V128LoadSplatI32 {
                align: _,
                offset: _,
            } => Extended(0xFD, 9),
            V128LoadSplatI64 {
                align: _,
                offset: _,
            } => Extended(0xFD, 10),
            V128LoadZeroI32 {
                align: _,
                offset: _,
            } => Extended(0xFD, 92),
            V128LoadZeroI64 {
                align: _,
                offset: _,
            } => Extended(0xFD, 93),
            V128Store {
                align: _,
                offset: _,
            } => Extended(0xFD, 11),
            V128Load8Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 84),
            V128Load16Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 85),
            V128Load32Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 86),
            V128Load64Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 87),
            V128Store8Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 88),
            V128Store16Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 89),
            V128Store32Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 90),
            V128Store64Lane {
                align: _,
                offset: _,
                lane: _,
            } => Extended(0xFD, 91),
            V128Const(_) => Extended(0xFD, 12),
            I8x16Shuffle { lanes: _ } => Extended(0xFD, 13),
            S8x16ExtractLane { lane: _ } => Extended(0xFD, 21),
            U8x16ExtractLane { lane: _ } => Extended(0xFD, 22),
            I8x16ReplaceLane { lane: _ } => Extended(0xFD, 23),
            S16x8ExtractLane { lane: _ } => Extended(0xFD, 24),
            U16x8ExtractLane { lane: _ } => Extended(0xFD, 25),
            I16x8ReplaceLane { lane: _ } => Extended(0xFD, 26),
            I32x4ExtractLane { lane: _ } => Extended(0xFD, 27),
            I32x4ReplaceLane { lane: _ } => Extended(0xFD, 28),
            I64x2ExtractLane { lane: _ } => Extended(0xFD, 29),
            I64x2ReplaceLane { lane: _ } => Extended(0xFD, 30),
            F32x4ExtractLane { lane: _ } => Extended(0xFD, 31),
            F32x4ReplaceLane { lane: _ } => Extended(0xFD, 32),
            F64x2ExtractLane { lane: _ } => Extended(0xFD, 33),
            F64x2ReplaceLane { lane: _ } => Extended(0xFD, 34),
            I8x16Swizzle => Extended(0xFD, 14),
            I8x16Splat => Extended(0xFD, 15),
            I16x8Splat => Extended(0xFD, 16),
            I32x4Splat => Extended(0xFD, 17),
            I64x2Splat => Extended(0xFD, 18),
            F32x4Splat => Extended(0xFD, 19),
            F64x2Splat => Extended(0xFD, 20),
            I8x16Equal => Extended(0xFD, 35),
            I8x16NotEqual => Extended(0xFD, 36),
            S8x16LessThan => Extended(0xFD, 37),
            U8x16LessThan => Extended(0xFD, 38),
            S8x16GreaterThan => Extended(0xFD, 39),
            U8x16GreaterThan => Extended(0xFD, 40),
            S8x16LessThanOrEqual => Extended(0xFD, 41),
            U8x16LessThanOrEqual => Extended(0xFD, 42),
            S8x16GreaterThanOrEqual => Extended(0xFD, 43),
            U8x16GreaterThanOrEqual => Extended(0xFD, 44),
            I16x8Equal => Extended(0xFD, 45),
            I16x8NotEqual => Extended(0xFD, 46),
            S16x8LessThan => Extended(0xFD, 47),
            U16x8LessThan => Extended(0xFD, 48),
            S16x8GreaterThan => Extended(0xFD, 49),
            U16x8GreaterThan => Extended(0xFD, 50),
            S16x8LessThanOrEqual => Extended(0xFD, 51),
            U16x8LessThanOrEqual => Extended(0xFD, 52),
            S16x8GreaterThanOrEqual => Extended(0xFD, 53),
            U16x8GreaterThanOrEqual => Extended(0xFD, 54),
            I32x4Equal => Extended(0xFD, 55),
            I32x4NotEqual => Extended(0xFD, 56),
            S32x4LessThan => Extended(0xFD, 57),
            U32x4LessThan => Extended(0xFD, 58),
            S32x4GreaterThan => Extended(0xFD, 59),
            U32x4GreaterThan => Extended(0xFD, 60),
            S32x4LessThanOrEqual => Extended(0xFD, 61),
            U32x4LessThanOrEqual => Extended(0xFD, 62),
            S32x4GreaterThanOrEqual => Extended(0xFD, 63),
            U32x4GreaterThanOrEqual => Extended(0xFD, 64),
            I64x2Equal => Extended(0xFD, 214),
            I64x2NotEqual => Extended(0xFD, 215),
            S64x2LessThan => Extended(0xFD, 216),
            S64x2GreaterThan => Extended(0xFD, 217),
            S64x2LessThanOrEqual => Extended(0xFD, 218),
            S64x2GreaterThanOrEqual => Extended(0xFD, 219),
            F32x4Equal => Extended(0xFD, 65),
            F32x4NotEqual => Extended(0xFD, 66),
            F32x4LessThan => Extended(0xFD, 67),
            F32x4GreaterThan => Extended(0xFD, 68),
            F32x4LessThanOrEqual => Extended(0xFD, 69),
            F32x4GreaterThanOrEqual => Extended(0xFD, 70),
            F64x2Equal => Extended(0xFD, 71),
            F64x2NotEqual => Extended(0xFD, 72),
            F64x2LessThan => Extended(0xFD, 73),
            F64x2GreaterThan => Extended(0xFD, 74),
            F64x2LessThanOrEqual => Extended(0xFD, 75),
            F64x2GreaterThanOrEqual => Extended(0xFD, 76),
            V128Not => Extended(0xFD, 77),
            V128And => Extended(0xFD, 78),
            V128AndNot => Extended(0xFD, 79),
            V128Or => Extended(0xFD, 80),
            V128Xor => Extended(0xFD, 81),
            V128BitSelect => Extended(0xFD, 82),
            V128AnyTrue => Extended(0xFD, 83),
            I8x16Abs => Extended(0xFD, 96),
            I8x16Neg => Extended(0xFD, 97),
            I8x16CountOnes => Extended(0xFD, 98),
            I8x16AllTrue => Extended(0xFD, 99),
            I8x16Bitmask => Extended(0xFD, 100),
            S8x16NarrowI16x8 => Extended(0xFD, 101),
            U8x16NarrowI16x8 => Extended(0xFD, 102),
            I8x16Shl => Extended(0xFD, 107),
            S8x16Shr => Extended(0xFD, 108),
            U8x16Shr => Extended(0xFD, 109),
            I8x16Add => Extended(0xFD, 110),
            S8x16AddSaturate => Extended(0xFD, 111),
            U8x16AddSaturate => Extended(0xFD, 112),
            I8x16Sub => Extended(0xFD, 113),
            S8x16SubSaturate => Extended(0xFD, 114),
            U8x16SubSaturate => Extended(0xFD, 115),
            S8x16Min => Extended(0xFD, 118),
            U8x16Min => Extended(0xFD, 119),
            S8x16Max => Extended(0xFD, 120),
            U8x16Max => Extended(0xFD, 121),
            U8x16Avgr => Extended(0xFD, 123),
            I16x8ExtendAddPairwiseS8x16 => Extended(0xFD, 124),
            I16x8ExtendAddPairwiseU8x16 => Extended(0xFD, 125),
            I16x8Abs => Extended(0xFD, 128),
            I16x8Neg => Extended(0xFD, 129),
            S16x8Q15MulRSat => Extended(0xFD, 130),
            I16x8AllTrue => Extended(0xFD, 131),
            I16x8Bitmask => Extended(0xFD, 132),
            S16x8NarrowI32x4 => Extended(0xFD, 133),
            U16x8NarrowI32x4 => Extended(0xFD, 134),
            I16x8ExtendLowS8x16 => Extended(0xFD, 135),
            I16x8ExtendHighS8x16 => Extended(0xFD, 136),
            I16x8ExtendLowU8x16 => Extended(0xFD, 137),
            I16x8ExtendHighU8x16 => Extended(0xFD, 138),
            I16x8Shl => Extended(0xFD, 139),
            S16x8Shr => Extended(0xFD, 140),
            U16x8Shr => Extended(0xFD, 141),
            I16x8Add => Extended(0xFD, 142),
            S16x8AddSaturate => Extended(0xFD, 143),
            U16x8AddSaturate => Extended(0xFD, 144),
            I16x8Sub => Extended(0xFD, 145),
            S16x8SubSaturate => Extended(0xFD, 146),
            U16x8SubSaturate => Extended(0xFD, 147),
            I16x8Mul => Extended(0xFD, 149),
            S16x8Min => Extended(0xFD, 150),
            U16x8Min => Extended(0xFD, 151),
            S16x8Max => Extended(0xFD, 152),
            U16x8Max => Extended(0xFD, 153),
            U16x8Avgr => Extended(0xFD, 155),
            I16x8ExtMulLowS8x16 => Extended(0xFD, 156),
            I16x8ExtMulHighS8x16 => Extended(0xFD, 157),
            I16x8ExtMulLowU8x16 => Extended(0xFD, 158),
            I16x8ExtMulHighU8x16 => Extended(0xFD, 159),
            I32x4ExtendAddPairwiseS16x8 => Extended(0xFD, 126),
            I32x4ExtendAddPairwiseU16x8 => Extended(0xFD, 127),
            I32x4Abs => Extended(0xFD, 160),
            I32x4Neg => Extended(0xFD, 161),
            I32x4AllTrue => Extended(0xFD, 163),
            I32x4Bitmask => Extended(0xFD, 164),
            I32x4ExtendLowS16x8 => Extended(0xFD, 167),
            I32x4ExtendHighS16x8 => Extended(0xFD, 168),
            I32x4ExtendLowU16x8 => Extended(0xFD, 169),
            I32x4ExtendHighU16x8 => Extended(0xFD, 170),
            I32x4Shl => Extended(0xFD, 171),
            S32x4Shr => Extended(0xFD, 172),
            U32x4Shr => Extended(0xFD, 173),
            I32x4Add => Extended(0xFD, 174),
            I32x4Sub => Extended(0xFD, 177),
            I32x4Mul => Extended(0xFD, 181),
            S32x4Min => Extended(0xFD, 182),
            U32x4Min => Extended(0xFD, 183),
            S32x4Max => Extended(0xFD, 184),
            U32x4Max => Extended(0xFD, 185),
            I32x4DotProductS16x8 => Extended(0xFD, 186),
            I32x4ExtMulLowS16x8 => Extended(0xFD, 188),
            I32x4ExtMulHighS16x8 => Extended(0xFD, 189),
            I32x4ExtMulLowU16x8 => Extended(0xFD, 190),
            I32x4ExtMulHighU16x8 => Extended(0xFD, 191),
            I64x2Abs => Extended(0xFD, 192),
            I64x2Neg => Extended(0xFD, 193),
            I64x2AllTrue => Extended(0xFD, 195),
            I64x2Bitmask => Extended(0xFD, 196),
            I64x2ExtendLowS32x4 => Extended(0xFD, 199),
            I64x2ExtendHighS32x4 => Extended(0xFD, 200),
            I64x2ExtendLowU32x4 => Extended(0xFD, 201),
            I64x2ExtendHighU32x4 => Extended(0xFD, 202),
            I64x2Shl => Extended(0xFD, 203),
            S64x2Shr => Extended(0xFD, 204),
            U64x2Shr => Extended(0xFD, 205),
            I64x2Add => Extended(0xFD, 206),
            I64x2Sub => Extended(0xFD, 209),
            I64x2Mul => Extended(0xFD, 213),
            I64x2ExtMulLowS32x4 => Extended(0xFD, 220),
            I64x2ExtMulHighS32x4 => Extended(0xFD, 221),
            I64x2ExtMulLowU32x4 => Extended(0xFD, 222),
            I64x2ExtMulHighU32x4 => Extended(0xFD, 223),
            F32x4Ceil => Extended(0xFD, 103),
            F32x4Floor => Extended(0xFD, 104),
            F32x4Trunc => Extended(0xFD, 105),
            F32x4Nearest => Extended(0xFD, 106),
            F32x4Abs => Extended(0xFD, 224),
            F32x4Neg => Extended(0xFD, 225),
            F32x4Sqrt => Extended(0xFD, 227),
            F32x4Add => Extended(0xFD, 228),
            F32x4Sub => Extended(0xFD, 229),
            F32x4Mul => Extended(0xFD, 230),
            F32x4Div => Extended(0xFD, 231),
            F32x4Min => Extended(0xFD, 232),
            F32x4Max => Extended(0xFD, 233),
            F32x4PMin => Extended(0xFD, 234),
            F32x4PMax => Extended(0xFD, 235),
            F64x2Ceil => Extended(0xFD, 116),
            F64x2Floor => Extended(0xFD, 117),
            F64x2Trunc => Extended(0xFD, 122),
            F64x2Nearest => Extended(0xFD, 148),
            F64x2Abs => Extended(0xFD, 236),
            F64x2Neg => Extended(0xFD, 237),
            F64x2Sqrt => Extended(0xFD, 239),
            F64x2Add => Extended(0xFD, 240),
            F64x2Sub => Extended(0xFD, 241),
            F64x2Mul => Extended(0xFD, 242),
            F64x2Div => Extended(0xFD, 243),
            F64x2Min => Extended(0xFD, 244),
            F64x2Max => Extended(0xFD, 245),
            F64x2PMin => Extended(0xFD, 246),
            F64x2PMax => Extended(0xFD, 247),
            S32x4TruncSatF32x4 => Extended(0xFD, 248),
            U32x4TruncSatF32x4 => Extended(0xFD, 249),
            F32x4ConvertS32x4 => Extended(0xFD, 250),
            F32x4ConvertU32x4 => Extended(0xFD, 251),
            S32x4TruncSatZeroF64x2 => Extended(0xFD, 252),
            U32x4TruncSatZeroF64x2 => Extended(0xFD, 253),
            F64x2ConvertLowS32x4 => Extended(0xFD, 254),
            F64x2ConvertLowU32x4 => Extended(0xFD, 255),
            F32x4DemoteF64x2Zero => Extended(0xFD, 94),
            F64x2PromoteLowF32x4 => Extended(0xFD, 95),
        }
    }
}

impl WasmEncode for Instruction {
    fn size(&self) -> usize {
        use Instruction::*;
        let immediate_size = match *self {
            Block(_) => 1,
            Loop(_) => 1,
            If(_) => 1,
            Branch(depth) => depth.size(),
            BranchIf(depth) => depth.size(),
            BranchTable {
                ref depths,
                failsafe,
            } => depths.size() + failsafe.size(),
            Call(func_idx) => func_idx.size(),
            CallIndirect {
                type_idx,
                table_idx,
            } => type_idx.size() + table_idx.size(),
            RefNull(ref_type) => ref_type.size(),
            RefFunc(func_idx) => func_idx.size(),
            LocalGet(local_index) | LocalSet(local_index) | LocalTee(local_index) => {
                local_index.size()
            }
            GlobalGet(global_index) | GlobalSet(global_index) => global_index.size(),
            TableGet(table_index) | TableSet(table_index) => table_index.size(),
            TableSize(table_index) | TableGrow(table_index) | TableFill(table_index) => {
                table_index.size()
            }
            TableCopy {
                dest_index,
                src_index,
            } => dest_index.size() + src_index.size(),
            TableInit {
                table_index,
                elem_index,
            } => table_index.size() + elem_index.size(),
            ElemDrop(elem_index) => elem_index.size(),
            I32Load { align, offset }
            | I64Load { align, offset }
            | F32Load { align, offset }
            | F64Load { align, offset }
            | I32LoadS8 { align, offset }
            | I32LoadU8 { align, offset }
            | I32LoadS16 { align, offset }
            | I32LoadU16 { align, offset }
            | I64LoadS8 { align, offset }
            | I64LoadU8 { align, offset }
            | I64LoadS16 { align, offset }
            | I64LoadU16 { align, offset }
            | I64LoadS32 { align, offset }
            | I64LoadU32 { align, offset }
            | I32Store { align, offset }
            | I64Store { align, offset }
            | F32Store { align, offset }
            | F64Store { align, offset }
            | I32StoreI8 { align, offset }
            | I32StoreI16 { align, offset }
            | I64StoreI8 { align, offset }
            | I64StoreI16 { align, offset }
            | I64StoreI32 { align, offset } => align.size() + offset.size(),
            MemorySize => 1,
            MemoryGrow => 1,
            MemoryInit(data_idx) => data_idx.size(),
            DataDrop(data_idx) => data_idx.size(),
            MemoryCopy => 2,
            MemoryFill => 1,
            I32Const(x) => x.size(),
            I64Const(x) => x.size(),
            F32Const(x) => x.size(),
            F64Const(x) => x.size(),

            V128Load { align, offset }
            | V128LoadS8x8 { align, offset }
            | V128LoadU8x8 { align, offset }
            | V128LoadS16x4 { align, offset }
            | V128LoadU16x4 { align, offset }
            | V128LoadS32x2 { align, offset }
            | V128LoadU32x2 { align, offset }
            | V128LoadSplatI8 { align, offset }
            | V128LoadSplatI16 { align, offset }
            | V128LoadSplatI32 { align, offset }
            | V128LoadSplatI64 { align, offset }
            | V128LoadZeroI32 { align, offset }
            | V128LoadZeroI64 { align, offset }
            | V128Store { align, offset } => align.size() + offset.size(),
            V128Load8Lane {
                align,
                offset,
                lane: _,
            }
            | V128Load16Lane {
                align,
                offset,
                lane: _,
            }
            | V128Load32Lane {
                align,
                offset,
                lane: _,
            }
            | V128Load64Lane {
                align,
                offset,
                lane: _,
            }
            | V128Store8Lane {
                align,
                offset,
                lane: _,
            }
            | V128Store16Lane {
                align,
                offset,
                lane: _,
            }
            | V128Store32Lane {
                align,
                offset,
                lane: _,
            }
            | V128Store64Lane {
                align,
                offset,
                lane: _,
            } => 1 + align.size() + offset.size(),
            V128Const(_) => 18,
            I8x16Shuffle { lanes: _ } => 18,
            S8x16ExtractLane { lane: _ }
            | U8x16ExtractLane { lane: _ }
            | I8x16ReplaceLane { lane: _ }
            | S16x8ExtractLane { lane: _ }
            | U16x8ExtractLane { lane: _ }
            | I16x8ReplaceLane { lane: _ }
            | I32x4ExtractLane { lane: _ }
            | I32x4ReplaceLane { lane: _ }
            | I64x2ExtractLane { lane: _ }
            | I64x2ReplaceLane { lane: _ }
            | F32x4ExtractLane { lane: _ }
            | F32x4ReplaceLane { lane: _ }
            | F64x2ExtractLane { lane: _ }
            | F64x2ReplaceLane { lane: _ } => 3,

            _ => 0,
        };
        self.instr_id().size() + immediate_size
    }

    fn encode(&self, v: &mut Vec<u8>) {
        use Instruction::*;
        self.instr_id().encode(v);
        match *self {
            Block(kind) => kind.encode(v),
            Loop(kind) => kind.encode(v),
            If(kind) => kind.encode(v),
            Branch(depth) => depth.encode(v),
            BranchIf(depth) => depth.encode(v),
            BranchTable {
                ref depths,
                failsafe,
            } => {
                for depth in depths {
                    depth.encode(v);
                }
                failsafe.encode(v);
            }
            Call(func_idx) => {
                (func_idx).encode(v);
            }
            CallIndirect {
                type_idx,
                table_idx,
            } => (type_idx, table_idx).encode(v),
            RefNull(ref_type) => ref_type.encode(v),
            RefFunc(func_index) => func_index.encode(v),
            LocalGet(local_index) => local_index.encode(v),
            LocalSet(local_index) => local_index.encode(v),
            LocalTee(local_index) => local_index.encode(v),
            GlobalGet(global_index) => global_index.encode(v),
            GlobalSet(global_index) => global_index.encode(v),
            TableGet(table_index) => table_index.encode(v),
            TableSet(table_index) => table_index.encode(v),
            TableSize(table_index) => table_index.encode(v),
            TableGrow(table_index) => table_index.encode(v),
            TableFill(table_index) => table_index.encode(v),
            TableCopy {
                dest_index,
                src_index,
            } => (dest_index, src_index).encode(v),
            TableInit {
                table_index,
                elem_index,
            } => (table_index, elem_index).encode(v),
            ElemDrop(elem_index) => (elem_index).encode(v),
            I32Load { align, offset }
            | I64Load { align, offset }
            | F32Load { align, offset }
            | F64Load { align, offset }
            | I32LoadS8 { align, offset }
            | I32LoadU8 { align, offset }
            | I32LoadS16 { align, offset }
            | I32LoadU16 { align, offset }
            | I64LoadS8 { align, offset }
            | I64LoadU8 { align, offset }
            | I64LoadS16 { align, offset }
            | I64LoadU16 { align, offset }
            | I64LoadS32 { align, offset }
            | I64LoadU32 { align, offset }
            | I32Store { align, offset }
            | I64Store { align, offset }
            | F32Store { align, offset }
            | F64Store { align, offset }
            | I32StoreI8 { align, offset }
            | I32StoreI16 { align, offset }
            | I64StoreI8 { align, offset }
            | I64StoreI16 { align, offset }
            | I64StoreI32 { align, offset } => (align, offset).encode(v),
            MemorySize => 0.encode(v),
            MemoryGrow => 0.encode(v),
            MemoryInit(data_idx) => (data_idx, 0u8).encode(v),
            DataDrop(data_idx) => (data_idx).encode(v),
            MemoryCopy => [0u8, 0u8].encode(v),
            MemoryFill => 0u8.encode(v),
            I32Const(x) => x.encode(v),
            I64Const(x) => x.encode(v),
            F32Const(x) => x.encode(v),
            F64Const(x) => x.encode(v),
            V128Load { align, offset }
            | V128LoadS8x8 { align, offset }
            | V128LoadU8x8 { align, offset }
            | V128LoadS16x4 { align, offset }
            | V128LoadU16x4 { align, offset }
            | V128LoadS32x2 { align, offset }
            | V128LoadU32x2 { align, offset }
            | V128LoadSplatI8 { align, offset }
            | V128LoadSplatI16 { align, offset }
            | V128LoadSplatI32 { align, offset }
            | V128LoadSplatI64 { align, offset }
            | V128LoadZeroI32 { align, offset }
            | V128LoadZeroI64 { align, offset }
            | V128Store { align, offset } => (align, offset).encode(v),
            V128Load8Lane {
                align,
                offset,
                lane,
            }
            | V128Load16Lane {
                align,
                offset,
                lane,
            }
            | V128Load32Lane {
                align,
                offset,
                lane,
            }
            | V128Load64Lane {
                align,
                offset,
                lane,
            }
            | V128Store8Lane {
                align,
                offset,
                lane,
            }
            | V128Store16Lane {
                align,
                offset,
                lane,
            }
            | V128Store32Lane {
                align,
                offset,
                lane,
            }
            | V128Store64Lane {
                align,
                offset,
                lane,
            } => (align, offset, lane).encode(v),
            V128Const(bytes) => bytes.encode(v),
            I8x16Shuffle { lanes } => lanes.encode(v),
            S8x16ExtractLane { lane } => lane.encode(v),
            U8x16ExtractLane { lane } => lane.encode(v),
            I8x16ReplaceLane { lane } => lane.encode(v),
            S16x8ExtractLane { lane } => lane.encode(v),
            U16x8ExtractLane { lane } => lane.encode(v),
            I16x8ReplaceLane { lane } => lane.encode(v),
            I32x4ExtractLane { lane } => lane.encode(v),
            I32x4ReplaceLane { lane } => lane.encode(v),
            I64x2ExtractLane { lane } => lane.encode(v),
            I64x2ReplaceLane { lane } => lane.encode(v),
            F32x4ExtractLane { lane } => lane.encode(v),
            F32x4ReplaceLane { lane } => lane.encode(v),
            F64x2ExtractLane { lane } => lane.encode(v),
            F64x2ReplaceLane { lane } => lane.encode(v),

            // instructions that have no immediates are encoded just by their id
            _ => (),
        }
    }
}

impl WasmDecode for Instruction {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::ErrorKind> {
        use InstrId::*;
        use Instruction::*;

        let id = InstrId::decode(buf)?;
        let instr = match id {
            Single(0) => Unreachable,
            Single(1) => NoOp,
            Single(2) => Block(Option::<ValType>::decode(buf)?),
            Single(3) => Loop(Option::<ValType>::decode(buf)?),
            Single(4) => If(Option::<ValType>::decode(buf)?),
            Single(5) => Else,
            Single(0xB) => End,
            Single(0xC) => Branch(u32::decode(buf)?),
            Single(0xD) => BranchIf(u32::decode(buf)?),
            Single(0xE) => BranchTable {
                depths: Vec::<u32>::decode(buf)?,
                failsafe: u32::decode(buf)?,
            },
            Single(0xF) => Return,
            Single(0x10) => Call(u32::decode(buf)?),
            Single(0x11) => CallIndirect {
                type_idx: u32::decode(buf)?,
                table_idx: u32::decode(buf)?,
            },
            Single(0xD0) => RefNull(RefType::decode(buf)?),
            Single(0xD1) => RefIsNull,
            Single(0xD2) => RefFunc(u32::decode(buf)?),
            Single(0x1A) => Drop,
            Single(0x1B) => Select,
            Single(0x20) => LocalGet(u32::decode(buf)?),
            Single(0x21) => LocalSet(u32::decode(buf)?),
            Single(0x22) => LocalTee(u32::decode(buf)?),
            Single(0x23) => GlobalGet(u32::decode(buf)?),
            Single(0x24) => GlobalSet(u32::decode(buf)?),
            Single(0x25) => TableGet(u32::decode(buf)?),
            Single(0x26) => TableSet(u32::decode(buf)?),
            Extended(0xFCu8, 12u32) => TableInit {
                table_index: u32::decode(buf)?,
                elem_index: u32::decode(buf)?,
            },
            Extended(0xFCu8, 13u32) => ElemDrop(u32::decode(buf)?),
            Extended(0xFCu8, 14u32) => TableCopy {
                dest_index: u32::decode(buf)?,
                src_index: u32::decode(buf)?,
            },
            Extended(0xFCu8, 16u32) => TableSize(u32::decode(buf)?),
            Extended(0xFCu8, 15u32) => TableGrow(u32::decode(buf)?),
            Extended(0xFCu8, 17u32) => TableFill(u32::decode(buf)?),
            Single(0x28) => I32Load {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x29) => I64Load {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x2A) => F32Load {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x2B) => F64Load {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x2C) => I32LoadS8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x2D) => I32LoadU8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x2E) => I32LoadS16 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x2F) => I32LoadU16 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x30) => I64LoadS8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x31) => I64LoadU8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x32) => I64LoadS16 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x33) => I64LoadU16 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x34) => I64LoadS32 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x35) => I64LoadU32 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x36) => I32Store {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x37) => I64Store {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x38) => F32Store {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x39) => F64Store {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x3A) => I32StoreI8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x3B) => I32StoreI16 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x3C) => I64StoreI8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x3D) => I64StoreI16 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x3E) => I64StoreI32 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Single(0x3F) => {
                let mem_idx = u32::decode(buf)?;
                if mem_idx != 0 {
                    return Err(crate::ErrorKind::MemIndexOutOfBounds(mem_idx));
                }
                MemorySize
            },
            Single(0x40) => {
                let mem_idx = u32::decode(buf)?;
                if mem_idx != 0 {
                    return Err(crate::ErrorKind::MemIndexOutOfBounds(mem_idx));
                }
                MemoryGrow
            },
            Extended(0xFC, 8) => MemoryInit(u32::decode(buf)?),
            Extended(0x40, 9) => DataDrop(u32::decode(buf)?),
            Extended(0xFC, 10) => MemoryCopy,
            Extended(0xFC, 11) => MemoryFill,
            Single(0x41) => I32Const(i32::decode(buf)?),
            Single(0x42) => I64Const(i64::decode(buf)?),
            Single(0x43) => F32Const(f32::decode(buf)?),
            Single(0x44) => F64Const(f64::decode(buf)?),
            Single(0x45) => I32EqualsZero,
            Single(0x46) => I32Equal,
            Single(0x47) => I32NotEqual,
            Single(0x48) => S32LessThan,
            Single(0x49) => U32LessThan,
            Single(0x4A) => S32GreaterThan,
            Single(0x4B) => U32GreaterThan,
            Single(0x4C) => S32LessThanOrEqual,
            Single(0x4D) => U32LessThanOrEqual,
            Single(0x4E) => S32GreaterThanOrEqual,
            Single(0x4F) => U32GreaterThanOrEqual,
            Single(0x50) => I64EqualsZero,
            Single(0x51) => I64Equal,
            Single(0x52) => I64NotEqual,
            Single(0x53) => S64LessThan,
            Single(0x54) => U64LessThan,
            Single(0x55) => S64GreaterThan,
            Single(0x56) => U64GreaterThan,
            Single(0x57) => S64LessThanOrEqual,
            Single(0x58) => U64LessThanOrEqual,
            Single(0x59) => S64GreaterThanOrEqual,
            Single(0x5A) => U64GreaterThanOrEqual,
            Single(0x5B) => F32Equal,
            Single(0x5C) => F32NotEqual,
            Single(0x5D) => F32LessThan,
            Single(0x5E) => F32GreaterThan,
            Single(0x5F) => F32LessThanOrEqual,
            Single(0x60) => F32GreaterThanOrEqual,
            Single(0x61) => F64Equal,
            Single(0x62) => F64NotEqual,
            Single(0x63) => F64LessThan,
            Single(0x64) => F64GreaterThan,
            Single(0x65) => F64LessThanOrEqual,
            Single(0x66) => F64GreaterThanOrEqual,
            Single(0x67) => I32CountLeadingZeroes,
            Single(0x68) => I32CountTrailingZeroes,
            Single(0x69) => I32CountOnes,
            Single(0x6A) => I32Add,
            Single(0x6B) => I32Sub,
            Single(0x6C) => I32Mul,
            Single(0x6D) => S32Div,
            Single(0x6E) => U32Div,
            Single(0x6F) => S32Rem,
            Single(0x70) => U32Rem,
            Single(0x71) => I32And,
            Single(0x72) => I32Or,
            Single(0x73) => I32Xor,
            Single(0x74) => I32ShiftLeft,
            Single(0x75) => S32ShiftRight,
            Single(0x76) => U32ShiftRight,
            Single(0x77) => I32RotateLeft,
            Single(0x78) => I32RotateRight,
            Single(0x79) => I64CountLeadingZeroes,
            Single(0x7A) => I64CountTrailingZeroes,
            Single(0x7B) => I64CountOnes,
            Single(0x7C) => I64Add,
            Single(0x7D) => I64Sub,
            Single(0x7E) => I64Mul,
            Single(0x7F) => S64Div,
            Single(0x80) => U64Div,
            Single(0x81) => S64Rem,
            Single(0x82) => U64Rem,
            Single(0x83) => I64And,
            Single(0x84) => I64Or,
            Single(0x85) => I64Xor,
            Single(0x86) => I64ShiftLeft,
            Single(0x87) => S64ShiftRight,
            Single(0x88) => U64ShiftRight,
            Single(0x89) => I64RotateLeft,
            Single(0x8A) => I64RotateRight,
            Single(0x8B) => F32AbsoluteValue,
            Single(0x8C) => F32Negate,
            Single(0x8D) => F32Ceiling,
            Single(0x8E) => F32Floor,
            Single(0x8F) => F32Truncate,
            Single(0x90) => F32Nearest,
            Single(0x91) => F32SquareRoot,
            Single(0x92) => F32Add,
            Single(0x93) => F32Sub,
            Single(0x94) => F32Mul,
            Single(0x95) => F32Div,
            Single(0x96) => F32Min,
            Single(0x97) => F32Max,
            Single(0x98) => F32CopySign,
            Single(0x99) => F64AbsoluteValue,
            Single(0x9A) => F64Negate,
            Single(0x9B) => F64Ceiling,
            Single(0x9C) => F64Floor,
            Single(0x9D) => F64Truncate,
            Single(0x9E) => F64Nearest,
            Single(0x9F) => F64SquareRoot,
            Single(0xA0) => F64Add,
            Single(0xA1) => F64Sub,
            Single(0xA2) => F64Mul,
            Single(0xA3) => F64Div,
            Single(0xA4) => F64Min,
            Single(0xA5) => F64Max,
            Single(0xA6) => F64CopySign,
            Single(0xA7) => I32WrapI64,
            Single(0xA8) => S32TruncateF32,
            Single(0xA9) => U32TruncateF32,
            Single(0xAA) => S32TruncateF64,
            Single(0xAB) => U32TruncateF64,
            Single(0xAC) => I64ExtendS32,
            Single(0xAD) => I64ExtendU32,
            Single(0xAE) => S64TruncateF32,
            Single(0xAF) => U64TruncateF32,
            Single(0xB0) => S64TruncateF64,
            Single(0xB1) => U64TruncateF64,
            Single(0xB2) => F32ConvertS32,
            Single(0xB3) => F32ConvertU32,
            Single(0xB4) => F32ConvertS64,
            Single(0xB5) => F32ConvertU64,
            Single(0xB6) => F32DemoteF64,
            Single(0xB7) => F64ConvertS32,
            Single(0xB8) => F64ConvertU32,
            Single(0xB9) => F64ConvertS64,
            Single(0xBA) => F64ConvertU64,
            Single(0xBB) => F64PromoteF32,
            Single(0xBC) => I32ReinterpretF32,
            Single(0xBD) => I64ReinterpretF64,
            Single(0xBE) => F32ReinterpretI32,
            Single(0xBF) => F64ReinterpretI64,
            Single(0xC0) => S32Extend8,
            Single(0xC1) => S32Extend16,
            Single(0xC2) => S64Extend8,
            Single(0xC3) => S64Extend16,
            Single(0xC4) => S64Extend32,
            Extended(0xFC, 0) => S32SaturatingTruncateF32,
            Extended(0xFC, 1) => U32SaturatingTruncateF32,
            Extended(0xFC, 2) => S32SaturatingTruncateF64,
            Extended(0xFC, 3) => U32SaturatingTruncateF64,
            Extended(0xFC, 4) => S64SaturatingTruncateF32,
            Extended(0xFC, 5) => U64SaturatingTruncateF32,
            Extended(0xFC, 6) => S64SaturatingTruncateF64,
            Extended(0xFC, 7) => U64SaturatingTruncateF64,
            Extended(0xFD, 0) => V128Load {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 1) => V128LoadS8x8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 2) => V128LoadU8x8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 3) => V128LoadS16x4 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 4) => V128LoadU16x4 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 5) => V128LoadS32x2 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 6) => V128LoadU32x2 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 7) => V128LoadSplatI8 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 8) => V128LoadSplatI16 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 9) => V128LoadSplatI32 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 10) => V128LoadSplatI64 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 92) => V128LoadZeroI32 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 93) => V128LoadZeroI64 {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 11) => V128Store {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
            },
            Extended(0xFD, 84) => V128Load8Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 85) => V128Load16Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 86) => V128Load32Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 87) => V128Load64Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 88) => V128Store8Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 89) => V128Store16Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 90) => V128Store32Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 91) => V128Store64Lane {
                align: u32::decode(buf)?,
                offset: u32::decode(buf)?,
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 12) => V128Const(<[u8; 16]>::decode(buf)?),
            Extended(0xFD, 13) => I8x16Shuffle {
                lanes: <[u8; 16]>::decode(buf)?,
            },
            Extended(0xFD, 21) => S8x16ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 22) => U8x16ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 23) => I8x16ReplaceLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 24) => S16x8ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 25) => U16x8ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 26) => I16x8ReplaceLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 27) => I32x4ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 28) => I32x4ReplaceLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 29) => I64x2ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 30) => I64x2ReplaceLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 31) => F32x4ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 32) => F32x4ReplaceLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 33) => F64x2ExtractLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 34) => F64x2ReplaceLane {
                lane: u8::decode(buf)?,
            },
            Extended(0xFD, 14) => I8x16Swizzle,
            Extended(0xFD, 15) => I8x16Splat,
            Extended(0xFD, 16) => I16x8Splat,
            Extended(0xFD, 17) => I32x4Splat,
            Extended(0xFD, 18) => I64x2Splat,
            Extended(0xFD, 19) => F32x4Splat,
            Extended(0xFD, 20) => F64x2Splat,
            Extended(0xFD, 35) => I8x16Equal,
            Extended(0xFD, 36) => I8x16NotEqual,
            Extended(0xFD, 37) => S8x16LessThan,
            Extended(0xFD, 38) => U8x16LessThan,
            Extended(0xFD, 39) => S8x16GreaterThan,
            Extended(0xFD, 40) => U8x16GreaterThan,
            Extended(0xFD, 41) => S8x16LessThanOrEqual,
            Extended(0xFD, 42) => U8x16LessThanOrEqual,
            Extended(0xFD, 43) => S8x16GreaterThanOrEqual,
            Extended(0xFD, 44) => U8x16GreaterThanOrEqual,
            Extended(0xFD, 45) => I16x8Equal,
            Extended(0xFD, 46) => I16x8NotEqual,
            Extended(0xFD, 47) => S16x8LessThan,
            Extended(0xFD, 48) => U16x8LessThan,
            Extended(0xFD, 49) => S16x8GreaterThan,
            Extended(0xFD, 50) => U16x8GreaterThan,
            Extended(0xFD, 51) => S16x8LessThanOrEqual,
            Extended(0xFD, 52) => U16x8LessThanOrEqual,
            Extended(0xFD, 53) => S16x8GreaterThanOrEqual,
            Extended(0xFD, 54) => U16x8GreaterThanOrEqual,
            Extended(0xFD, 55) => I32x4Equal,
            Extended(0xFD, 56) => I32x4NotEqual,
            Extended(0xFD, 57) => S32x4LessThan,
            Extended(0xFD, 58) => U32x4LessThan,
            Extended(0xFD, 59) => S32x4GreaterThan,
            Extended(0xFD, 60) => U32x4GreaterThan,
            Extended(0xFD, 61) => S32x4LessThanOrEqual,
            Extended(0xFD, 62) => U32x4LessThanOrEqual,
            Extended(0xFD, 63) => S32x4GreaterThanOrEqual,
            Extended(0xFD, 64) => U32x4GreaterThanOrEqual,
            Extended(0xFD, 214) => I64x2Equal,
            Extended(0xFD, 215) => I64x2NotEqual,
            Extended(0xFD, 216) => S64x2LessThan,
            Extended(0xFD, 217) => S64x2GreaterThan,
            Extended(0xFD, 218) => S64x2LessThanOrEqual,
            Extended(0xFD, 219) => S64x2GreaterThanOrEqual,
            Extended(0xFD, 65) => F32x4Equal,
            Extended(0xFD, 66) => F32x4NotEqual,
            Extended(0xFD, 67) => F32x4LessThan,
            Extended(0xFD, 68) => F32x4GreaterThan,
            Extended(0xFD, 69) => F32x4LessThanOrEqual,
            Extended(0xFD, 70) => F32x4GreaterThanOrEqual,
            Extended(0xFD, 71) => F64x2Equal,
            Extended(0xFD, 72) => F64x2NotEqual,
            Extended(0xFD, 73) => F64x2LessThan,
            Extended(0xFD, 74) => F64x2GreaterThan,
            Extended(0xFD, 75) => F64x2LessThanOrEqual,
            Extended(0xFD, 76) => F64x2GreaterThanOrEqual,
            Extended(0xFD, 77) => V128Not,
            Extended(0xFD, 78) => V128And,
            Extended(0xFD, 79) => V128AndNot,
            Extended(0xFD, 80) => V128Or,
            Extended(0xFD, 81) => V128Xor,
            Extended(0xFD, 82) => V128BitSelect,
            Extended(0xFD, 83) => V128AnyTrue,
            Extended(0xFD, 96) => I8x16Abs,
            Extended(0xFD, 97) => I8x16Neg,
            Extended(0xFD, 98) => I8x16CountOnes,
            Extended(0xFD, 99) => I8x16AllTrue,
            Extended(0xFD, 100) => I8x16Bitmask,
            Extended(0xFD, 101) => S8x16NarrowI16x8,
            Extended(0xFD, 102) => U8x16NarrowI16x8,
            Extended(0xFD, 107) => I8x16Shl,
            Extended(0xFD, 108) => S8x16Shr,
            Extended(0xFD, 109) => U8x16Shr,
            Extended(0xFD, 110) => I8x16Add,
            Extended(0xFD, 111) => S8x16AddSaturate,
            Extended(0xFD, 112) => U8x16AddSaturate,
            Extended(0xFD, 113) => I8x16Sub,
            Extended(0xFD, 114) => S8x16SubSaturate,
            Extended(0xFD, 115) => U8x16SubSaturate,
            Extended(0xFD, 118) => S8x16Min,
            Extended(0xFD, 119) => U8x16Min,
            Extended(0xFD, 120) => S8x16Max,
            Extended(0xFD, 121) => U8x16Max,
            Extended(0xFD, 123) => U8x16Avgr,
            Extended(0xFD, 124) => I16x8ExtendAddPairwiseS8x16,
            Extended(0xFD, 125) => I16x8ExtendAddPairwiseU8x16,
            Extended(0xFD, 128) => I16x8Abs,
            Extended(0xFD, 129) => I16x8Neg,
            Extended(0xFD, 130) => S16x8Q15MulRSat,
            Extended(0xFD, 131) => I16x8AllTrue,
            Extended(0xFD, 132) => I16x8Bitmask,
            Extended(0xFD, 133) => S16x8NarrowI32x4,
            Extended(0xFD, 134) => U16x8NarrowI32x4,
            Extended(0xFD, 135) => I16x8ExtendLowS8x16,
            Extended(0xFD, 136) => I16x8ExtendHighS8x16,
            Extended(0xFD, 137) => I16x8ExtendLowU8x16,
            Extended(0xFD, 138) => I16x8ExtendHighU8x16,
            Extended(0xFD, 139) => I16x8Shl,
            Extended(0xFD, 140) => S16x8Shr,
            Extended(0xFD, 141) => U16x8Shr,
            Extended(0xFD, 142) => I16x8Add,
            Extended(0xFD, 143) => S16x8AddSaturate,
            Extended(0xFD, 144) => U16x8AddSaturate,
            Extended(0xFD, 145) => I16x8Sub,
            Extended(0xFD, 146) => S16x8SubSaturate,
            Extended(0xFD, 147) => U16x8SubSaturate,
            Extended(0xFD, 149) => I16x8Mul,
            Extended(0xFD, 150) => S16x8Min,
            Extended(0xFD, 151) => U16x8Min,
            Extended(0xFD, 152) => S16x8Max,
            Extended(0xFD, 153) => U16x8Max,
            Extended(0xFD, 155) => U16x8Avgr,
            Extended(0xFD, 156) => I16x8ExtMulLowS8x16,
            Extended(0xFD, 157) => I16x8ExtMulHighS8x16,
            Extended(0xFD, 158) => I16x8ExtMulLowU8x16,
            Extended(0xFD, 159) => I16x8ExtMulHighU8x16,
            Extended(0xFD, 126) => I32x4ExtendAddPairwiseS16x8,
            Extended(0xFD, 127) => I32x4ExtendAddPairwiseU16x8,
            Extended(0xFD, 160) => I32x4Abs,
            Extended(0xFD, 161) => I32x4Neg,
            Extended(0xFD, 163) => I32x4AllTrue,
            Extended(0xFD, 164) => I32x4Bitmask,
            Extended(0xFD, 167) => I32x4ExtendLowS16x8,
            Extended(0xFD, 168) => I32x4ExtendHighS16x8,
            Extended(0xFD, 169) => I32x4ExtendLowU16x8,
            Extended(0xFD, 170) => I32x4ExtendHighU16x8,
            Extended(0xFD, 171) => I32x4Shl,
            Extended(0xFD, 172) => S32x4Shr,
            Extended(0xFD, 173) => U32x4Shr,
            Extended(0xFD, 174) => I32x4Add,
            Extended(0xFD, 177) => I32x4Sub,
            Extended(0xFD, 181) => I32x4Mul,
            Extended(0xFD, 182) => S32x4Min,
            Extended(0xFD, 183) => U32x4Min,
            Extended(0xFD, 184) => S32x4Max,
            Extended(0xFD, 185) => U32x4Max,
            Extended(0xFD, 186) => I32x4DotProductS16x8,
            Extended(0xFD, 188) => I32x4ExtMulLowS16x8,
            Extended(0xFD, 189) => I32x4ExtMulHighS16x8,
            Extended(0xFD, 190) => I32x4ExtMulLowU16x8,
            Extended(0xFD, 191) => I32x4ExtMulHighU16x8,
            Extended(0xFD, 192) => I64x2Abs,
            Extended(0xFD, 193) => I64x2Neg,
            Extended(0xFD, 195) => I64x2AllTrue,
            Extended(0xFD, 196) => I64x2Bitmask,
            Extended(0xFD, 199) => I64x2ExtendLowS32x4,
            Extended(0xFD, 200) => I64x2ExtendHighS32x4,
            Extended(0xFD, 201) => I64x2ExtendLowU32x4,
            Extended(0xFD, 202) => I64x2ExtendHighU32x4,
            Extended(0xFD, 203) => I64x2Shl,
            Extended(0xFD, 204) => S64x2Shr,
            Extended(0xFD, 205) => U64x2Shr,
            Extended(0xFD, 206) => I64x2Add,
            Extended(0xFD, 209) => I64x2Sub,
            Extended(0xFD, 213) => I64x2Mul,
            Extended(0xFD, 220) => I64x2ExtMulLowS32x4,
            Extended(0xFD, 221) => I64x2ExtMulHighS32x4,
            Extended(0xFD, 222) => I64x2ExtMulLowU32x4,
            Extended(0xFD, 223) => I64x2ExtMulHighU32x4,
            Extended(0xFD, 103) => F32x4Ceil,
            Extended(0xFD, 104) => F32x4Floor,
            Extended(0xFD, 105) => F32x4Trunc,
            Extended(0xFD, 106) => F32x4Nearest,
            Extended(0xFD, 224) => F32x4Abs,
            Extended(0xFD, 225) => F32x4Neg,
            Extended(0xFD, 227) => F32x4Sqrt,
            Extended(0xFD, 228) => F32x4Add,
            Extended(0xFD, 229) => F32x4Sub,
            Extended(0xFD, 230) => F32x4Mul,
            Extended(0xFD, 231) => F32x4Div,
            Extended(0xFD, 232) => F32x4Min,
            Extended(0xFD, 233) => F32x4Max,
            Extended(0xFD, 234) => F32x4PMin,
            Extended(0xFD, 235) => F32x4PMax,
            Extended(0xFD, 116) => F64x2Ceil,
            Extended(0xFD, 117) => F64x2Floor,
            Extended(0xFD, 122) => F64x2Trunc,
            Extended(0xFD, 148) => F64x2Nearest,
            Extended(0xFD, 236) => F64x2Abs,
            Extended(0xFD, 237) => F64x2Neg,
            Extended(0xFD, 239) => F64x2Sqrt,
            Extended(0xFD, 240) => F64x2Add,
            Extended(0xFD, 241) => F64x2Sub,
            Extended(0xFD, 242) => F64x2Mul,
            Extended(0xFD, 243) => F64x2Div,
            Extended(0xFD, 244) => F64x2Min,
            Extended(0xFD, 245) => F64x2Max,
            Extended(0xFD, 246) => F64x2PMin,
            Extended(0xFD, 247) => F64x2PMax,
            Extended(0xFD, 248) => S32x4TruncSatF32x4,
            Extended(0xFD, 249) => U32x4TruncSatF32x4,
            Extended(0xFD, 250) => F32x4ConvertS32x4,
            Extended(0xFD, 251) => F32x4ConvertU32x4,
            Extended(0xFD, 252) => S32x4TruncSatZeroF64x2,
            Extended(0xFD, 253) => U32x4TruncSatZeroF64x2,
            Extended(0xFD, 254) => F64x2ConvertLowS32x4,
            Extended(0xFD, 255) => F64x2ConvertLowU32x4,
            Extended(0xFD, 94) => F32x4DemoteF64x2Zero,
            Extended(0xFD, 95) => F64x2PromoteLowF32x4,
            _ => {
                let (x, y) = id.deconstruct();
                return Err(crate::encode::ErrorKind::InvalidInstruction(x, y));
            }
        };
        Ok(instr)
    }
}

enum InstrId {
    Single(u8),
    Extended(u8, u32),
}

impl InstrId {
    fn deconstruct(&self) -> (u8, Option<u32>) {
        match *self {
            InstrId::Single(x) => (x, None),
            InstrId::Extended(x, y) => (x, Some(y)),
        }
    }
}

impl WasmEncode for InstrId {
    fn size(&self) -> usize {
        match *self {
            InstrId::Single(_) => 1,
            InstrId::Extended(_, y) => 1 + y.size(),
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        match *self {
            InstrId::Single(x) => x.encode(v),
            InstrId::Extended(x, y) => (x, y).encode(v),
        }
    }
}

impl WasmDecode for InstrId {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::ErrorKind> {
        let b = u8::decode(buf)?;
        match b {
            0xFC | 0xFD => Ok(Self::Extended(b, u32::decode(buf)?)),
            _ => Ok(Self::Single(b)),
        }
    }
}
