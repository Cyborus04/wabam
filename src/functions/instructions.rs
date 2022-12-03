use crate::{RefType, ValType, WasmEncode};

#[doc(hidden)]
#[macro_export]
macro_rules! instr {
    (unreachable) => { $crate::Instruction::Unreachable };
    (nop) => { $crate::Instruction::NoOp };

    (block) => { $crate::Instruction::Block { kind: None } };
    (block (result $t:tt)) => { $crate::Instruction::Block { kind: $crate::mlt!($t) } };

    (loop) => { $crate::Instruction::Block { kind: None } };
    (loop (result $t:tt)) => { $crate::Instruction::Block { kind: $crate::mlt!($t) } };

    (if) => { $crate::Instruction::Block { kind: None } };
    (if (result $t:tt)) => { $crate::Instruction::Block { kind: $crate::mlt!($t) } };

    (else) => { $crate::Instruction::Else };
    (end) => { $crate::Instruction::End };
    (br $x:tt) => { $crate::Instruction::Branch { depth: $crate::ml!($x) } };
    (br_if $x:tt) => { $crate::Instruction::BranchIf { depth: $crate::ml!($x) } };
    (br_table $($x:tt)+) => {
        {
            let a = [$($crate::ml!($x)),*];
            let (failsafe, depths) = a.split_last().unwrap();
            $crate::Instruction::BranchTable { depths: depths.to_vec(), failsafe: *failsafe }
        }
    };

    (return) => { $crate::Instruction::Return };
    (call $x:tt) => { $crate::Instruction::Call { func_idx: $crate::ml!($x) } };
    (call_indirect $type:tt $table:tt) => { $crate::Instruction::CallIndirect { type_idx: $crate::ml!($type), table_idx: $crate::ml!($table) } };

    (drop) => { $crate::Instruction::Drop };
    (select) => { $crate::Instruction::Select };

    (local.get $x:tt) => { $crate::Instruction::LocalGet { local_index: $crate::ml!($x) } };
    (local.set $x:tt) => { $crate::Instruction::LocalSet { local_index: $crate::ml!($x) } };
    (local.tee $x:tt) => { $crate::Instruction::LocalTee { local_index: $crate::ml!($x) } };
    (global.get $x:tt) => { $crate::Instruction::GlobalGet { global_index: $crate::ml!($x) } };
    (global.set $x:tt) => { $crate::Instruction::GlobalSet { global_index: $crate::ml!($x) } };

}

#[doc(hidden)]
#[macro_export]
macro_rules! ml {
    ({ $x:expr }) => {
        $x
    };
    ($x:literal) => {
        $x
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! mlto {
    (i32) => {
        Some($crate::ValType::I32)
    };
    (i64) => {
        Some($crate::ValType::I64)
    };
    (f32) => {
        Some($crate::ValType::F32)
    };
    (f64) => {
        Some($crate::ValType::F64)
    };
    (v128) => {
        Some($crate::ValType::V128)
    };
    (funcref) => {
        Some($crate::ValType::FuncRef)
    };
    (externref) => {
        Some($crate::ValType::ExternRef)
    };
    ({$x:expr}) => {
        $x
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! mlt {
    (i32) => {
        $crate::ValType::I32
    };
    (i64) => {
        $crate::ValType::I64
    };
    (f32) => {
        $crate::ValType::F32
    };
    (f64) => {
        $crate::ValType::F64
    };
    (v128) => {
        $crate::ValType::V128
    };
    (funcref) => {
        $crate::ValType::FuncRef
    };
    (externref) => {
        $crate::ValType::ExternRef
    };
    ({$x:expr}) => {
        $x
    };
}

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

impl From<Vec<Instruction>> for Expr {
    fn from(instructions: Vec<Instruction>) -> Self {
        Self { instructions }
    }
}

impl From<Instruction> for Expr {
    fn from(instr: Instruction) -> Self {
        Self {
            instructions: vec![instr],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Control Instructions
    /// `unreachable`
    Unreachable,
    /// `nop`
    NoOp,
    /// `block`
    Block(Option<ValType>),
    /// `loop`
    Loop(Option<ValType>),
    /// `if`
    If(Option<ValType>),
    /// `else`
    Else,
    /// `end`
    End,
    /// `br`
    Branch(u32),
    /// `br_if`
    BranchIf(u32),
    /// `br_table`
    BranchTable {
        depths: Vec<u32>,
        failsafe: u32,
    },
    /// `return`
    Return,
    /// `call`
    Call(u32),
    /// `call_indirect`
    CallIndirect {
        type_idx: u32,
        table_idx: u32,
    },

    // Reference Instructions
    /// `ref.null`
    RefNull(RefType),
    /// `ref.is_null`
    RefIsNull,
    /// `ref.func`
    RefFunc(u32),

    // Parametric Instructions
    /// `drop`
    Drop,
    /// `select`
    Select,

    // Variable Instructions
    /// `local.get`
    LocalGet(u32),
    /// `local.set`
    LocalSet(u32),
    /// `local.tee`
    LocalTee(u32),
    /// `global.get`
    GlobalGet(u32),
    /// `global.set`
    GlobalSet(u32),

    // Table Instructions
    /// `table.get`
    TableGet(u32),
    /// `table.set`
    TableSet(u32),
    /// `table.size`
    TableSize(u32),
    /// `table.grow`
    TableGrow(u32),
    /// `table.fill`
    TableFill(u32),
    /// `table.copy`
    TableCopy {
        dest_index: u32,
        src_index: u32,
    },
    /// `table.init`
    TableInit {
        table_index: u32,
        elem_index: u32,
    },
    /// `elem.drop`
    ElemDrop(u32),

    // Memory Instructions
    /// `i32.load`
    I32Load {
        align: u32,
        offset: u32,
    },
    /// `i64.load`
    I64Load {
        align: u32,
        offset: u32,
    },
    /// `f32.load`
    F32Load {
        align: u32,
        offset: u32,
    },
    /// `f64.load`
    F64Load {
        align: u32,
        offset: u32,
    },

    /// `i32.load8_s`
    I32LoadS8 {
        align: u32,
        offset: u32,
    },
    /// `i32.load8_u`
    I32LoadU8 {
        align: u32,
        offset: u32,
    },
    /// `i32.load16_s`
    I32LoadS16 {
        align: u32,
        offset: u32,
    },
    /// `i32.load16_u`
    I32LoadU16 {
        align: u32,
        offset: u32,
    },

    /// `i64.load8_s`
    I64LoadS8 {
        align: u32,
        offset: u32,
    },
    /// `i64.load8_u`
    I64LoadU8 {
        align: u32,
        offset: u32,
    },
    /// `i64.load16_s`
    I64LoadS16 {
        align: u32,
        offset: u32,
    },
    /// `i64.load16_u`
    I64LoadU16 {
        align: u32,
        offset: u32,
    },
    /// `i64.load32_s`
    I64LoadS32 {
        align: u32,
        offset: u32,
    },
    /// `i64.load32_u`
    I64LoadU32 {
        align: u32,
        offset: u32,
    },

    /// `i32.store`
    I32Store {
        align: u32,
        offset: u32,
    },
    /// `i64.store`
    I64Store {
        align: u32,
        offset: u32,
    },
    /// `f32.store`
    F32Store {
        align: u32,
        offset: u32,
    },
    /// `f64.store`
    F64Store {
        align: u32,
        offset: u32,
    },

    /// `i32.store8`
    I32StoreI8 {
        align: u32,
        offset: u32,
    },
    /// `i32.store16`
    I32StoreI16 {
        align: u32,
        offset: u32,
    },
    /// `i64.store8`
    I64StoreI8 {
        align: u32,
        offset: u32,
    },
    /// `i64.store16`
    I64StoreI16 {
        align: u32,
        offset: u32,
    },
    /// `i64.store32`
    I64StoreI32 {
        align: u32,
        offset: u32,
    },

    /// `memory.size`
    MemorySize,
    /// `memory.grow`
    MemoryGrow,
    /// `memory.init`
    MemoryInit(u32),
    /// `data.drop`
    DataDrop(u32),
    /// `memory.copy`
    MemoryCopy,
    /// `memory.fill`
    MemoryFill,

    // Numeric Instructions
    /// `i32.const`
    I32Const(i32),
    /// `i64.const`
    I64Const(i64),
    /// `f32.const`
    F32Const(f32),
    /// `f64.const`
    F64Const(f64),

    /// `i32.eqz`
    I32EqualsZero,
    /// `i32.eq`
    I32Equal,
    /// `i32.ne`
    I32NotEqual,
    /// `i32.lt_s`
    S32LessThan,
    /// `i32.lt_u`
    U32LessThan,
    /// `i32.gt_s`
    S32GreaterThan,
    /// `i32.gt_u`
    U32GreaterThan,
    /// `i32.le_s`
    S32LessThanOrEqual,
    /// `i32.le_u`
    U32LessThanOrEqual,
    /// `i32.ge_s`
    S32GreaterThanOrEqual,
    /// `i32.ge_u`
    U32GreaterThanOrEqual,

    /// `i64.eqz`
    I64EqualsZero,
    /// `i64.eq`
    I64Equal,
    /// `i64.ne`
    I64NotEqual,
    /// `i64.lt_s`
    S64LessThan,
    /// `i64.lt_u`
    U64LessThan,
    /// `i64.gt_s`
    S64GreaterThan,
    /// `i64.gt_u`
    U64GreaterThan,
    /// `i64.le_s`
    S64LessThanOrEqual,
    /// `i64.le_u`
    U64LessThanOrEqual,
    /// `i64.ge_s`
    S64GreaterThanOrEqual,
    /// `i64.ge_u`
    U64GreaterThanOrEqual,

    /// `f32.eq`
    F32Equal,
    /// `f32.ne`
    F32NotEqual,
    /// `f32.lt`
    F32LessThan,
    /// `f32.gt`
    F32GreaterThan,
    /// `f32.le`
    F32LessThanOrEqual,
    /// `f32.ge`
    F32GreaterThanOrEqual,

    /// `f64.eq`
    F64Equal,
    /// `f64.ne`
    F64NotEqual,
    /// `f64.lt`
    F64LessThan,
    /// `f64.gt`
    F64GreaterThan,
    /// `f64.le`
    F64LessThanOrEqual,
    /// `f64.ge`
    F64GreaterThanOrEqual,

    /// `i32.clz`
    I32CountLeadingZeroes,
    /// `i32.ctz`
    I32CountTrailingZeroes,
    /// `i32.popcnt`
    I32CountOnes,
    /// `i32.add`
    I32Add,
    /// `i32.sub`
    I32Sub,
    /// `i32.mul`
    I32Mul,
    /// `i32.div_s`
    S32Div,
    /// `i32.div_u`
    U32Div,
    /// `i32.rem_s`
    S32Rem,
    /// `i32.rem_u`
    U32Rem,
    /// `i32.and`
    I32And,
    /// `i32.or`
    I32Or,
    /// `i32.xor`
    I32Xor,
    /// `i32.shl`
    I32ShiftLeft,
    /// `i32.shr_s`
    S32ShiftRight,
    /// `i32.shr_u`
    U32ShiftRight,
    /// `i32.rotl`
    I32RotateLeft,
    /// `i32.rotr`
    I32RotateRight,

    /// `i64.clz`
    I64CountLeadingZeroes,
    /// `i64.ctz`
    I64CountTrailingZeroes,
    /// `i64.popcnt`
    I64CountOnes,
    /// `i64.add`
    I64Add,
    /// `i64.sub`
    I64Sub,
    /// `i64.mul`
    I64Mul,
    /// `i64.div_s`
    S64Div,
    /// `i64.div_u`
    U64Div,
    /// `i64.rem_s`
    S64Rem,
    /// `i64.rem_u`
    U64Rem,
    /// `i64.and`
    I64And,
    /// `i64.or`
    I64Or,
    /// `i64.xor`
    I64Xor,
    /// `i64.shl`
    I64ShiftLeft,
    /// `i64.shr_s`
    S64ShiftRight,
    /// `i64.shr_u`
    U64ShiftRight,
    /// `i64.rotl`
    I64RotateLeft,
    /// `i64.rotr`
    I64RotateRight,

    /// `f32.abs`
    F32AbsoluteValue,
    /// `f32.neg`
    F32Negate,
    /// `f32.ceil`
    F32Ceiling,
    /// `f32.floor`
    F32Floor,
    /// `f32.trunc`
    F32Truncate,
    /// `f32.nearest`
    F32Nearest,
    /// `f32.sqrt`
    F32SquareRoot,
    /// `f32.add`
    F32Add,
    /// `f32.sub`
    F32Sub,
    /// `f32.mul`
    F32Mul,
    /// `f32.div`
    F32Div,
    /// `f32.min`
    F32Min,
    /// `f32.max`
    F32Max,
    /// `f32.copysign`
    F32CopySign,

    /// `f64.abs`
    F64AbsoluteValue,
    /// `f64.neg`
    F64Negate,
    /// `f64.ceil`
    F64Ceiling,
    /// `f64.floor`
    F64Floor,
    /// `f64.trunc`
    F64Truncate,
    /// `f64.nearest`
    F64Nearest,
    /// `f64.sqrt`
    F64SquareRoot,
    /// `f64.add`
    F64Add,
    /// `f64.sub`
    F64Sub,
    /// `f64.mul`
    F64Mul,
    /// `f64.div`
    F64Div,
    /// `f64.min`
    F64Min,
    /// `f64.max`
    F64Max,
    /// `f64.copysign`
    F64CopySign,

    /// `i32.wrap_i64`
    I32WrapI64,
    /// `i32.trunc_f32_s`
    S32TruncateF32,
    /// `i32.trunc_f32_u`
    U32TruncateF32,
    /// `i32.trunc_f64_s`
    S32TruncateF64,
    /// `i32.trunc_f64_u`
    U32TruncateF64,
    /// `i64.extend_i32_s`
    I64ExtendS32,
    /// `i64.extend_i32_u`
    I64ExtendU32,
    /// `i64.trunc_f32_s`
    S64TruncateF32,
    /// `i64.trunc_f32_u`
    U64TruncateF32,
    /// `i64.trunc_f64_s`
    S64TruncateF64,
    /// `i64.trunc_f64_u`
    U64TruncateF64,
    /// `f32.convert_i32_s`
    F32ConvertS32,
    /// `f32.convert_i32_u`
    F32ConvertU32,
    /// `f32.convert_i64_s`
    F32ConvertS64,
    /// `f32.convert_i64_u`
    F32ConvertU64,
    /// `f32.demote_f64`
    F32DemoteF64,
    /// `f64.convert_i32_s`
    F64ConvertS32,
    /// `f64.convert_i32_u`
    F64ConvertU32,
    /// `f64.convert_i64_s`
    F64ConvertS64,
    /// `f64.convert_i64_u`
    F64ConvertU64,
    /// `f64.promote_f32`
    F64PromoteF32,
    /// `i32.reinterpret_f32`
    I32ReinterpretF32,
    /// `i64.reinterpret_f64`
    I64ReinterpretF64,
    /// `f32.reinterpret_i32`
    F32ReinterpretI32,
    /// `f64.reinterpret_i64`
    F64ReinterpretI64,

    /// `i32.extend8_s`
    S32Extend8,
    /// `i32.extend16_s`
    S32Extend16,
    /// `i64.extend8_s`
    S64Extend8,
    /// `i64.extend16_s`
    S64Extend16,
    /// `i64.extend32_s`
    S64Extend32,

    /// `i32.trunc_sat_f32_s`
    S32SaturatingTruncateF32,
    /// `i32.trunc_sat_f32_u`
    U32SaturatingTruncateF32,
    /// `i32.trunc_sat_f64_s`
    S32SaturatingTruncateF64,
    /// `i32.trunc_sat_f64_u`
    U32SaturatingTruncateF64,
    /// `i64.trunc_sat_f32_s`
    S64SaturatingTruncateF32,
    /// `i64.trunc_sat_f32_u`
    U64SaturatingTruncateF32,
    /// `i64.trunc_sat_f64_s`
    S64SaturatingTruncateF64,
    /// `i64.trunc_sat_f64_u`
    U64SaturatingTruncateF64,

    V128Load {
        align: u32,
        offset: u32,
    },
    V128LoadS8x8 {
        align: u32,
        offset: u32,
    },
    V128LoadU8x8 {
        align: u32,
        offset: u32,
    },
    V128LoadS16x4 {
        align: u32,
        offset: u32,
    },
    V128LoadU16x4 {
        align: u32,
        offset: u32,
    },
    V128LoadS32x2 {
        align: u32,
        offset: u32,
    },
    V128LoadU32x2 {
        align: u32,
        offset: u32,
    },
    V128LoadSplatI8 {
        align: u32,
        offset: u32,
    },
    V128LoadSplatI16 {
        align: u32,
        offset: u32,
    },
    V128LoadSplatI32 {
        align: u32,
        offset: u32,
    },
    V128LoadSplatI64 {
        align: u32,
        offset: u32,
    },
    V128LoadZeroI32 {
        align: u32,
        offset: u32,
    },
    V128LoadZeroI64 {
        align: u32,
        offset: u32,
    },
    V128Store {
        align: u32,
        offset: u32,
    },
    V128Load8Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    V128Load16Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    V128Load32Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    V128Load64Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    V128Store8Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    V128Store16Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    V128Store32Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },
    V128Store64Lane {
        align: u32,
        offset: u32,
        lane: u8,
    },

    V128Const([u8; 16]),

    I8x16Shuffle {
        lanes: [u8; 16],
    },
    S8x16ExtractLane {
        lane: u8,
    },
    U8x16ExtractLane {
        lane: u8,
    },
    I8x16ReplaceLane {
        lane: u8,
    },
    S16x8ExtractLane {
        lane: u8,
    },
    U16x8ExtractLane {
        lane: u8,
    },
    I16x8ReplaceLane {
        lane: u8,
    },
    I32x4ExtractLane {
        lane: u8,
    },
    I32x4ReplaceLane {
        lane: u8,
    },
    I64x2ExtractLane {
        lane: u8,
    },
    I64x2ReplaceLane {
        lane: u8,
    },
    F32x4ExtractLane {
        lane: u8,
    },
    F32x4ReplaceLane {
        lane: u8,
    },
    F64x2ExtractLane {
        lane: u8,
    },
    F64x2ReplaceLane {
        lane: u8,
    },
    I8x16Swizzle,
    I8x16Splat,
    I16x8Splat,
    I32x4Splat,
    I64x2Splat,
    F32x4Splat,
    F64x2Splat,

    I8x16Equal,
    I8x16NotEqual,
    S8x16LessThan,
    U8x16LessThan,
    S8x16GreaterThan,
    U8x16GreaterThan,
    S8x16LessThanOrEqual,
    U8x16LessThanOrEqual,
    S8x16GreaterThanOrEqual,
    U8x16GreaterThanOrEqual,

    I16x8Equal,
    I16x8NotEqual,
    S16x8LessThan,
    U16x8LessThan,
    S16x8GreaterThan,
    U16x8GreaterThan,
    S16x8LessThanOrEqual,
    U16x8LessThanOrEqual,
    S16x8GreaterThanOrEqual,
    U16x8GreaterThanOrEqual,

    I32x4Equal,
    I32x4NotEqual,
    S32x4LessThan,
    U32x4LessThan,
    S32x4GreaterThan,
    U32x4GreaterThan,
    S32x4LessThanOrEqual,
    U32x4LessThanOrEqual,
    S32x4GreaterThanOrEqual,
    U32x4GreaterThanOrEqual,

    I64x2Equal,
    I64x2NotEqual,
    S64x2LessThan,
    S64x2GreaterThan,
    S64x2LessThanOrEqual,
    S64x2GreaterThanOrEqual,

    F32x4Equal,
    F32x4NotEqual,
    F32x4LessThan,
    F32x4GreaterThan,
    F32x4LessThanOrEqual,
    F32x4GreaterThanOrEqual,

    F64x2Equal,
    F64x2NotEqual,
    F64x2LessThan,
    F64x2GreaterThan,
    F64x2LessThanOrEqual,
    F64x2GreaterThanOrEqual,

    V128Not,
    V128And,
    V128AndNot,
    V128Or,
    V128Xor,
    V128BitSelect,
    V128AnyTrue,

    I8x16Abs,
    I8x16Neg,
    I8x16CountOnes,
    I8x16AllTrue,
    I8x16Bitmask,
    S8x16NarrowI16x8,
    U8x16NarrowI16x8,
    I8x16Shl,
    S8x16Shr,
    U8x16Shr,
    I8x16Add,
    S8x16AddSaturate,
    U8x16AddSaturate,
    I8x16Sub,
    S8x16SubSaturate,
    U8x16SubSaturate,
    S8x16Min,
    U8x16Min,
    S8x16Max,
    U8x16Max,
    U8x16Avgr,

    I16x8ExtendAddPairwiseS8x16,
    I16x8ExtendAddPairwiseU8x16,
    I16x8Abs,
    I16x8Neg,
    S16x8Q15MulRSat,
    I16x8AllTrue,
    I16x8Bitmask,
    S16x8NarrowI32x4,
    U16x8NarrowI32x4,
    I16x8ExtendLowS8x16,
    I16x8ExtendHighS8x16,
    I16x8ExtendLowU8x16,
    I16x8ExtendHighU8x16,
    I16x8Shl,
    S16x8Shr,
    U16x8Shr,
    I16x8Add,
    S16x8AddSaturate,
    U16x8AddSaturate,
    I16x8Sub,
    S16x8SubSaturate,
    U16x8SubSaturate,
    I16x8Mul,
    S16x8Min,
    U16x8Min,
    S16x8Max,
    U16x8Max,
    U16x8Avgr,
    I16x8ExtMulLowS8x16,
    I16x8ExtMulHighS8x16,
    I16x8ExtMulLowU8x16,
    I16x8ExtMulHighU8x16,

    I32x4ExtendAddPairwiseS16x8,
    I32x4ExtendAddPairwiseU16x8,
    I32x4Abs,
    I32x4Neg,
    I32x4AllTrue,
    I32x4Bitmask,
    I32x4ExtendLowS16x8,
    I32x4ExtendHighS16x8,
    I32x4ExtendLowU16x8,
    I32x4ExtendHighU16x8,
    I32x4Shl,
    S32x4Shr,
    U32x4Shr,
    I32x4Add,
    I32x4Sub,
    I32x4Mul,
    S32x4Min,
    U32x4Min,
    S32x4Max,
    U32x4Max,
    I32x4DotProductS16x8,
    I32x4ExtMulLowS16x8,
    I32x4ExtMulHighS16x8,
    I32x4ExtMulLowU16x8,
    I32x4ExtMulHighU16x8,

    I64x2Abs,
    I64x2Neg,
    I64x2AllTrue,
    I64x2Bitmask,
    I64x2ExtendLowS32x4,
    I64x2ExtendHighS32x4,
    I64x2ExtendLowU32x4,
    I64x2ExtendHighU32x4,
    I64x2Shl,
    S64x2Shr,
    U64x2Shr,
    I64x2Add,
    I64x2Sub,
    I64x2Mul,
    I64x2ExtMulLowS32x4,
    I64x2ExtMulHighS32x4,
    I64x2ExtMulLowU32x4,
    I64x2ExtMulHighU32x4,

    F32x4Ceil,
    F32x4Floor,
    F32x4Trunc,
    F32x4Nearest,
    F32x4Abs,
    F32x4Neg,
    F32x4Sqrt,
    F32x4Add,
    F32x4Sub,
    F32x4Mul,
    F32x4Div,
    F32x4Min,
    F32x4Max,
    F32x4PMin,
    F32x4PMax,

    F64x2Ceil,
    F64x2Floor,
    F64x2Trunc,
    F64x2Nearest,
    F64x2Abs,
    F64x2Neg,
    F64x2Sqrt,
    F64x2Add,
    F64x2Sub,
    F64x2Mul,
    F64x2Div,
    F64x2Min,
    F64x2Max,
    F64x2PMin,
    F64x2PMax,

    S32x4TruncSatF32x4,
    U32x4TruncSatF32x4,
    F32x4ConvertS32x4,
    F32x4ConvertU32x4,
    S32x4TruncSatZeroF64x2,
    U32x4TruncSatZeroF64x2,
    F64x2ConvertLowS32x4,
    F64x2ConvertLowU32x4,
    F32x4DemoteF64x2Zero,
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
            TableSize(_) => Extended(0xFCu8, 12u32),
            TableGrow(_) => Extended(0xFCu8, 13u32),
            TableFill(_) => Extended(0xFCu8, 14u32),
            TableCopy {
                dest_index: _,
                src_index: _,
            } => Extended(0xFCu8, 15u32),
            TableInit {
                table_index: _,
                elem_index: _,
            } => Extended(0xFCu8, 16u32),
            ElemDrop(_) => Extended(0xFCu8, 14u32),
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
            I64x2ExtMulLowU32x4 => Extended(0xFD, 223),
            I64x2ExtMulHighU32x4 => Extended(0xFD, 224),
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

enum InstrId {
    Single(u8),
    Extended(u8, u32),
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
