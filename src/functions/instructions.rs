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
        Self { instructions: vec![instr] }
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

    I8x16Eq,
    I8x16Ne,
    S8x16Lt,
    U8x16Lt,
    S8x16Gt,
    U8x16Gt,
    S8x16Le,
    U8x16Le,
    S8x16Ge,
    U8x16Ge,

    I16x8Eq,
    I16x8Ne,
    S16x8Lt,
    U16x8Lt,
    S16x8Gt,
    U16x8Gt,
    S16x8Le,
    U16x8Le,
    S16x8Ge,
    U16x8Ge,

    I32x4Eq,
    I32x4Ne,
    S32x4Lt,
    U32x4Lt,
    S32x4Gt,
    U32x4Gt,
    S32x4Le,
    U32x4Le,
    S32x4Ge,
    U32x4Ge,

    I64x2Eq,
    I64x2Ne,
    S64x2Lt,
    S64x2Gt,
    S64x2Le,
    S64x2Ge,

    F32x4Eq,
    F32x4Ne,
    F32x4Lt,
    F32x4Gt,
    F32x4Le,
    F32x4Ge,

    F64x2Eq,
    F64x2Ne,
    F64x2Lt,
    F64x2Gt,
    F64x2Le,
    F64x2Ge,

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

impl WasmEncode for Instruction {
    fn size(&self) -> usize {
        use Instruction::*;
        match *self {
            Unreachable => 1,
            NoOp => 1,
            Block(_) => 2,
            Loop(_) => 2,
            If(_) => 2,
            Else => 1,
            End => 1,
            Branch(depth) => 1 + depth.size(),
            BranchIf(depth) => 1 + depth.size(),
            BranchTable { ref depths, failsafe } => 1 + depths.size() + failsafe.size(),
            Return => 1,
            Call(func_idx) => 1 + func_idx.size(),
            CallIndirect { type_idx, table_idx } => 1 + type_idx.size() + table_idx.size(),
            RefNull(ref_type) => 1 + ref_type.size(),
            RefIsNull => 1,
            RefFunc(func_idx) => 1 + func_idx.size(),
            Drop => 1,
            Select => 1, //fixme
            LocalGet(local_index) |
            LocalSet(local_index) |
            LocalTee(local_index) => 1 + local_index.size(),
            GlobalGet(global_index) |
            GlobalSet(global_index) => 1 + global_index.size(),
            TableGet(table_index) |
            TableSet(table_index) => 1 + table_index.size(),
            TableSize(table_index) |
            TableGrow(table_index) |
            TableFill(table_index) => 2 + table_index.size(),
            TableCopy { dest_index, src_index } => 2 + dest_index.size() + src_index.size(),
            TableInit { table_index, elem_index } => 2 + table_index.size() + elem_index.size(),
            ElemDrop(elem_index) => 2 + elem_index.size(),
            I32Load { align, offset } |
            I64Load { align, offset } |
            F32Load { align, offset } |
            F64Load { align, offset } |
            I32LoadS8 { align, offset } |
            I32LoadU8 { align, offset } |
            I32LoadS16 { align, offset } |
            I32LoadU16 { align, offset } |
            I64LoadS8 { align, offset } |
            I64LoadU8 { align, offset } |
            I64LoadS16 { align, offset } |
            I64LoadU16 { align, offset } |
            I64LoadS32 { align, offset } |
            I64LoadU32 { align, offset } |
            I32Store { align, offset } |
            I64Store { align, offset } |
            F32Store { align, offset } |
            F64Store { align, offset } |
            I32StoreI8 { align, offset } |
            I32StoreI16 { align, offset } |
            I64StoreI8 { align, offset } |
            I64StoreI16 { align, offset } |
            I64StoreI32 { align, offset } => 1 + align.size() + offset.size(),
            MemorySize => 2,
            MemoryGrow => 2,
            MemoryInit(data_idx) => 3 + data_idx.size(),
            DataDrop(data_idx) => 2 + data_idx.size(),
            MemoryCopy => 4,
            MemoryFill => 3,
            I32Const(x) => 1 + x.size(),
            I64Const(x) => 1 + x.size(),
            F32Const(x) => 1 + x.size(),
            F64Const(x) => 1 + x.size(),
            I32EqualsZero |
            I32Equal |
            I32NotEqual |
            S32LessThan |
            U32LessThan |
            S32GreaterThan |
            U32GreaterThan |
            S32LessThanOrEqual |
            U32LessThanOrEqual |
            S32GreaterThanOrEqual |
            U32GreaterThanOrEqual |
            I64EqualsZero |
            I64Equal |
            I64NotEqual |
            S64LessThan |
            U64LessThan |
            S64GreaterThan |
            U64GreaterThan |
            S64LessThanOrEqual |
            U64LessThanOrEqual |
            S64GreaterThanOrEqual |
            U64GreaterThanOrEqual |
            F32Equal |
            F32NotEqual |
            F32LessThan |
            F32GreaterThan |
            F32LessThanOrEqual |
            F32GreaterThanOrEqual |
            F64Equal |
            F64NotEqual |
            F64LessThan |
            F64GreaterThan |
            F64LessThanOrEqual |
            F64GreaterThanOrEqual |
            I32CountLeadingZeroes |
            I32CountTrailingZeroes |
            I32CountOnes |
            I32Add |
            I32Sub |
            I32Mul |
            S32Div |
            U32Div |
            S32Rem |
            U32Rem |
            I32And |
            I32Or |
            I32Xor |
            I32ShiftLeft |
            S32ShiftRight |
            U32ShiftRight |
            I32RotateLeft |
            I32RotateRight |
            I64CountLeadingZeroes |
            I64CountTrailingZeroes |
            I64CountOnes |
            I64Add |
            I64Sub |
            I64Mul |
            S64Div |
            U64Div |
            S64Rem |
            U64Rem |
            I64And |
            I64Or |
            I64Xor |
            I64ShiftLeft |
            S64ShiftRight |
            U64ShiftRight |
            I64RotateLeft |
            I64RotateRight |
            F32AbsoluteValue |
            F32Negate |
            F32Ceiling |
            F32Floor |
            F32Truncate |
            F32Nearest |
            F32SquareRoot |
            F32Add |
            F32Sub |
            F32Mul |
            F32Div |
            F32Min |
            F32Max |
            F32CopySign |
            F64AbsoluteValue |
            F64Negate |
            F64Ceiling |
            F64Floor |
            F64Truncate |
            F64Nearest |
            F64SquareRoot |
            F64Add |
            F64Sub |
            F64Mul |
            F64Div |
            F64Min |
            F64Max |
            F64CopySign |
            I32WrapI64 |
            S32TruncateF32 |
            U32TruncateF32 |
            S32TruncateF64 |
            U32TruncateF64 |
            I64ExtendS32 |
            I64ExtendU32 |
            S64TruncateF32 |
            U64TruncateF32 |
            S64TruncateF64 |
            U64TruncateF64 |
            F32ConvertS32 |
            F32ConvertU32 |
            F32ConvertS64 |
            F32ConvertU64 |
            F32DemoteF64 |
            F64ConvertS32 |
            F64ConvertU32 |
            F64ConvertS64 |
            F64ConvertU64 |
            F64PromoteF32 |
            I32ReinterpretF32 |
            I64ReinterpretF64 |
            F32ReinterpretI32 |
            F64ReinterpretI64 |
            S32Extend8 |
            S32Extend16 |
            S64Extend8 |
            S64Extend16 |
            S64Extend32 => 1,
            S32SaturatingTruncateF32 |
            U32SaturatingTruncateF32 |
            S32SaturatingTruncateF64 |
            U32SaturatingTruncateF64 |
            S64SaturatingTruncateF32 |
            U64SaturatingTruncateF32 |
            S64SaturatingTruncateF64 |
            U64SaturatingTruncateF64 => 2,

            V128Load { align, offset } => 2 + align.size() + offset.size(),
            V128LoadS8x8 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadU8x8 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadS16x4 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadU16x4 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadS32x2 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadU32x2 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadSplatI8 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadSplatI16 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadSplatI32 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadSplatI64 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadZeroI32 { align, offset } => 2 + align.size() + offset.size(),
            V128LoadZeroI64 { align, offset } => 2 + align.size() + offset.size(),
            V128Store { align, offset } => 2 + align.size() + offset.size(),
            V128Load8Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Load16Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Load32Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Load64Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Store8Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Store16Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Store32Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Store64Lane { align, offset, lane: _ } => 3 + align.size() + offset.size(),
            V128Const(_) => 18,
            I8x16Shuffle { lanes: _ } => 18,
            S8x16ExtractLane { lane: _ } |
            U8x16ExtractLane { lane: _ } |
            I8x16ReplaceLane { lane: _ } |
            S16x8ExtractLane { lane: _ } |
            U16x8ExtractLane { lane: _ } |
            I16x8ReplaceLane { lane: _ } |
            I32x4ExtractLane { lane: _ } |
            I32x4ReplaceLane { lane: _ } |
            I64x2ExtractLane { lane: _ } |
            I64x2ReplaceLane { lane: _ } |
            F32x4ExtractLane { lane: _ } |
            F32x4ReplaceLane { lane: _ } |
            F64x2ExtractLane { lane: _ } |
            F64x2ReplaceLane { lane: _ } => 3,
            I8x16Swizzle |
            I8x16Splat |
            I16x8Splat |
            I32x4Splat |
            I64x2Splat |
            F32x4Splat |
            F64x2Splat |
            I8x16Eq |
            I8x16Ne |
            S8x16Lt |
            U8x16Lt |
            S8x16Gt |
            U8x16Gt |
            S8x16Le |
            U8x16Le |
            S8x16Ge |
            U8x16Ge |
            I16x8Eq |
            I16x8Ne |
            S16x8Lt |
            U16x8Lt |
            S16x8Gt |
            U16x8Gt |
            S16x8Le |
            U16x8Le |
            S16x8Ge |
            U16x8Ge |
            I32x4Eq |
            I32x4Ne |
            S32x4Lt |
            U32x4Lt |
            S32x4Gt |
            U32x4Gt |
            S32x4Le |
            U32x4Le |
            S32x4Ge |
            U32x4Ge |
            I64x2Eq |
            I64x2Ne |
            S64x2Lt |
            S64x2Gt |
            S64x2Le |
            S64x2Ge |
            F32x4Eq |
            F32x4Ne |
            F32x4Lt |
            F32x4Gt |
            F32x4Le |
            F32x4Ge |
            F64x2Eq |
            F64x2Ne |
            F64x2Lt |
            F64x2Gt |
            F64x2Le |
            F64x2Ge |
            V128Not |
            V128And |
            V128AndNot |
            V128Or |
            V128Xor |
            V128BitSelect |
            V128AnyTrue |
            I8x16Abs |
            I8x16Neg |
            I8x16CountOnes |
            I8x16AllTrue |
            I8x16Bitmask |
            S8x16NarrowI16x8 |
            U8x16NarrowI16x8 |
            I8x16Shl |
            S8x16Shr |
            U8x16Shr |
            I8x16Add |
            S8x16AddSaturate |
            U8x16AddSaturate |
            I8x16Sub |
            S8x16SubSaturate |
            U8x16SubSaturate |
            S8x16Min |
            U8x16Min |
            S8x16Max |
            U8x16Max |
            U8x16Avgr |
            I16x8ExtendAddPairwiseS8x16 |
            I16x8ExtendAddPairwiseU8x16 |
            I16x8Abs |
            I16x8Neg |
            S16x8Q15MulRSat |
            I16x8AllTrue |
            I16x8Bitmask |
            S16x8NarrowI32x4 |
            U16x8NarrowI32x4 |
            I16x8ExtendLowS8x16 |
            I16x8ExtendHighS8x16 |
            I16x8ExtendLowU8x16 |
            I16x8ExtendHighU8x16 |
            I16x8Shl |
            S16x8Shr |
            U16x8Shr |
            I16x8Add |
            S16x8AddSaturate |
            U16x8AddSaturate |
            I16x8Sub  |
            S16x8SubSaturate  |
            U16x8SubSaturate  |
            I16x8Mul  |
            S16x8Min  |
            U16x8Min  |
            S16x8Max  |
            U16x8Max  |
            U16x8Avgr  |
            I16x8ExtMulLowS8x16  |
            I16x8ExtMulHighS8x16  |
            I16x8ExtMulLowU8x16  |
            I16x8ExtMulHighU8x16  |
            I32x4ExtendAddPairwiseS16x8  |
            I32x4ExtendAddPairwiseU16x8  |
            I32x4Abs  |
            I32x4Neg  |
            I32x4AllTrue  |
            I32x4Bitmask  |
            I32x4ExtendLowS16x8  |
            I32x4ExtendHighS16x8  |
            I32x4ExtendLowU16x8  |
            I32x4ExtendHighU16x8  |
            I32x4Shl  |
            S32x4Shr  |
            U32x4Shr  |
            I32x4Add  |
            I32x4Sub  |
            I32x4Mul  |
            S32x4Min  |
            U32x4Min  |
            S32x4Max  |
            U32x4Max  |
            I32x4DotProductS16x8  |
            I32x4ExtMulLowS16x8  |
            I32x4ExtMulHighS16x8  |
            I32x4ExtMulLowU16x8  |
            I32x4ExtMulHighU16x8  |
            I64x2Abs  |
            I64x2Neg  |
            I64x2AllTrue  |
            I64x2Bitmask  |
            I64x2ExtendLowS32x4  |
            I64x2ExtendHighS32x4  |
            I64x2ExtendLowU32x4  |
            I64x2ExtendHighU32x4  |
            I64x2Shl  |
            S64x2Shr  |
            U64x2Shr  |
            I64x2Add  |
            I64x2Sub  |
            I64x2Mul  |
            I64x2ExtMulLowS32x4  |
            I64x2ExtMulHighS32x4  |
            I64x2ExtMulLowU32x4  |
            I64x2ExtMulHighU32x4  |
            F32x4Ceil  |
            F32x4Floor  |
            F32x4Trunc  |
            F32x4Nearest  |
            F32x4Abs  |
            F32x4Neg  |
            F32x4Sqrt  |
            F32x4Add  |
            F32x4Sub  |
            F32x4Mul  |
            F32x4Div  |
            F32x4Min  |
            F32x4Max |
            F32x4PMin |
            F32x4PMax |
            F64x2Ceil |
            F64x2Floor |
            F64x2Trunc |
            F64x2Nearest |
            F64x2Abs |
            F64x2Neg |
            F64x2Sqrt |
            F64x2Add |
            F64x2Sub |
            F64x2Mul |
            F64x2Div |
            F64x2Min |
            F64x2Max |
            F64x2PMin |
            F64x2PMax |
            S32x4TruncSatF32x4 |
            U32x4TruncSatF32x4 |
            F32x4ConvertS32x4 |
            F32x4ConvertU32x4 |
            S32x4TruncSatZeroF64x2 |
            U32x4TruncSatZeroF64x2 |
            F64x2ConvertLowS32x4 |
            F64x2ConvertLowU32x4 |
            F32x4DemoteF64x2Zero |
            F64x2PromoteLowF32x4 => 2,

            
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        use Instruction::*;
        match *self {
            Unreachable => v.push(0),
            NoOp => v.push(1),
            Block(kind) => {
                v.push(2);
                kind.encode(v);
            },
            Loop(kind) => {
                v.push(3);
                kind.encode(v);
            },
            If(kind) => {
                v.push(4);
                kind.encode(v);
            },
            Else => v.push(5),
            End => v.push(0xB),
            Branch(depth) => {
                v.push(0xC);
                depth.encode(v);
            },
            BranchIf(depth) => {
                v.push(0xD);
                depth.encode(v);
            },
            BranchTable { ref depths, failsafe } => {
                v.push(0xE);
                for depth in depths {
                    depth.encode(v);
                }
                failsafe.encode(v);
            },
            Return => v.push(0xF),
            Call(func_idx) => {
                v.push(0x10);
                func_idx.encode(v);
            },
            CallIndirect { type_idx, table_idx } => {
                v.push(0x11);
                type_idx.encode(v);
                table_idx.encode(v);
            },
            RefNull(ref_type) => {
                v.push(0xD0);
                ref_type.encode(v);
            },
            RefIsNull => v.push(0xD1),
            RefFunc(func_index) => {
                v.push(0xD2);
                func_index.encode(v);
            },
            Drop => v.push(0x1A),
            Select => v.push(0x1B),
            LocalGet(local_index) => {
                v.push(0x20);
                local_index.encode(v);
            },
            LocalSet(local_index) => {
                v.push(0x21);
                local_index.encode(v);
            },
            LocalTee(local_index) => {
                v.push(0x22);
                local_index.encode(v);
            },
            GlobalGet(global_index) => {
                v.push(0x23);
                global_index.encode(v);
            },
            GlobalSet(global_index) => {
                v.push(0x24);
                global_index.encode(v);
            },
            TableGet(table_index) => {
                v.push(0x25);
                table_index.encode(v);
            },
            TableSet(table_index) => {
                v.push(0x26);
                table_index.encode(v);
            },
            TableSize(table_index) => {
                [0xFCu8, 12].encode(v);
                table_index.encode(v);
            },
            TableGrow(table_index) => {
                [0xFCu8, 13].encode(v);
                table_index.encode(v);
            },
            TableFill(table_index) => {
                [0xFCu8, 14].encode(v);
                table_index.encode(v);
            },
            TableCopy { dest_index, src_index } => {
                [0xFCu8, 15].encode(v);
                dest_index.encode(v);
                src_index.encode(v);
            },
            TableInit { table_index, elem_index } => {
                [0xFCu8, 16].encode(v);
                table_index.encode(v);
                elem_index.encode(v);
            },
            ElemDrop(elem_index) => {
                [0xFCu8, 14].encode(v);
                elem_index.encode(v);
            },
            I32Load { align, offset } => {
                v.push(0x28);
                align.encode(v);
                offset.encode(v);
            },
            I64Load { align, offset } => {
                v.push(0x29);
                align.encode(v);
                offset.encode(v);
            },
            F32Load { align, offset } => {
                v.push(0x2A);
                align.encode(v);
                offset.encode(v);
            },
            F64Load { align, offset } => {
                v.push(0x2B);
                align.encode(v);
                offset.encode(v);
            },
            I32LoadS8 { align, offset } => {
                v.push(0x2C);
                align.encode(v);
                offset.encode(v);
            },
            I32LoadU8 { align, offset } => {
                v.push(0x2D);
                align.encode(v);
                offset.encode(v);
            },
            I32LoadS16 { align, offset } => {
                v.push(0x2E);
                align.encode(v);
                offset.encode(v);
            },
            I32LoadU16 { align, offset } => {
                v.push(0x2F);
                align.encode(v);
                offset.encode(v);
            },
            I64LoadS8 { align, offset } => {
                v.push(0x30);
                align.encode(v);
                offset.encode(v);
            },
            I64LoadU8 { align, offset } => {
                v.push(0x31);
                align.encode(v);
                offset.encode(v);
            },
            I64LoadS16 { align, offset } => {
                v.push(0x32);
                align.encode(v);
                offset.encode(v);
            },
            I64LoadU16 { align, offset } => {
                v.push(0x33);
                align.encode(v);
                offset.encode(v);
            },
            I64LoadS32 { align, offset } => {
                v.push(0x34);
                align.encode(v);
                offset.encode(v);
            },
            I64LoadU32 { align, offset } => {
                v.push(0x35);
                align.encode(v);
                offset.encode(v);
            },
            I32Store { align, offset } => {
                v.push(0x36);
                align.encode(v);
                offset.encode(v);
            },
            I64Store { align, offset } => {
                v.push(0x37);
                align.encode(v);
                offset.encode(v);
            },
            F32Store { align, offset } => {
                v.push(0x38);
                align.encode(v);
                offset.encode(v);
            },
            F64Store { align, offset } => {
                v.push(0x39);
                align.encode(v);
                offset.encode(v);
            },
            I32StoreI8 { align, offset } => {
                v.push(0x3A);
                align.encode(v);
                offset.encode(v);
            },
            I32StoreI16 { align, offset } => {
                v.push(0x3B);
                align.encode(v);
                offset.encode(v);
            },
            I64StoreI8 { align, offset } => {
                v.push(0x3C);
                align.encode(v);
                offset.encode(v);
            },
            I64StoreI16 { align, offset } => {
                v.push(0x3D);
                align.encode(v);
                offset.encode(v);
            },
            I64StoreI32 { align, offset } => {
                v.push(0x3E);
                align.encode(v);
                offset.encode(v);
            },
            MemorySize => [0x3Fu8, 0].encode(v),
            MemoryGrow => [0x40u8, 0].encode(v),
            MemoryInit(data_idx) => {
                [0xFCu8, 8].encode(v);
                data_idx.encode(v);
                v.push(0);
            },
            DataDrop(data_idx) => {
                [0x40u8, 9].encode(v);
                data_idx.encode(v);
            },
            MemoryCopy => [0xFCu8, 10, 0, 0].encode(v),
            MemoryFill => [0xFCu8, 11, 0].encode(v),
            I32Const(x) => {
                v.push(0x41);
                x.encode(v);
            },
            I64Const(x) => {
                v.push(0x42);
                x.encode(v);
            },
            F32Const(x) => {
                v.push(0x43);
                x.encode(v);
            },
            F64Const(x) => {
                v.push(0x44);
                x.encode(v);
            },
            I32EqualsZero => v.push(0x45),
            I32Equal => v.push(0x46),
            I32NotEqual => v.push(0x47),
            S32LessThan => v.push(0x48),
            U32LessThan => v.push(0x49),
            S32GreaterThan => v.push(0x4A),
            U32GreaterThan => v.push(0x4B),
            S32LessThanOrEqual => v.push(0x4C),
            U32LessThanOrEqual => v.push(0x4D),
            S32GreaterThanOrEqual => v.push(0x4E),
            U32GreaterThanOrEqual => v.push(0x4F),
            I64EqualsZero => v.push(0x50),
            I64Equal => v.push(0x51),
            I64NotEqual => v.push(0x52),
            S64LessThan => v.push(0x53),
            U64LessThan => v.push(0x54),
            S64GreaterThan => v.push(0x55),
            U64GreaterThan => v.push(0x56),
            S64LessThanOrEqual => v.push(0x57),
            U64LessThanOrEqual => v.push(0x58),
            S64GreaterThanOrEqual => v.push(0x59),
            U64GreaterThanOrEqual => v.push(0x5A),
            F32Equal => v.push(0x5B),
            F32NotEqual => v.push(0x5C),
            F32LessThan => v.push(0x5D),
            F32GreaterThan => v.push(0x5E),
            F32LessThanOrEqual => v.push(0x5F),
            F32GreaterThanOrEqual => v.push(0x60),
            F64Equal => v.push(0x61),
            F64NotEqual => v.push(0x62),
            F64LessThan => v.push(0x63),
            F64GreaterThan => v.push(0x64),
            F64LessThanOrEqual => v.push(0x65),
            F64GreaterThanOrEqual => v.push(0x66),
            I32CountLeadingZeroes => v.push(0x67),
            I32CountTrailingZeroes => v.push(0x68),
            I32CountOnes => v.push(0x69),
            I32Add => v.push(0x6A),
            I32Sub => v.push(0x6B),
            I32Mul => v.push(0x6C),
            S32Div => v.push(0x6D),
            U32Div => v.push(0x6E),
            S32Rem => v.push(0x6F),
            U32Rem => v.push(0x70),
            I32And => v.push(0x71),
            I32Or => v.push(0x72),
            I32Xor => v.push(0x73),
            I32ShiftLeft => v.push(0x74),
            S32ShiftRight => v.push(0x75),
            U32ShiftRight => v.push(0x76),
            I32RotateLeft => v.push(0x77),
            I32RotateRight => v.push(0x78),
            I64CountLeadingZeroes => v.push(0x79),
            I64CountTrailingZeroes => v.push(0x7A),
            I64CountOnes => v.push(0x7B),
            I64Add => v.push(0x7C),
            I64Sub => v.push(0x7D),
            I64Mul => v.push(0x7E),
            S64Div => v.push(0x7F),
            U64Div => v.push(0x80),
            S64Rem => v.push(0x81),
            U64Rem => v.push(0x82),
            I64And => v.push(0x83),
            I64Or => v.push(0x84),
            I64Xor => v.push(0x85),
            I64ShiftLeft => v.push(0x86),
            S64ShiftRight => v.push(0x87),
            U64ShiftRight => v.push(0x88),
            I64RotateLeft => v.push(0x89),
            I64RotateRight => v.push(0x8A),
            F32AbsoluteValue => v.push(0x8B),
            F32Negate => v.push(0x8C),
            F32Ceiling => v.push(0x8D),
            F32Floor => v.push(0x8E),
            F32Truncate => v.push(0x8F),
            F32Nearest => v.push(0x90),
            F32SquareRoot => v.push(0x91),
            F32Add => v.push(0x92),
            F32Sub => v.push(0x93),
            F32Mul => v.push(0x94),
            F32Div => v.push(0x95),
            F32Min => v.push(0x96),
            F32Max => v.push(0x97),
            F32CopySign => v.push(0x98),
            F64AbsoluteValue => v.push(0x99),
            F64Negate => v.push(0x9A),
            F64Ceiling => v.push(0x9B),
            F64Floor => v.push(0x9C),
            F64Truncate => v.push(0x9D),
            F64Nearest => v.push(0x9E),
            F64SquareRoot => v.push(0x9F),
            F64Add => v.push(0xA0),
            F64Sub => v.push(0xA1),
            F64Mul => v.push(0xA2),
            F64Div => v.push(0xA3),
            F64Min => v.push(0xA4),
            F64Max => v.push(0xA5),
            F64CopySign => v.push(0xA6),
            I32WrapI64 => v.push(0xA7),
            S32TruncateF32 => v.push(0xA8),
            U32TruncateF32 => v.push(0xA9),
            S32TruncateF64 => v.push(0xAA),
            U32TruncateF64 => v.push(0xAB),
            I64ExtendS32 => v.push(0xAC),
            I64ExtendU32 => v.push(0xAD),
            S64TruncateF32 => v.push(0xAE),
            U64TruncateF32 => v.push(0xAF),
            S64TruncateF64 => v.push(0xB0),
            U64TruncateF64 => v.push(0xB1),
            F32ConvertS32 => v.push(0xB2),
            F32ConvertU32 => v.push(0xB3),
            F32ConvertS64 => v.push(0xB4),
            F32ConvertU64 => v.push(0xB5),
            F32DemoteF64 => v.push(0xB6),
            F64ConvertS32 => v.push(0xB7),
            F64ConvertU32 => v.push(0xB8),
            F64ConvertS64 => v.push(0xB9),
            F64ConvertU64 => v.push(0xBA),
            F64PromoteF32 => v.push(0xBB),
            I32ReinterpretF32 => v.push(0xBC),
            I64ReinterpretF64 => v.push(0xBD),
            F32ReinterpretI32 => v.push(0xBE),
            F64ReinterpretI64 => v.push(0xBF),
            S32Extend8 => v.push(0xC0),
            S32Extend16 => v.push(0xC1),
            S64Extend8 => v.push(0xC2),
            S64Extend16 => v.push(0xC3),
            S64Extend32 => v.push(0xC4),
            S32SaturatingTruncateF32 => [0xFCu8, 0].encode(v),
            U32SaturatingTruncateF32 => [0xFCu8, 1].encode(v),
            S32SaturatingTruncateF64 => [0xFCu8, 2].encode(v),
            U32SaturatingTruncateF64 => [0xFCu8, 3].encode(v),
            S64SaturatingTruncateF32 => [0xFCu8, 4].encode(v),
            U64SaturatingTruncateF32 => [0xFCu8, 5].encode(v),
            S64SaturatingTruncateF64 => [0xFCu8, 6].encode(v),
            U64SaturatingTruncateF64 => [0xFCu8, 7].encode(v),
            V128Load { align, offset } => {
                [0xFDu8, 0].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadS8x8 { align, offset } => {
                [0xFDu8, 1].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadU8x8 { align, offset } => {
                [0xFDu8, 2].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadS16x4 { align, offset } => {
                [0xFDu8, 3].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadU16x4 { align, offset } => {
                [0xFDu8, 4].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadS32x2 { align, offset } => {
                [0xFDu8, 5].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadU32x2 { align, offset } => {
                [0xFDu8, 6].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadSplatI8 { align, offset } => {
                [0xFDu8, 7].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadSplatI16 { align, offset } => {
                [0xFDu8, 8].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadSplatI32 { align, offset } => {
                [0xFDu8, 9].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadSplatI64 { align, offset } => {
                [0xFDu8, 10].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadZeroI32 { align, offset } => {
                [0xFDu8, 92].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128LoadZeroI64 { align, offset } => {
                [0xFDu8, 93].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128Store { align, offset } => {
                [0xFDu8, 11].encode(v);
                align.encode(v);
                offset.encode(v);
            },
            V128Load8Lane { align, offset, lane } => {
                [0xFDu8, 84].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Load16Lane { align, offset, lane } => {
                [0xFDu8, 85].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Load32Lane { align, offset, lane } => {
                [0xFDu8, 86].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Load64Lane { align, offset, lane } => {
                [0xFDu8, 87].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Store8Lane { align, offset, lane } => {
                [0xFDu8, 88].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Store16Lane { align, offset, lane } => {
                [0xFDu8, 89].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Store32Lane { align, offset, lane } => {
                [0xFDu8, 90].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Store64Lane { align, offset, lane } => {
                [0xFDu8, 91].encode(v);
                align.encode(v);
                offset.encode(v);
                lane.encode(v);
            },
            V128Const(bytes) => {
                [0xFDu8, 12].encode(v);
                bytes.encode(v);
            },
            I8x16Shuffle { lanes } => {
                [0xFDu8, 13].encode(v);
                lanes.encode(v);
            },
            S8x16ExtractLane { lane } => {
                [0xFDu8, 21, lane].encode(v);
            },
            U8x16ExtractLane { lane } => {
                [0xFDu8, 22, lane].encode(v);
            },
            I8x16ReplaceLane { lane } => {
                [0xFDu8, 23, lane].encode(v);
            },
            S16x8ExtractLane { lane } => {
                [0xFDu8, 24, lane].encode(v);
            },
            U16x8ExtractLane { lane } => {
                [0xFDu8, 25, lane].encode(v);
            },
            I16x8ReplaceLane { lane } => {
                [0xFDu8, 26, lane].encode(v);
            },
            I32x4ExtractLane { lane } => {
                [0xFDu8, 27, lane].encode(v);
            },
            I32x4ReplaceLane { lane } => {
                [0xFDu8, 28, lane].encode(v);
            },
            I64x2ExtractLane { lane } => {
                [0xFDu8, 29, lane].encode(v);
            },
            I64x2ReplaceLane { lane } => {
                [0xFDu8, 30, lane].encode(v);
            },
            F32x4ExtractLane { lane } => {
                [0xFDu8, 31, lane].encode(v);
            },
            F32x4ReplaceLane { lane } => {
                [0xFDu8, 32, lane].encode(v);
            },
            F64x2ExtractLane { lane } => {
                [0xFDu8, 33, lane].encode(v);
            },
            F64x2ReplaceLane { lane } => {
                [0xFDu8, 34, lane].encode(v);
            },
            I8x16Swizzle => [0xFDu8, 14].encode(v),
            I8x16Splat => [0xFDu8, 15].encode(v),
            I16x8Splat => [0xFDu8, 16].encode(v),
            I32x4Splat => [0xFDu8, 17].encode(v),
            I64x2Splat => [0xFDu8, 18].encode(v),
            F32x4Splat => [0xFDu8, 19].encode(v),
            F64x2Splat => [0xFDu8, 20].encode(v),
            I8x16Eq => [0xFDu8, 35].encode(v),
            I8x16Ne => [0xFDu8, 36].encode(v),
            S8x16Lt => [0xFDu8, 37].encode(v),
            U8x16Lt => [0xFDu8, 38].encode(v),
            S8x16Gt => [0xFDu8, 39].encode(v),
            U8x16Gt => [0xFDu8, 40].encode(v),
            S8x16Le => [0xFDu8, 41].encode(v),
            U8x16Le => [0xFDu8, 42].encode(v),
            S8x16Ge => [0xFDu8, 43].encode(v),
            U8x16Ge => [0xFDu8, 44].encode(v),
            I16x8Eq => [0xFDu8, 45].encode(v),
            I16x8Ne => [0xFDu8, 46].encode(v),
            S16x8Lt => [0xFDu8, 47].encode(v),
            U16x8Lt => [0xFDu8, 48].encode(v),
            S16x8Gt => [0xFDu8, 49].encode(v),
            U16x8Gt => [0xFDu8, 50].encode(v),
            S16x8Le => [0xFDu8, 51].encode(v),
            U16x8Le => [0xFDu8, 52].encode(v),
            S16x8Ge => [0xFDu8, 53].encode(v),
            U16x8Ge => [0xFDu8, 54].encode(v),
            I32x4Eq => [0xFDu8, 55].encode(v),
            I32x4Ne => [0xFDu8, 56].encode(v),
            S32x4Lt => [0xFDu8, 57].encode(v),
            U32x4Lt => [0xFDu8, 58].encode(v),
            S32x4Gt => [0xFDu8, 59].encode(v),
            U32x4Gt => [0xFDu8, 60].encode(v),
            S32x4Le => [0xFDu8, 61].encode(v),
            U32x4Le => [0xFDu8, 62].encode(v),
            S32x4Ge => [0xFDu8, 63].encode(v),
            U32x4Ge => [0xFDu8, 64].encode(v),
            I64x2Eq => [0xFDu8, 214].encode(v),
            I64x2Ne => [0xFDu8, 215].encode(v),
            S64x2Lt => [0xFDu8, 216].encode(v),
            S64x2Gt => [0xFDu8, 217].encode(v),
            S64x2Le => [0xFDu8, 218].encode(v),
            S64x2Ge => [0xFDu8, 219].encode(v),
            F32x4Eq => [0xFDu8, 65].encode(v),
            F32x4Ne => [0xFDu8, 66].encode(v),
            F32x4Lt => [0xFDu8, 67].encode(v),
            F32x4Gt => [0xFDu8, 68].encode(v),
            F32x4Le => [0xFDu8, 69].encode(v),
            F32x4Ge => [0xFDu8, 70].encode(v),
            F64x2Eq => [0xFDu8, 71].encode(v),
            F64x2Ne => [0xFDu8, 72].encode(v),
            F64x2Lt => [0xFDu8, 73].encode(v),
            F64x2Gt => [0xFDu8, 74].encode(v),
            F64x2Le => [0xFDu8, 75].encode(v),
            F64x2Ge => [0xFDu8, 76].encode(v),
            V128Not => [0xFDu8, 77].encode(v),
            V128And => [0xFDu8, 78].encode(v),
            V128AndNot => [0xFDu8, 79].encode(v),
            V128Or => [0xFDu8, 80].encode(v),
            V128Xor => [0xFDu8, 81].encode(v),
            V128BitSelect => [0xFDu8, 82].encode(v),
            V128AnyTrue => [0xFDu8, 83].encode(v),
            I8x16Abs => [0xFDu8, 96].encode(v),
            I8x16Neg => [0xFDu8, 97].encode(v),
            I8x16CountOnes => [0xFDu8, 98].encode(v),
            I8x16AllTrue => [0xFDu8, 99].encode(v),
            I8x16Bitmask => [0xFDu8, 100].encode(v),
            S8x16NarrowI16x8 => [0xFDu8, 101].encode(v),
            U8x16NarrowI16x8 => [0xFDu8, 102].encode(v),
            I8x16Shl => [0xFDu8, 107].encode(v),
            S8x16Shr => [0xFDu8, 108].encode(v),
            U8x16Shr => [0xFDu8, 109].encode(v),
            I8x16Add => [0xFDu8, 110].encode(v),
            S8x16AddSaturate => [0xFDu8, 111].encode(v),
            U8x16AddSaturate => [0xFDu8, 112].encode(v),
            I8x16Sub => [0xFDu8, 113].encode(v),
            S8x16SubSaturate => [0xFDu8, 114].encode(v),
            U8x16SubSaturate => [0xFDu8, 115].encode(v),
            S8x16Min => [0xFDu8, 118].encode(v),
            U8x16Min => [0xFDu8, 119].encode(v),
            S8x16Max => [0xFDu8, 120].encode(v),
            U8x16Max => [0xFDu8, 121].encode(v),
            U8x16Avgr => [0xFDu8, 123].encode(v),
            I16x8ExtendAddPairwiseS8x16 => [0xFDu8, 124].encode(v),
            I16x8ExtendAddPairwiseU8x16 => [0xFDu8, 125].encode(v),
            I16x8Abs => [0xFDu8, 128].encode(v),
            I16x8Neg => [0xFDu8, 129].encode(v),
            S16x8Q15MulRSat => [0xFDu8, 130].encode(v),
            I16x8AllTrue => [0xFDu8, 131].encode(v),
            I16x8Bitmask => [0xFDu8, 132].encode(v),
            S16x8NarrowI32x4 => [0xFDu8, 133].encode(v),
            U16x8NarrowI32x4 => [0xFDu8, 134].encode(v),
            I16x8ExtendLowS8x16 => [0xFDu8, 135].encode(v),
            I16x8ExtendHighS8x16 => [0xFDu8, 136].encode(v),
            I16x8ExtendLowU8x16 => [0xFDu8, 137].encode(v),
            I16x8ExtendHighU8x16 => [0xFDu8, 138].encode(v),
            I16x8Shl => [0xFDu8, 139].encode(v),
            S16x8Shr => [0xFDu8, 140].encode(v),
            U16x8Shr => [0xFDu8, 141].encode(v),
            I16x8Add => [0xFDu8, 142].encode(v),
            S16x8AddSaturate => [0xFDu8, 143].encode(v),
            U16x8AddSaturate => [0xFDu8, 144].encode(v),
            I16x8Sub => [0xFDu8, 145].encode(v),
            S16x8SubSaturate => [0xFDu8, 146].encode(v),
            U16x8SubSaturate => [0xFDu8, 147].encode(v),
            I16x8Mul => [0xFDu8, 149].encode(v),
            S16x8Min => [0xFDu8, 150].encode(v),
            U16x8Min => [0xFDu8, 151].encode(v),
            S16x8Max => [0xFDu8, 152].encode(v),
            U16x8Max => [0xFDu8, 153].encode(v),
            U16x8Avgr => [0xFDu8, 155].encode(v),
            I16x8ExtMulLowS8x16 => [0xFDu8, 156].encode(v),
            I16x8ExtMulHighS8x16 => [0xFDu8, 157].encode(v),
            I16x8ExtMulLowU8x16 => [0xFDu8, 158].encode(v),
            I16x8ExtMulHighU8x16 => [0xFDu8, 159].encode(v),
            I32x4ExtendAddPairwiseS16x8 => [0xFDu8, 126].encode(v),
            I32x4ExtendAddPairwiseU16x8 => [0xFDu8, 127].encode(v),
            I32x4Abs => [0xFDu8, 160].encode(v),
            I32x4Neg => [0xFDu8, 161].encode(v),
            I32x4AllTrue => [0xFDu8, 163].encode(v),
            I32x4Bitmask => [0xFDu8, 164].encode(v),
            I32x4ExtendLowS16x8 => [0xFDu8, 167].encode(v),
            I32x4ExtendHighS16x8 => [0xFDu8, 168].encode(v),
            I32x4ExtendLowU16x8 => [0xFDu8, 169].encode(v),
            I32x4ExtendHighU16x8 => [0xFDu8, 170].encode(v),
            I32x4Shl => [0xFDu8, 171].encode(v),
            S32x4Shr => [0xFDu8, 172].encode(v),
            U32x4Shr => [0xFDu8, 173].encode(v),
            I32x4Add => [0xFDu8, 174].encode(v),
            I32x4Sub => [0xFDu8, 177].encode(v),
            I32x4Mul => [0xFDu8, 181].encode(v),
            S32x4Min => [0xFDu8, 182].encode(v),
            U32x4Min => [0xFDu8, 183].encode(v),
            S32x4Max => [0xFDu8, 184].encode(v),
            U32x4Max => [0xFDu8, 185].encode(v),
            I32x4DotProductS16x8 => [0xFDu8, 186].encode(v),
            I32x4ExtMulLowS16x8 => [0xFDu8, 188].encode(v),
            I32x4ExtMulHighS16x8 => [0xFDu8, 189].encode(v),
            I32x4ExtMulLowU16x8 => [0xFDu8, 190].encode(v),
            I32x4ExtMulHighU16x8 => [0xFDu8, 191].encode(v),
            I64x2Abs => [0xFDu8, 192].encode(v),
            I64x2Neg => [0xFDu8, 193].encode(v),
            I64x2AllTrue => [0xFDu8, 195].encode(v),
            I64x2Bitmask => [0xFDu8, 196].encode(v),
            I64x2ExtendLowS32x4 => [0xFDu8, 199].encode(v),
            I64x2ExtendHighS32x4 => [0xFDu8, 200].encode(v),
            I64x2ExtendLowU32x4 => [0xFDu8, 201].encode(v),
            I64x2ExtendHighU32x4 => [0xFDu8, 202].encode(v),
            I64x2Shl => [0xFDu8, 203].encode(v),
            S64x2Shr => [0xFDu8, 204].encode(v),
            U64x2Shr => [0xFDu8, 205].encode(v),
            I64x2Add => [0xFDu8, 206].encode(v),
            I64x2Sub => [0xFDu8, 209].encode(v),
            I64x2Mul => [0xFDu8, 213].encode(v),
            I64x2ExtMulLowS32x4 => [0xFDu8, 220].encode(v),
            I64x2ExtMulHighS32x4 => [0xFDu8, 221].encode(v),
            I64x2ExtMulLowU32x4 => [0xFDu8, 223].encode(v),
            I64x2ExtMulHighU32x4 => [0xFDu8, 224].encode(v),
            F32x4Ceil => [0xFDu8, 103].encode(v),
            F32x4Floor => [0xFDu8, 104].encode(v),
            F32x4Trunc => [0xFDu8, 105].encode(v),
            F32x4Nearest => [0xFDu8, 106].encode(v),
            F32x4Abs => [0xFDu8, 224].encode(v),
            F32x4Neg => [0xFDu8, 225].encode(v),
            F32x4Sqrt => [0xFDu8, 227].encode(v),
            F32x4Add => [0xFDu8, 228].encode(v),
            F32x4Sub => [0xFDu8, 229].encode(v),
            F32x4Mul => [0xFDu8, 230].encode(v),
            F32x4Div => [0xFDu8, 231].encode(v),
            F32x4Min => [0xFDu8, 232].encode(v),
            F32x4Max => [0xFDu8, 233].encode(v),
            F32x4PMin => [0xFDu8, 234].encode(v),
            F32x4PMax => [0xFDu8, 235].encode(v),
            F64x2Ceil => [0xFDu8, 116].encode(v),
            F64x2Floor => [0xFDu8, 117].encode(v),
            F64x2Trunc => [0xFDu8, 122].encode(v),
            F64x2Nearest => [0xFDu8, 148].encode(v),
            F64x2Abs => [0xFDu8, 236].encode(v),
            F64x2Neg => [0xFDu8, 237].encode(v),
            F64x2Sqrt => [0xFDu8, 239].encode(v),
            F64x2Add => [0xFDu8, 240].encode(v),
            F64x2Sub => [0xFDu8, 241].encode(v),
            F64x2Mul => [0xFDu8, 242].encode(v),
            F64x2Div => [0xFDu8, 243].encode(v),
            F64x2Min => [0xFDu8, 244].encode(v),
            F64x2Max => [0xFDu8, 245].encode(v),
            F64x2PMin => [0xFDu8, 246].encode(v),
            F64x2PMax => [0xFDu8, 247].encode(v),
            S32x4TruncSatF32x4 => [0xFDu8, 248].encode(v),
            U32x4TruncSatF32x4 => [0xFDu8, 249].encode(v),
            F32x4ConvertS32x4 => [0xFDu8, 250].encode(v),
            F32x4ConvertU32x4 => [0xFDu8, 251].encode(v),
            S32x4TruncSatZeroF64x2 => [0xFDu8, 252].encode(v),
            U32x4TruncSatZeroF64x2 => [0xFDu8, 253].encode(v),
            F64x2ConvertLowS32x4 => [0xFDu8, 254].encode(v),
            F64x2ConvertLowU32x4 => [0xFDu8, 255].encode(v),
            F32x4DemoteF64x2Zero => [0xFDu8, 94].encode(v),
            F64x2PromoteLowF32x4 => [0xFDu8, 95].encode(v),
        }
    }
}