/// A macro for creating [`Instruction`](crate::functions::Instruction)s with wat-like syntax
/// 
/// Values can be interpolated with `{ /* ... */ }` in place of an instruction's
/// immediate or type
/// 
/// ```
/// # use wabam::{functions::Instruction, instr};
/// let x = 42;
/// let c = instr!(i32.const { x });
/// assert_eq!(c, Instruction::I32Const(42));
/// ```
#[macro_export]
macro_rules! instr {
    // Split the implementation from the usage, so the docs don't show every
    // single instruction at once
    ($($x:tt)*) => {
        $crate::instr_impl!($($x)*)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! instr_impl {
    (unreachable) => { $crate::I::Unreachable };
    (nop) => { $crate::I::NoOp };

    (block) => { $crate::I::Block(None) };
    (block (result $t:tt)) => { $crate::I::Block($crate::mlto!($t))};

    (loop) => { $crate::I::Loop(kind) };
    (loop (result $t:tt)) => { $crate::I::Loop($crate::mlto!($t))};

    (if) => { $crate::I::If(None)};
    (if (result $t:tt)) => { $crate::I::If($crate::mlto!($t))};

    (else) => { $crate::I::Else };
    (end) => { $crate::I::End };
    (br $x:tt) => { $crate::I::Branch { depth: $crate::ml!($x) } };
    (br_if $x:tt) => { $crate::I::BranchIf { depth: $crate::ml!($x) } };
    (br_table $($x:tt)+) => {
        {
            let a = [$($crate::ml!($x)),*];
            let (failsafe, depths) = a.split_last().unwrap();
            $crate::I::BranchTable { depths: depths.to_vec(), failsafe: *failsafe }
        }
    };

    (return) => { $crate::I::Return };
    (call $x:tt) => { $crate::I::Call { func_idx: $crate::ml!($x) } };
    (call_indirect $type:tt $table:tt) => { $crate::I::CallIndirect { type_idx: $crate::ml!($type), table_idx: $crate::ml!($table) } };

    (drop) => { $crate::I::Drop };
    (select) => { $crate::I::Select };

    (local.get $x:tt) => { $crate::I::LocalGet($crate::ml!($x))};
    (local.set $x:tt) => { $crate::I::LocalSet($crate::ml!($x))};
    (local.tee $x:tt) => { $crate::I::LocalTee($crate::ml!($x))};
    (global.get $x:tt) => { $crate::I::GlobalGet($crate::ml!($x))};
    (global.set $x:tt) => { $crate::I::GlobalSet($crate::ml!($x))};

    (table.get $x:tt) => {$crate::I::TableGet($crate::ml!($x))};
    (table.set $x:tt) => {$crate::I::TableSet($crate::ml!($x))};
    (table.size $x:tt) => {$crate::I::TableSize($crate::ml!($x))};
    (table.grow $x:tt) => {$crate::I::TableGrow($crate::ml!($x))};
    (table.fill $x:tt) => {$crate::I::TableFill($crate::ml!($x))};
    (table.copy $x:tt $y:tt) => {$crate::I::TableCopy { dest_index: $crate::ml!($x), src_index: $crate::ml!($x) }};
    (table.init $y:tt $x:tt) => {$crate::I::TableInit { table_index: $crate::ml!($x), elem_index: $crate::ml!($x) }};
    (elem.drop $x:tt) => {$crate::I::ElemDrop($crate::ml!($x))};

    (i32.load $($memarg:tt)*) => {$crate::meminstr!(I32Load, 2, $($memarg)*)};
    (i64.load $($memarg:tt)*) => {$crate::meminstr!(I64Load, 3, $($memarg)*)};
    (f32.load $($memarg:tt)*) => {$crate::meminstr!(F32Load, 2, $($memarg)*)};
    (f64.load $($memarg:tt)*) => {$crate::meminstr!(F64Load, 3, $($memarg)*)};
    (i32.load8_s $($memarg:tt)*) => {$crate::meminstr!(I32LoadS8, 0, $($memarg)*)};
    (i32.load8_u $($memarg:tt)*) => {$crate::meminstr!(I32LoadU8, 0, $($memarg)*)};
    (i32.load16_s $($memarg:tt)*) => {$crate::meminstr!(I32LoadS16, 1, $($memarg)*)};
    (i32.load16_u $($memarg:tt)*) => {$crate::meminstr!(I32LoadU16, 1, $($memarg)*)};
    (i64.load8_s $($memarg:tt)*) => {$crate::meminstr!(I64LoadS8, 0, $($memarg)*)};
    (i64.load8_u $($memarg:tt)*) => {$crate::meminstr!(I64LoadU8, 0, $($memarg)*)};
    (i64.load16_s $($memarg:tt)*) => {$crate::meminstr!(I64LoadS16, 1, $($memarg)*)};
    (i64.load16_u $($memarg:tt)*) => {$crate::meminstr!(I64LoadU16, 1, $($memarg)*)};
    (i64.load32_s $($memarg:tt)*) => {$crate::meminstr!(I64LoadS32, 2, $($memarg)*)};
    (i64.load32_u $($memarg:tt)*) => {$crate::meminstr!(I64LoadU32, 2, $($memarg)*)};
    (i32.store $($memarg:tt)*) => {$crate::meminstr!(I32Store, 2, $($memarg)*)};
    (i64.store $($memarg:tt)*) => {$crate::meminstr!(I64Store, 3, $($memarg)*)};
    (f32.store $($memarg:tt)*) => {$crate::meminstr!(F32Store, 2, $($memarg)*)};
    (f64.store $($memarg:tt)*) => {$crate::meminstr!(F64Store, 3, $($memarg)*)};
    (i32.store8 $($memarg:tt)*) => {$crate::meminstr!(I32StoreI8, 0, $($memarg)*)};
    (i32.store16 $($memarg:tt)*) => {$crate::meminstr!(I32StoreI16, 1, $($memarg)*)};
    (i64.store8 $($memarg:tt)*) => {$crate::meminstr!(I64StoreI8, 0, $($memarg)*)};
    (i64.store16 $($memarg:tt)*) => {$crate::meminstr!(I64StoreI16, 1, $($memarg)*)};
    (i64.store32 $($memarg:tt)*) => {$crate::meminstr!(I64StoreI32, 2, $($memarg)*)};

    (memory.size) => {$crate::I::MemorySize};
    (memory.grow) => {$crate::I::MemoryGrow};
    (memory.init $x:tt) => {$crate::I::MemoryInit($crate::ml!($x))};
    (data.drop $x:tt) => {$crate::I::DataDrop($crate::ml!($x))};
    (memory.copy) => {$crate::I::MemoryCopy};
    (memory.fill) => {$crate::I::MemoryFill};

    (i32.const $x:tt) => {$crate::I::I32Const($crate::ml!($x))};
    (i64.const $x:tt) => {$crate::I::I64Const($crate::ml!($x))};
    (f32.const $x:tt) => {$crate::I::F32Const($crate::ml!($x))};
    (f64.const $x:tt) => {$crate::I::F64Const($crate::ml!($x))};

    (i32.eqz) => {$crate::I::I32EqualsZero};
    (i32.eq) => {$crate::I::I32Equal};
    (i32.ne) => {$crate::I::I32NotEqual};
    (i32.lt_s) => {$crate::I::S32LessThan};
    (i32.lt_u) => {$crate::I::U32LessThan};
    (i32.gt_s) => {$crate::I::S32GreaterThan};
    (i32.gt_u) => {$crate::I::U32GreaterThan};
    (i32.le_s) => {$crate::I::S32LessThanOrEqual};
    (i32.le_u) => {$crate::I::U32LessThanOrEqual};
    (i32.ge_s) => {$crate::I::S32GreaterThanOrEqual};
    (i32.ge_u) => {$crate::I::U32GreaterThanOrEqual};

    (i64.eqz) => {$crate::I::I64EqualsZero};
    (i64.eq) => {$crate::I::I64Equal};
    (i64.ne) => {$crate::I::I64NotEqual};
    (i64.lt_s) => {$crate::I::S64LessThan};
    (i64.lt_u) => {$crate::I::U64LessThan};
    (i64.gt_s) => {$crate::I::S64GreaterThan};
    (i64.gt_u) => {$crate::I::U64GreaterThan};
    (i64.le_s) => {$crate::I::S64LessThanOrEqual};
    (i64.le_u) => {$crate::I::U64LessThanOrEqual};
    (i64.ge_s) => {$crate::I::S64GreaterThanOrEqual};
    (i64.ge_u) => {$crate::I::U64GreaterThanOrEqual};

    (f32.eq) => {$crate::I::F32Equal};
    (f32.ne) => {$crate::I::F32NotEqual};
    (f32.lt) => {$crate::I::F32LessThan};
    (f32.gt) => {$crate::I::F32GreaterThan};
    (f32.le) => {$crate::I::F32LessThanOrEqual};
    (f32.ge) => {$crate::I::F32GreaterThanOrEqual};

    (f64.eq) => {$crate::I::F64Equal};
    (f64.ne) => {$crate::I::F64NotEqual};
    (f64.lt) => {$crate::I::F64LessThan};
    (f64.gt) => {$crate::I::F64GreaterThan};
    (f64.le) => {$crate::I::F64LessThanOrEqual};
    (f64.ge) => {$crate::I::F64GreaterThanOrEqual};

    (i32.clz) => {$crate::I::I32CountLeadingZeroes};
    (i32.ctz) => {$crate::I::I32CountTrailingZeroes};
    (i32.popcnt) => {$crate::I::I32CountOnes};
    (i32.add) => {$crate::I::I32Add};
    (i32.sub) => {$crate::I::I32Sub};
    (i32.mul) => {$crate::I::I32Mul};
    (i32.div_s) => {$crate::I::S32Div};
    (i32.div_u) => {$crate::I::U32Div};
    (i32.rem_s) => {$crate::I::S32Rem};
    (i32.rem_u) => {$crate::I::U32Rem};
    (i32.and) => {$crate::I::I32And};
    (i32.or) => {$crate::I::I32Or};
    (i32.xor) => {$crate::I::I32Xor};
    (i32.shl) => {$crate::I::I32ShiftLeft};
    (i32.shr_s) => {$crate::I::S32ShiftRight};
    (i32.shr_u) => {$crate::I::U32ShiftRight};
    (i32.rotl) => {$crate::I::I32RotateLeft};
    (i32.rotr) => {$crate::I::I32RotateRight};

    (i64.clz) => {$crate::I::I64CountLeadingZeroes};
    (i64.ctz) => {$crate::I::I64CountTrailingZeroes};
    (i64.popcnt) => {$crate::I::I64CountOnes};
    (i64.add) => {$crate::I::I64Add};
    (i64.sub) => {$crate::I::I64Sub};
    (i64.mul) => {$crate::I::I64Mul};
    (i64.div_s) => {$crate::I::S64Div};
    (i64.div_u) => {$crate::I::U64Div};
    (i64.rem_s) => {$crate::I::S64Rem};
    (i64.rem_u) => {$crate::I::U64Rem};
    (i64.and) => {$crate::I::I64And};
    (i64.or) => {$crate::I::I64Or};
    (i64.xor) => {$crate::I::I64Xor};
    (i64.shl) => {$crate::I::I64ShiftLeft};
    (i64.shr_s) => {$crate::I::S64ShiftRight};
    (i64.shr_u) => {$crate::I::U64ShiftRight};
    (i64.rotl) => {$crate::I::I64RotateLeft};
    (i64.rotr) => {$crate::I::I64RotateRight};

    (f32.abs) => {$crate::I::F32AbsoluteValue};
    (f32.neg) => {$crate::I::F32Negate};
    (f32.ceil) => {$crate::I::F32Ceiling};
    (f32.floor) => {$crate::I::F32Floor};
    (f32.trunc) => {$crate::I::F32Truncate};
    (f32.nearest) => {$crate::I::F32Nearest};
    (f32.sqrt) => {$crate::I::F32SquareRoot};
    (f32.add) => {$crate::I::F32Add};
    (f32.sub) => {$crate::I::F32Sub};
    (f32.mul) => {$crate::I::F32Mul};
    (f32.div) => {$crate::I::F32Div};
    (f32.min) => {$crate::I::F32Min};
    (f32.max) => {$crate::I::F32Max};
    (f32.copysign) => {$crate::I::F32CopySign};

    (f64.abs) => {$crate::I::F64AbsoluteValue};
    (f64.neg) => {$crate::I::F64Negate};
    (f64.ceil) => {$crate::I::F64Ceiling};
    (f64.floor) => {$crate::I::F64Floor};
    (f64.trunc) => {$crate::I::F64Truncate};
    (f64.nearest) => {$crate::I::F64Nearest};
    (f64.sqrt) => {$crate::I::F64SquareRoot};
    (f64.add) => {$crate::I::F64Add};
    (f64.sub) => {$crate::I::F64Sub};
    (f64.mul) => {$crate::I::F64Mul};
    (f64.div) => {$crate::I::F64Div};
    (f64.min) => {$crate::I::F64Min};
    (f64.max) => {$crate::I::F64Max};
    (f64.copysign) => {$crate::I::F64CopySign};

    (i32.wrap_i64) => {$crate::I::I32WrapI64};
    (i32.trunc_f32_s) => {$crate::I::S32TruncateF32};
    (i32.trunc_f32_u) => {$crate::I::U32TruncateF32};
    (i32.trunc_f64_s) => {$crate::I::S32TruncateF64};
    (i32.trunc_f64_u) => {$crate::I::U32TruncateF64};

    (i64.extend_i32_s) => {$crate::I::I64ExtendS32};
    (i64.extend_i32_u) => {$crate::I::I64ExtendU32};
    (i64.trunc_f32_s) => {$crate::I::S64TruncateF32};
    (i64.trunc_f32_u) => {$crate::I::U64TruncateF32};
    (i64.trunc_f64_s) => {$crate::I::S64TruncateF64};
    (i64.trunc_f64_u) => {$crate::I::U64TruncateF64};

    (f32.convert_i32_s) => {$crate::I::F32ConvertS32};
    (f32.convert_i32_u) => {$crate::I::F32ConvertU32};
    (f32.convert_i64_s) => {$crate::I::F32ConvertS64};
    (f32.convert_i64_u) => {$crate::I::F32ConvertU64};
    (f32.demote_f64) => {$crate::I::F32DemoteF64};

    (f64.convert_i32_s) => {$crate::I::F64ConvertS32};
    (f64.convert_i32_u) => {$crate::I::F64ConvertU32};
    (f64.convert_i64_s) => {$crate::I::F64ConvertS64};
    (f64.convert_i64_u) => {$crate::I::F64ConvertU64};
    (f64.promote_f32) => {$crate::I::F64PromoteF32};

    (i32.reinterpret_f32) => {$crate::I::I32ReinterpretF32};
    (i64.reinterpret_f64) => {$crate::I::I64ReinterpretF64};
    (f32.reinterpret_i32) => {$crate::I::F32ReinterpretI32};
    (f64.reinterpret_i64) => {$crate::I::F64ReinterpretI64};

    (i32.extend8_s) => {$crate::I::S32Extend8};
    (i32.extend16_s) => {$crate::I::S32Extend16};
    (i64.extend8_s) => {$crate::I::S64Extend8};
    (i64.extend16_s) => {$crate::I::S64Extend16};
    (i64.extend32_s) => {$crate::I::S64Extend32};

    (i32.trunc_sat_f32_s) => {$crate::I::S32SaturatingTruncateF32};
    (i32.trunc_sat_f32_u) => {$crate::I::U32SaturatingTruncateF32};
    (i32.trunc_sat_f64_s) => {$crate::I::S32SaturatingTruncateF64};
    (i32.trunc_sat_f64_u) => {$crate::I::U32SaturatingTruncateF64};
    (i64.trunc_sat_f32_s) => {$crate::I::S64SaturatingTruncateF32};
    (i64.trunc_sat_f32_u) => {$crate::I::U64SaturatingTruncateF32};
    (i64.trunc_sat_f64_s) => {$crate::I::S64SaturatingTruncateF64};
    (i64.trunc_sat_f64_u) => {$crate::I::U64SaturatingTruncateF64};

    (v128.load $($memarg:tt)*) => {$crate::meminstr!(V128Load, 4, $($memarg)*)};
    (v128.load8x8_s $($memarg:tt)*) => {$crate::meminstr!(V128LoadS8x8, 3, $($memarg)*)};
    (v128.load8x8_u $($memarg:tt)*) => {$crate::meminstr!(V128LoadU8x8, 3, $($memarg)*)};
    (v128.load16x4_s $($memarg:tt)*) => {$crate::meminstr!(V128LoadS16x4, 3, $($memarg)*)};
    (v128.load16x4_u $($memarg:tt)*) => {$crate::meminstr!(V128LoadU16x4, 3, $($memarg)*)};
    (v128.load32x2_s $($memarg:tt)*) => {$crate::meminstr!(V128LoadS16x4, 3, $($memarg)*)};
    (v128.load32x2_u $($memarg:tt)*) => {$crate::meminstr!(V128LoadU16x4, 3, $($memarg)*)};
    (v128.load8_splat $($memarg:tt)*) => {$crate::meminstr!(V128LoadSplatI8, 0, $($memarg)*)};
    (v128.load16_splat $($memarg:tt)*) => {$crate::meminstr!(V128LoadSplatI16, 1, $($memarg)*)};
    (v128.load32_splat $($memarg:tt)*) => {$crate::meminstr!(V128LoadSplatI32, 2, $($memarg)*)};
    (v128.load64_splat $($memarg:tt)*) => {$crate::meminstr!(V128LoadSplatI64, 3, $($memarg)*)};
    (v128.load32_zero $($memarg:tt)*) => {$crate::meminstr!(V128LoadZeroI32, 2, $($memarg)*)};
    (v128.load64_zero $($memarg:tt)*) => {$crate::meminstr!(V128LoadZeroI64, 3, $($memarg)*)};
    (v128.store $($memarg:tt)*) => {$crate::meminstr!(V128Store, 4, $($memarg)*)};
    (v128.load8_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Load8Lane {
            align: $crate::memarg_align!(0, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.load16_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Load16Lane {
            align: $crate::memarg_align!(1, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.load32_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Load32Lane {
            align: $crate::memarg_align!(2, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.load64_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Load16Lane {
            align: $crate::memarg_align!(3, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.store8_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Store8Lane {
            align: $crate::memarg_align!(0, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.store16_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Store16Lane {
            align: $crate::memarg_align!(1, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.store32_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Store32Lane {
            align: $crate::memarg_align!(2, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.store64_lane $(align=$align:tt)? $(offset=$offset:tt)? $lane_idx:tt) => {
        $crate::I::V128Store16Lane {
            align: $crate::memarg_align!(3, $(align=$align)?),
            offset: $crate::memarg_offset!($(offset=$offset)?),
            lane_idx: $crate::ml!($lane_idx)
        }
    };
    (v128.const $_0:tt $_1:tt $_2:tt $_3:tt $_4:tt $_5:tt $_6:tt $_7:tt $_8:tt $_9:tt $a:tt $b:tt $c:tt $d:tt $e:tt $f:tt) => {
        {
            let bytes: [u8; 16] = [
                $crate::ml!($_0),
                $crate::ml!($_1),
                $crate::ml!($_2),
                $crate::ml!($_3),
                $crate::ml!($_4),
                $crate::ml!($_5),
                $crate::ml!($_6),
                $crate::ml!($_7),
                $crate::ml!($_8),
                $crate::ml!($_9),
                $crate::ml!($a),
                $crate::ml!($b),
                $crate::ml!($c),
                $crate::ml!($d),
                $crate::ml!($e),
                $crate::ml!($f),
            ];
            $crate::I::V128Const(bytes)
        }
    };
    (v128.const $_0:tt $_1:tt $_2:tt $_3:tt $_4:tt $_5:tt $_6:tt $_7:tt) => {
        {
            let values: [u16; 8] = [
                $crate::ml!($_0),
                $crate::ml!($_1),
                $crate::ml!($_2),
                $crate::ml!($_3),
                $crate::ml!($_4),
                $crate::ml!($_5),
                $crate::ml!($_6),
                $crate::ml!($_7),
            ];
            let bytes: [[u8; 2]; 8] = [
                values[0].to_le_bytes(),
                values[1].to_le_bytes(),
                values[2].to_le_bytes(),
                values[3].to_le_bytes(),
                values[4].to_le_bytes(),
                values[5].to_le_bytes(),
                values[6].to_le_bytes(),
                values[7].to_le_bytes(),
            ]
            let bytes: [u8; 16] = [
                bytes[0x0][0],
                bytes[0x1][1],
                bytes[0x2][0],
                bytes[0x3][1],
                bytes[0x4][0],
                bytes[0x5][1],
                bytes[0x6][0],
                bytes[0x7][1],
                bytes[0x8][0],
                bytes[0x9][1],
                bytes[0xA][0],
                bytes[0xB][1],
                bytes[0xC][0],
                bytes[0xD][1],
                bytes[0xE][0],
                bytes[0xF][1],
            ];
            $crate::I::V128Const(bytes)
        }
    };
    (v128.const $_0:tt $_1:tt $_2:tt $_3:tt) => {
        {
            let values: [u32; 4] = [
                $crate::ml!($_0),
                $crate::ml!($_1),
                $crate::ml!($_2),
                $crate::ml!($_3),
            ];
            let bytes: [[u8; 4]; 4] = [
                values[0].to_le_bytes(),
                values[1].to_le_bytes(),
                values[2].to_le_bytes(),
                values[3].to_le_bytes(),
            ];
            let bytes: [u8; 16] = [
                bytes[0x0][0],
                bytes[0x1][1],
                bytes[0x2][2],
                bytes[0x3][3],
                bytes[0x4][0],
                bytes[0x5][1],
                bytes[0x6][2],
                bytes[0x7][3],
                bytes[0x8][0],
                bytes[0x9][1],
                bytes[0xA][2],
                bytes[0xB][3],
                bytes[0xC][0],
                bytes[0xD][1],
                bytes[0xE][2],
                bytes[0xF][3],
            ];
            $crate::I::V128Const(bytes)
        }
    };
    (v128.const $_0:tt $_1:tt) => {
        {
            let values: [u64; 2] = [
                $crate::ml!($_0),
                $crate::ml!($_1),
            ];
            let bytes: [[u8; 8]; 2] = [
                values[0].to_le_bytes(),
                values[1].to_le_bytes(),
            ];
            let bytes: [u8; 16] = [
                bytes[0x0][0],
                bytes[0x1][1],
                bytes[0x2][2],
                bytes[0x3][3],
                bytes[0x4][4],
                bytes[0x5][5],
                bytes[0x6][6],
                bytes[0x7][7],
                bytes[0x8][0],
                bytes[0x9][1],
                bytes[0xA][2],
                bytes[0xB][3],
                bytes[0xC][4],
                bytes[0xD][5],
                bytes[0xE][6],
                bytes[0xF][7],
            ];
            $crate::I::V128Const(bytes)
        }
    };
    (v128.const $v:tt) => {
        {
            let value: u128 = $crate::ml!($v);
            $crate::I::V128Const(value.to_le_bytes())
        }
    };
    (i8x16.shuffle $($x:tt)*) => { $crate::I::I8x16Shuffle { lanes: [$($crate::ml!($x))*] } };
    (i8x16.extract_lane_s $l:tt) => { $crate::I::S8x16ExtractLane { lane: $crate::ml!($t) } };
    (i8x16.extract_lane_u $l:tt) => { $crate::I::U8x16ExtractLane { lane: $crate::ml!($t) } };
    (i8x16.replace_lane $l:tt) => { $crate::I::I8x16ReplaceLane { lane: $crate::ml!($t) } };
    (i16x8.extract_lane_s $l:tt) => { $crate::I::S16x8ExtractLane { lane: $crate::ml!($t) } };
    (i16x8.extract_lane_u $l:tt) => { $crate::I::U16x8ExtractLane { lane: $crate::ml!($t) } };
    (i16x8.replace_lane $l:tt) => { $crate::I::I16x8ReplaceLane { lane: $crate::ml!($t) } };
    (i32x4.extract_lane $l:tt) => { $crate::I::I32x4ExtractLane { lane: $crate::ml!($t) } };
    (i32x4.replace_lane $l:tt) => { $crate::I::I32x4ReplaceLane { lane: $crate::ml!($t) } };
    (i64x2.extract_lane $l:tt) => { $crate::I::I64x2ExtractLane { lane: $crate::ml!($t) } };
    (i64x2.replace_lane $l:tt) => { $crate::I::I64x2ReplaceLane { lane: $crate::ml!($t) } };
    (f32x4.extract_lane $l:tt) => { $crate::I::F32x4ExtractLane { lane: $crate::ml!($t) } };
    (f32x4.replace_lane $l:tt) => { $crate::I::F32x4ReplaceLane { lane: $crate::ml!($t) } };
    (f64x2.extract_lane $l:tt) => { $crate::I::F64x2ExtractLane { lane: $crate::ml!($t) } };
    (f64x2.replace_lane $l:tt) => { $crate::I::F64x2ReplaceLane { lane: $crate::ml!($t) } };

    (i8x16.swizzle) => { $crate::I::I8x16Swizzle };
    (i8x16.splat) => { $crate::I::I8x16Splat };
    (i16x8.splat) => { $crate::I::I16x8Splat };
    (i32x4.splat) => { $crate::I::I32x4Splat };
    (i64x2.splat) => { $crate::I::I64x2Splat };
    (f32x4.splat) => { $crate::I::F32x4Splat };
    (f64x2.splat) => { $crate::I::F64x2Splat };

    (i8x16.eq) => {$crate::I::I8x16Eq};
    (i8x16.ne) => {$crate::I::I8x16Ne};
    (i8x16.lt_s) => {$crate::I::S8x16Lt};
    (i8x16.lt_u) => {$crate::I::U8x16Lt};
    (i8x16.gt_s) => {$crate::I::S8x16Gt};
    (i8x16.gt_u) => {$crate::I::U8x16Gt};
    (i8x16.le_s) => {$crate::I::S8x16Le};
    (i8x16.le_u) => {$crate::I::U8x16Le};
    (i8x16.ge_s) => {$crate::I::S8x16Ge};
    (i8x16.ge_u) => {$crate::I::U8x16Ge};

    (i16x8.eq) => {$crate::I::I16x8Eq};
    (i16x8.ne) => {$crate::I::I16x8Ne};
    (i16x8.lt_s) => {$crate::I::S16x8Lt};
    (i16x8.lt_u) => {$crate::I::U16x8Lt};
    (i16x8.gt_s) => {$crate::I::S16x8Gt};
    (i16x8.gt_u) => {$crate::I::U16x8Gt};
    (i16x8.le_s) => {$crate::I::S16x8Le};
    (i16x8.le_u) => {$crate::I::U16x8Le};
    (i16x8.ge_s) => {$crate::I::S16x8Ge};
    (i16x8.ge_u) => {$crate::I::U16x8Ge};

    (i32x4.eq) => {$crate::I::I32x4Eq};
    (i32x4.ne) => {$crate::I::I32x4Ne};
    (i32x4.lt_s) => {$crate::I::S32x4Lt};
    (i32x4.lt_u) => {$crate::I::U32x4Lt};
    (i32x4.gt_s) => {$crate::I::S32x4Gt};
    (i32x4.gt_u) => {$crate::I::U32x4Gt};
    (i32x4.le_s) => {$crate::I::S32x4Le};
    (i32x4.le_u) => {$crate::I::U32x4Le};
    (i32x4.ge_s) => {$crate::I::S32x4Ge};
    (i32x4.ge_u) => {$crate::I::U32x4Ge};

    (i64x2.eq) => {$crate::I::I64x2Eq};
    (i64x2.ne) => {$crate::I::I64x2Ne};
    (i64x2.lt_s) => {$crate::I::S64x2Lt};
    (i64x2.gt_s) => {$crate::I::S64x2Gt};
    (i64x2.le_s) => {$crate::I::S64x2Le};
    (i64x2.ge_s) => {$crate::I::S64x2Ge};

    (f32x4.eq) => {$crate::I::F32x4Eq};
    (f32x4.ne) => {$crate::I::F32x4Ne};
    (f32x4.lt) => {$crate::I::F32x4Lt};
    (f32x4.gt) => {$crate::I::F32x4Gt};
    (f32x4.le) => {$crate::I::F32x4Le};
    (f32x4.ge) => {$crate::I::F32x4Ge};

    (f64x2.eq) => {$crate::I::F64x2Eq};
    (f64x2.ne) => {$crate::I::F64x2Ne};
    (f64x2.lt) => {$crate::I::F64x2Lt};
    (f64x2.gt) => {$crate::I::F64x2Gt};
    (f64x2.le) => {$crate::I::F64x2Le};
    (f64x2.ge) => {$crate::I::F64x2Ge};

    (v128.not) => {$crate::I::V128Not};
    (v128.and) => {$crate::I::V128And};
    (v128.andnot) => {$crate::I::V128AndNot};
    (v128.or) => {$crate::I::V128Or};
    (v128.xor) => {$crate::I::V128Xor};
    (v128.bitselect) => {$crate::I::V128BitSelect};
    (v128.any_true) => {$crate::I::V128AnyTrue};

    (i8x16.abs) => {$crate::I::I8x16Abs};
    (i8x16.neg) => {$crate::I::I8x16Neg};
    (i8x16.popcnt) => {$crate::I::I8x16CountOnes};
    (i8x16.all_true) => {$crate::I::I8x16AllTrue};
    (i8x16.bitmask) => {$crate::I::I8x16Bitmask};
    (i8x16.narrow_i16x8_s) => {$crate::I::S8x16NarrowI16x8};
    (i8x16.narrow_i16x8_u) => {$crate::I::U8x16NarrowI16x8};
    (i8x16.shl) => {$crate::I::I8x16Shl};
    (i8x16.shr_s) => {$crate::I::S8x16Shr};
    (i8x16.shr_u) => {$crate::I::U8x16Shr};
    (i8x16.add) => {$crate::I::I8x16Add};
    (i8x16.add_sat_s) => {$crate::I::S8x16AddSaturate};
    (i8x16.add_sat_u) => {$crate::I::U8x16AddSaturate};
    (i8x16.sub) => {$crate::I::I8x16Sub};
    (i8x16.sub_sat_s) => {$crate::I::S8x16SubSaturate};
    (i8x16.sub_sat_u) => {$crate::I::U8x16SubSaturate};
    (i8x16.min_s) => {$crate::I::S8x16Min};
    (i8x16.min_u) => {$crate::I::U8x16Min};
    (i8x16.max_s) => {$crate::I::S8x16Max};
    (i8x16.max_u) => {$crate::I::U8x16Max};
    (i8x16.avgr_u) => {$crate::I::U8x16Avgr};

    (i16x8.extadd_pairwise_i8x16_s) => {$crate::I::I16x8ExtendAddPairwiseS8x16};
    (i16x8.extadd_pairwise_i8x16_u) => {$crate::I::I16x8ExtendAddPairwiseU8x16};
    (i16x8.abs) => {$crate::I::I16x8Abs};
    (i16x8.neg) => {$crate::I::I16x8Neg};
    (i16x8.q15mulr_sat_s) => {$crate::I::S16x8Q15MulRSat};
    (i16x8.all_true) => {$crate::I::I16x8AllTrue};
    (i16x8.bitmask) => {$crate::I::I16x8Bitmask};
    (i16x8.extend_low_i8x16_s) => {$crate::I::I16x8ExtendLowS8x16};
    (i16x8.extend_high_i8x16_s) => {$crate::I::I16x8ExtendHighS8x16};
    (i16x8.extend_low_i8x16_u) => {$crate::I::I16x8ExtendLowU8x16};
    (i16x8.extend_high_i8x16_u) => {$crate::I::I16x8ExtendHighU8x16};
    (i16x8.shl) => {$crate::I::I16x8Shl};
    (i16x8.shr_s) => {$crate::I::S16x8Shr};
    (i16x8.shr_u) => {$crate::I::U16x8Shr};
    (i16x8.add) => {$crate::I::I16x8Add};
    (i16x8.add_sat_s) => {$crate::I::S16x8AddSaturate};
    (i16x8.add_sat_u) => {$crate::I::U16x8AddSaturate};
    (i16x8.sub) => {$crate::I::I16x8Sub};
    (i16x8.sub_sat_s) => {$crate::I::S16x8SubSaturate};
    (i16x8.sub_sat_u) => {$crate::I::U16x8SubSaturate};
    (i16x8.mul) => {$crate::I::I16x8Mul};
    (i16x8.min_s) => {$crate::I::S16x8Min};
    (i16x8.min_u) => {$crate::I::U16x8Min};
    (i16x8.max_s) => {$crate::I::S16x8Max};
    (i16x8.max_u) => {$crate::I::U16x8Max};
    (i16x8.avgr_u) => {$crate::I::U16x8Avgr};
    (i16x8.extmul_low_i8x16_s) => {$crate::I::I16x8ExtMulLowS8x16};
    (i16x8.extmul_high_i8x16_s) => {$crate::I::I16x8ExtMulHighS8x16};
    (i16x8.extmul_low_i8x16_u) => {$crate::I::I16x8ExtMulLowU8x16};
    (i16x8.extmul_high_i8x16_u) => {$crate::I::I16x8ExtMulHighU8x16};

    (i32x4.extadd_pairwise_i8x16_s) => {$crate::I::I32x4ExtendAddPairwiseS16x8};
    (i32x4.extadd_pairwise_i8x16_u) => {$crate::I::I32x4ExtendAddPairwiseU16x8};
    (i32x4.abs) => {$crate::I::I32x4Abs};
    (i32x4.neg) => {$crate::I::I32x4Neg};
    (i32x4.all_true) => {$crate::I::I32x4AllTrue};
    (i32x4.bitmask) => {$crate::I::I32x4Bitmask};
    (i32x4.extend_low_i8x16_s) => {$crate::I::I32x4ExtendLowS16x8};
    (i32x4.extend_high_i8x16_s) => {$crate::I::I32x4ExtendHighS16x8};
    (i32x4.extend_low_i8x16_u) => {$crate::I::I32x4ExtendLowU16x8};
    (i32x4.extend_high_i8x16_u) => {$crate::I::I32x4ExtendHighU16x8};
    (i32x4.shl) => {$crate::I::I32x4Shl};
    (i32x4.shr_s) => {$crate::I::S32x4Shr};
    (i32x4.shr_u) => {$crate::I::U32x4Shr};
    (i32x4.add) => {$crate::I::I32x4Add};
    (i32x4.sub) => {$crate::I::I32x4Sub};
    (i32x4.mul) => {$crate::I::I32x4Mul};
    (i32x4.min_s) => {$crate::I::S32x4Min};
    (i32x4.min_u) => {$crate::I::U32x4Min};
    (i32x4.max_s) => {$crate::I::S32x4Max};
    (i32x4.max_u) => {$crate::I::U32x4Max};
    (i32x4.dot_i16x8_s) => {$crate::I::I32x4DotProductS16x8};
    (i32x4.extmul_low_i8x16_s) => {$crate::I::I32x4ExtMulLowS16x8};
    (i32x4.extmul_high_i8x16_s) => {$crate::I::I32x4ExtMulHighS16x8};
    (i32x4.extmul_low_i8x16_u) => {$crate::I::I32x4ExtMulLowU16x8};
    (i32x4.extmul_high_i8x16_u) => {$crate::I::I32x4ExtMulHighU16x8};

    (i64x2.abs) => {$crate::I::I64x2Abs};
    (i64x2.neg) => {$crate::I::I64x2Neg};
    (i64x2.all_true) => {$crate::I::I64x2AllTrue};
    (i64x2.bitmask) => {$crate::I::I64x2Bitmask};
    (i64x2.extend_low_i8x16_s) => {$crate::I::I64x2ExtendLowS32x4};
    (i64x2.extend_high_i8x16_s) => {$crate::I::I64x2ExtendHighS32x4};
    (i64x2.extend_low_i8x16_u) => {$crate::I::I64x2ExtendLowU32x4};
    (i64x2.extend_high_i8x16_u) => {$crate::I::I64x2ExtendHighU32x4};
    (i64x2.shl) => {$crate::I::I64x2Shl};
    (i64x2.shr_s) => {$crate::I::S64x2Shr};
    (i64x2.shr_u) => {$crate::I::U64x2Shr};
    (i64x2.add) => {$crate::I::I64x2Add};
    (i64x2.sub) => {$crate::I::I64x2Sub};
    (i64x2.mul) => {$crate::I::I64x2Mul};
    (i64x2.extmul_low_i8x16_s) => {$crate::I::I64x2ExtMulLowS32x4};
    (i64x2.extmul_high_i8x16_s) => {$crate::I::I64x2ExtMulHighS32x4};
    (i64x2.extmul_low_i8x16_u) => {$crate::I::I64x2ExtMulLowU32x4};
    (i64x2.extmul_high_i8x16_u) => {$crate::I::I64x2ExtMulHighU32x4};

    (f32x4.ceil) => {$crate::I::F32x4Ceil};
    (f32x4.floor) => {$crate::I::F32x4Floor};
    (f32x4.trunc) => {$crate::I::F32x4Trunc};
    (f32x4.nearest) => {$crate::I::F32x4Nearest};
    (f32x4.abs) => {$crate::I::F32x4Abs};
    (f32x4.neg) => {$crate::I::F32x4Neg};
    (f32x4.sqrt) => {$crate::I::F32x4Sqrt};
    (f32x4.add) => {$crate::I::F32x4Add};
    (f32x4.sub) => {$crate::I::F32x4Sub};
    (f32x4.mul) => {$crate::I::F32x4Mul};
    (f32x4.div) => {$crate::I::F32x4Div};
    (f32x4.max) => {$crate::I::F32x4Max};
    (f32x4.min) => {$crate::I::F32x4Min};
    (f32x4.pmax) => {$crate::I::F32x4PMax};
    (f32x4.pmin) => {$crate::I::F32x4PMin};

    (f64x2.ceil) => {$crate::I::F64x2Ceil};
    (f64x2.floor) => {$crate::I::F64x2Floor};
    (f64x2.trunc) => {$crate::I::F64x2Trunc};
    (f64x2.nearest) => {$crate::I::F64x2Nearest};
    (f64x2.abs) => {$crate::I::F64x2Abs};
    (f64x2.neg) => {$crate::I::F64x2Neg};
    (f64x2.sqrt) => {$crate::I::F64x2Sqrt};
    (f64x2.add) => {$crate::I::F64x2Add};
    (f64x2.sub) => {$crate::I::F64x2Sub};
    (f64x2.mul) => {$crate::I::F64x2Mul};
    (f64x2.div) => {$crate::I::F64x2Div};
    (f64x2.max) => {$crate::I::F64x2Max};
    (f64x2.min) => {$crate::I::F64x2Min};
    (f64x2.pmax) => {$crate::I::F64x2PMax};
    (f64x2.pmin) => {$crate::I::F64x2PMin};

    (i32x4.trunc_sat_f32x4_s) => {$crate::I::S32x4TruncSatF32x4};
    (i32x4.trunc_sat_f32x4_u) => {$crate::I::U32x4TruncSatF32x4};
    (f32x4.convert_i32x4_s) => {$crate::I::F32x4ConvertS32x4};
    (f32x4.convert_i32x4_u) => {$crate::I::F32x4ConvertU32x4};
    (i32x4.trunc_sat_f32x4_s) => {$crate::I::S32x4TruncSatZeroF32x4};
    (i32x4.trunc_sat_f32x4_u) => {$crate::I::U32x4TruncSatZeroF32x4};
    (f64x2.convert_i64x2_s) => {$crate::I::F64x2ConvertLowS32x4};
    (f64x2.convert_i64x2_u) => {$crate::I::F64x2ConvertLowU32x4};
    (f32x4.demote_f64x2_zero) => {$crate::I::F32x4DemoteF64x2Zero};
    (f64x2.promote_low_f32x4) => {$crate::I::F64x2PromoteLowF32x4};
}

#[doc(hidden)]
#[macro_export]
macro_rules! meminstr {
    ($instr:tt, $align_default:literal, $($memarg:tt)*) => {
        $crate::I::$instr { align: $crate::memarg_align!($align_default, $($memarg)*), offset: $crate::memarg_offset!($($memarg)*) }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! memarg_align {
    ($align_default:literal, ) => {
        $align_default
    };
    ($align_default:literal, align=$align:tt) => {
        $crate::ml!($align)
    };
    ($align_default:literal, offset=$offset:tt) => {
        $align_default
    };
    ($align_default:literal, align=$align:tt offset=$offset:tt) => {
        $crate::ml!($align)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! memarg_offset {
    () => {
        0
    };
    (align=$align:tt) => {
        0
    };
    (offset=$offset:tt) => {
        $crate::ml!($offset)
    };
    (align=$align:tt offset=$offset:tt) => {
        $crate::ml!($offset)
    };
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

/// A macro for creating arrays of [`Instruction`](crate::functions::Instructions)s 
/// with wat-like syntax
/// 
/// Values can be interpolated with `{ /* ... */ }` in place of an instruction's
/// immediate or type
/// 
/// ```
/// # use wabam::{functions::Instruction, instrs};
/// let x = 42;
/// let expr = instrs!(
///     (local.get 0)
///     (i32.const { x })
///     (i32.eq)
/// );
/// 
/// assert_eq!(
///     expr,
///     [
///         Instruction::LocalGet(0),
///         Instruction::I32Const(42),
///         Instruction::I32Equal,
///     ],
/// );
/// ```
#[macro_export]
macro_rules! instrs {
    ($(($($t:tt)*))*) => {
        [$($crate::instr!($($t)*),)*]
    };
}
