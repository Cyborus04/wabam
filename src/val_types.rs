use crate::encode::{Buf, ErrorKind, WasmDecode, WasmEncode};

/// The type of a value in a function signature, global or local variable, or
/// the stack.
///
/// The corresponding types in Rust are as follows:
///
/// | WebAssembly | Rust | Description
/// |-|-|-|
/// |`i32`| `i32` and `u32` | 32 bits, without a sign. |
/// |`i64`| `i64` and `u64` | 64 bits, without a sign. |
/// |`f32`| `f32` | IEEE float. |
/// |`f64`| `f64` | IEEE double. |
/// |`funcref`| `fn(*) -> *` | Function handle. |
/// |`externref`| No equivalent | Arbitrary external resource. |
/// |`v128` | `std::arch::wasm32::v128` | 128-bit SIMD vector. |
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
    Vec(VecType),
}

impl ValType {
    /// wasm's `i32` type.
    pub const I32: ValType = Self::Num(NumType::I32);
    /// wasm's `i64` type.
    pub const I64: ValType = Self::Num(NumType::I64);
    /// wasm's `f32` type.
    pub const F32: ValType = Self::Num(NumType::F32);
    /// wasm's `f64` type.
    pub const F64: ValType = Self::Num(NumType::F64);

    /// wasm's `funcref` type.
    #[allow(non_upper_case_globals)]
    pub const FuncRef: ValType = Self::Ref(RefType::FuncRef);
    /// wasm's `externref` type.
    #[allow(non_upper_case_globals)]
    pub const ExternRef: ValType = Self::Ref(RefType::ExternRef);

    /// wasm's `v128` type.
    pub const V128: ValType = Self::Vec(VecType::V128);

    pub fn type_id(self) -> u8 {
        use ValType::*;
        match self {
            Num(x) => x.type_id(),
            Ref(x) => x.type_id(),
            Vec(x) => x.type_id(),
        }
    }
}

impl WasmEncode for ValType {
    fn size(&self) -> usize {
        1
    }

    fn encode(&self, v: &mut Vec<u8>) {
        v.push(self.type_id())
    }
}

impl WasmEncode for Option<ValType> {
    fn size(&self) -> usize {
        1
    }

    fn encode(&self, v: &mut Vec<u8>) {
        if let Some(this) = self {
            v.push(this.type_id());
        } else {
            v.push(0x40);
        }
    }
}

impl WasmDecode for ValType {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let b = u8::decode(buf)?;
        match b {
            0x7F => Ok(Self::I32),
            0x7E => Ok(Self::I64),
            0x7D => Ok(Self::F32),
            0x7C => Ok(Self::F64),
            0x70 => Ok(Self::FuncRef),
            0x6F => Ok(Self::ExternRef),
            0x7B => Ok(Self::V128),
            x => Err(ErrorKind::InvalidType(x)),
        }
    }
}

impl WasmDecode for Option<ValType> {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        match u8::decode(buf)? {
            0x7F => Ok(Some(ValType::I32)),
            0x7E => Ok(Some(ValType::I64)),
            0x7D => Ok(Some(ValType::F32)),
            0x7C => Ok(Some(ValType::F64)),
            0x70 => Ok(Some(ValType::FuncRef)),
            0x6F => Ok(Some(ValType::ExternRef)),
            0x7B => Ok(Some(ValType::V128)),
            0x40 => Ok(None),
            x => Err(ErrorKind::InvalidType(x)),
        }
    }
}

impl PartialOrd for ValType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ValType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.type_id().cmp(&other.type_id())
    }
}

/// The type of a reference value.
///
/// See [`ValType`] for more info.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefType {
    /// wasm's `funcref` type.
    FuncRef,
    /// wasm's `externref` type.
    ExternRef,
}

impl RefType {
    pub fn type_id(self) -> u8 {
        match self {
            Self::FuncRef => 0x70,
            Self::ExternRef => 0x6F,
        }
    }
}

impl WasmEncode for RefType {
    fn size(&self) -> usize {
        1
    }

    fn encode(&self, v: &mut Vec<u8>) {
        v.push(self.type_id())
    }
}

impl WasmDecode for RefType {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        match u8::decode(buf)? {
            0x70 => Ok(Self::FuncRef),
            0x6F => Ok(Self::ExternRef),
            x => Err(ErrorKind::InvalidType(x)),
        }
    }
}

/// The type of a numeric value.
///
/// See [`ValType`] for more info.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumType {
    /// wasm's `i32` type.
    I32,
    /// wasm's `i64` type.
    I64,
    /// wasm's `f32` type.
    F32,
    /// wasm's `f64` type.
    F64,
}

impl WasmDecode for NumType {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        match u8::decode(buf)? {
            0x7F => Ok(Self::I32),
            0x7E => Ok(Self::I64),
            0x7D => Ok(Self::F32),
            0x7C => Ok(Self::F64),
            x => Err(ErrorKind::InvalidType(x)),
        }
    }
}

impl NumType {
    pub fn type_id(self) -> u8 {
        use NumType::*;
        match self {
            I32 => 0x7F,
            I64 => 0x7E,
            F32 => 0x7D,
            F64 => 0x7C,
        }
    }
}

/// The type of a SIMD vector value.
///
/// See [`ValType`] for more info.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VecType {
    /// wasm's `v128` type.
    V128,
}

impl VecType {
    pub fn type_id(self) -> u8 {
        0x7B
    }
}

impl WasmDecode for VecType {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        match u8::decode(buf)? {
            0x7B => Ok(Self::V128),
            x => Err(ErrorKind::InvalidType(x)),
        }
    }
}
