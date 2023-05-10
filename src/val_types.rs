use crate::encode::{Buf, DecodeError, WasmDecode, WasmEncode};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
    Vec(VecType),
}

impl ValType {
    pub const I32: ValType = Self::Num(NumType::I32);
    pub const I64: ValType = Self::Num(NumType::I64);
    pub const F32: ValType = Self::Num(NumType::F32);
    pub const F64: ValType = Self::Num(NumType::F64);

    #[allow(non_upper_case_globals)]
    pub const FuncRef: ValType = Self::Ref(RefType::FuncRef);
    #[allow(non_upper_case_globals)]
    pub const ExternRef: ValType = Self::Ref(RefType::ExternRef);

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
    fn decode(buf: &mut Buf<'_>) -> Result<Self, DecodeError> {
        let b = u8::decode(buf)?;
        match b {
            0x7F => Ok(Self::I32),
            0x7E => Ok(Self::I64),
            0x7D => Ok(Self::F32),
            0x7C => Ok(Self::F64),
            0x70 => Ok(Self::FuncRef),
            0x6F => Ok(Self::ExternRef),
            0x7B => Ok(Self::V128),
            x => Err(DecodeError::InvalidType(x)),
        }
    }
}

impl WasmDecode for Option<ValType> {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, DecodeError> {
        match u8::decode(buf)? {
            0x7F => Ok(Some(ValType::I32)),
            0x7E => Ok(Some(ValType::I64)),
            0x7D => Ok(Some(ValType::F32)),
            0x7C => Ok(Some(ValType::F64)),
            0x70 => Ok(Some(ValType::FuncRef)),
            0x6F => Ok(Some(ValType::ExternRef)),
            0x7B => Ok(Some(ValType::V128)),
            0x40 => Ok(None),
            x => Err(DecodeError::InvalidType(x)),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefType {
    FuncRef,
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
    fn decode(buf: &mut Buf<'_>) -> Result<Self, DecodeError> {
        match u8::decode(buf)? {
            0x70 => Ok(Self::FuncRef),
            0x6F => Ok(Self::ExternRef),
            x => Err(DecodeError::InvalidType(x)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

impl WasmDecode for NumType {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, DecodeError> {
        match u8::decode(buf)? {
            0x7F => Ok(Self::I32),
            0x7E => Ok(Self::I64),
            0x7D => Ok(Self::F32),
            0x7C => Ok(Self::F64),
            x => Err(DecodeError::InvalidType(x)),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VecType {
    V128,
}

impl VecType {
    pub fn type_id(self) -> u8 {
        0x7B
    }
}

impl WasmDecode for VecType {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, DecodeError> {
        match u8::decode(buf)? {
            0x7B => Ok(Self::V128),
            x => Err(DecodeError::InvalidType(x)),
        }
    }
}
