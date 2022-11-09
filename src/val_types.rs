use crate::encode::WasmEncode;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
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