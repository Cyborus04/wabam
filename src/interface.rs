use crate::{
    encode::{WasmDecode, WasmEncode},
    tables::TableType,
    GlobalType, Limit,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

impl WasmEncode for Import {
    fn size(&self) -> usize {
        self.module.size() + self.name.size() + self.desc.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.module.encode(v);
        self.name.encode(v);
        self.desc.encode(v);
    }
}

impl WasmDecode for Import {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::DecodeError> {
        let module = String::decode(buf)?;
        let name = String::decode(buf)?;
        let desc = ImportDesc::decode(buf)?;
        Ok(Self { module, name, desc })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ImportDesc {
    Func { type_idx: u32 },
    Table { table_type: TableType },
    Memory { mem_type: Limit },
    Global { global_type: GlobalType },
}

impl WasmEncode for ImportDesc {
    fn size(&self) -> usize {
        use ImportDesc::*;
        1 + match self {
            Func { type_idx } => type_idx.size(),
            Table { table_type } => table_type.size(),
            Memory { mem_type } => mem_type.size(),
            Global { global_type } => global_type.size(),
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        use ImportDesc::*;
        match self {
            Func { type_idx } => {
                v.push(0);
                type_idx.encode(v);
            }
            Table { table_type } => {
                v.push(1);
                table_type.encode(v);
            }
            Memory { mem_type } => {
                v.push(2);
                mem_type.encode(v);
            }
            Global { global_type } => {
                v.push(3);
                global_type.encode(v);
            }
        }
    }
}

impl WasmDecode for ImportDesc {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::DecodeError> {
        let d = u8::decode(buf)?;
        match d {
            0 => Ok(Self::Func {
                type_idx: u32::decode(buf)?,
            }),
            1 => Ok(Self::Table {
                table_type: TableType::decode(buf)?,
            }),
            2 => Ok(Self::Memory {
                mem_type: Limit::decode(buf)?,
            }),
            3 => Ok(Self::Global {
                global_type: GlobalType::decode(buf)?,
            }),
            _ => Err(crate::encode::DecodeError::InvalidDiscriminant(d)),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Export {
    pub name: String,
    pub desc: ExportDesc,
}

impl WasmEncode for Export {
    fn size(&self) -> usize {
        self.name.size() + self.desc.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.name.encode(v);
        self.desc.encode(v);
    }
}

impl WasmDecode for Export {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::DecodeError> {
        let name = String::decode(buf)?;
        let desc = ExportDesc::decode(buf)?;
        Ok(Self { name, desc })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ExportDesc {
    Func { func_idx: u32 },
    Table { table_idx: u32 },
    Memory { mem_idx: u32 },
    Global { global_idx: u32 },
}

impl WasmEncode for ExportDesc {
    fn size(&self) -> usize {
        1 + match self {
            ExportDesc::Func { func_idx } => func_idx.size(),
            ExportDesc::Table { table_idx } => table_idx.size(),
            ExportDesc::Memory { mem_idx } => mem_idx.size(),
            ExportDesc::Global { global_idx } => global_idx.size(),
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        match self {
            ExportDesc::Func { func_idx } => {
                v.push(0);
                func_idx.encode(v);
            }
            ExportDesc::Table { table_idx } => {
                v.push(1);
                table_idx.encode(v);
            }
            ExportDesc::Memory { mem_idx } => {
                v.push(2);
                mem_idx.encode(v);
            }
            ExportDesc::Global { global_idx } => {
                v.push(3);
                global_idx.encode(v);
            }
        }
    }
}

impl WasmDecode for ExportDesc {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::DecodeError> {
        let d = u8::decode(buf)?;
        match d {
            0 => Ok(Self::Func {
                func_idx: u32::decode(buf)?,
            }),
            1 => Ok(Self::Table {
                table_idx: u32::decode(buf)?,
            }),
            2 => Ok(Self::Memory {
                mem_idx: u32::decode(buf)?,
            }),
            3 => Ok(Self::Global {
                global_idx: u32::decode(buf)?,
            }),
            _ => Err(crate::encode::DecodeError::InvalidDiscriminant(d)),
        }
    }
}
