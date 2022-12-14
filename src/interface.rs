use crate::{encode::WasmEncode, tables::TableType, Limit, GlobalType};

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