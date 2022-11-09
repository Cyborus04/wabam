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
            Func { type_idx } => (*type_idx as u32).size(),
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
                (*type_idx as u32).encode(v);
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
    Func(u32),
    Table(u32),
    Memory(u32),
    Global(u32),
}

impl WasmEncode for ExportDesc {
    fn size(&self) -> usize {
        1 + match self {
            ExportDesc::Func(x) => (*x as u32).size(),
            ExportDesc::Table(x) => (*x as u32).size(),
            ExportDesc::Memory(x) => (*x as u32).size(),
            ExportDesc::Global(x) => (*x as u32).size(),
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        match self {
            ExportDesc::Func(x) => {
                v.push(0);
                (*x as u32).encode(v);
            }
            ExportDesc::Table(x) => {
                v.push(1);
                (*x as u32).encode(v);
            }
            ExportDesc::Memory(x) => {
                v.push(2);
                (*x as u32).encode(v);
            }
            ExportDesc::Global(x) => {
                v.push(3);
                (*x as u32).encode(v);
            }
        }
    }
}