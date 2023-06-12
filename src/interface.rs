//! [`Imports`] and [`Exports`] are how modules interface with the outside world.

use crate::{
    encode::{WasmDecode, WasmEncode},
    tables::TableType,
    GlobalType, Limit,
};

/// All functions, tables, memories, and globals requested from the host environment.
///
/// They are required to be supplied for instantiation to succeed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Imports {
    pub functions: Vec<FuncImport>,
    pub tables: Vec<TableImport>,
    pub memories: Vec<MemoryImport>,
    pub globals: Vec<GlobalImport>,
}

impl Imports {
    /// The smallest set of imports: nothing.
    pub const fn empty() -> Self {
        Self {
            functions: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
        }
    }

    /// The total number of imports
    pub fn len(&self) -> usize {
        self.functions.len() + self.tables.len() + self.memories.len() + self.globals.len()
    }

    /// Are there no imports?
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for Imports {
    fn default() -> Self {
        Self::empty()
    }
}

impl WasmEncode for Imports {
    fn size(&self) -> usize {
        (self.len() as u32).size()
            + self.functions.iter().map(|x| x.size() + 1).sum::<usize>()
            + self.tables.iter().map(|x| x.size() + 1).sum::<usize>()
            + self.memories.iter().map(|x| x.size() + 1).sum::<usize>()
            + self.globals.iter().map(|x| x.size() + 1).sum::<usize>()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        (self.len() as u32).encode(v);
        // Can't write the len of each vec, so write the elements directly.
        for func in &self.functions {
            func.encode(v);
        }
        for table in &self.tables {
            table.encode(v);
        }
        for memory in &self.memories {
            memory.encode(v);
        }
        for global in &self.globals {
            global.encode(v);
        }
    }
}

impl WasmDecode for Imports {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::ErrorKind> {
        let len = u32::decode(buf)? as usize;
        let mut functions = Vec::with_capacity(len);
        let mut tables = Vec::with_capacity(len);
        let mut memories = Vec::with_capacity(len);
        let mut globals = Vec::with_capacity(len);

        for _ in 0..len {
            let module = String::decode(buf)?;
            let name = String::decode(buf)?;
            let discriminant = u8::decode(buf)?;
            match discriminant {
                0 => {
                    let type_idx = u32::decode(buf)?;
                    functions.push(FuncImport {
                        module,
                        name,
                        type_idx,
                    });
                }
                1 => {
                    let table_type = TableType::decode(buf)?;
                    tables.push(TableImport {
                        module,
                        name,
                        table_type,
                    });
                }
                2 => {
                    let mem_type = Limit::decode(buf)?;
                    memories.push(MemoryImport {
                        module,
                        name,
                        mem_type,
                    });
                }
                3 => {
                    let global_type = GlobalType::decode(buf)?;
                    globals.push(GlobalImport {
                        module,
                        name,
                        global_type,
                    });
                }
                _ => return Err(crate::ErrorKind::InvalidDiscriminant(discriminant)),
            }
        }
        Ok(Self {
            functions,
            tables,
            memories,
            globals,
        })
    }
}

/// A function import, given by its type index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncImport {
    pub module: String,
    pub name: String,
    pub type_idx: u32,
}

impl WasmEncode for FuncImport {
    fn size(&self) -> usize {
        self.module.size() + self.name.size() + self.type_idx.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.module.encode(v);
        self.name.encode(v);
        v.push(0);
        self.type_idx.encode(v);
    }
}

/// A table import, given by its size and type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableImport {
    pub module: String,
    pub name: String,
    pub table_type: TableType,
}

impl WasmEncode for TableImport {
    fn size(&self) -> usize {
        self.module.size() + self.name.size() + self.table_type.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.module.encode(v);
        self.name.encode(v);
        v.push(1);
        self.table_type.encode(v);
    }
}

/// A function import, given by its size.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryImport {
    pub module: String,
    pub name: String,
    pub mem_type: Limit,
}

impl WasmEncode for MemoryImport {
    fn size(&self) -> usize {
        self.module.size() + self.name.size() + self.mem_type.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.module.encode(v);
        self.name.encode(v);
        v.push(2);
        self.mem_type.encode(v);
    }
}

/// A function import, given by its mutability and type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalImport {
    pub module: String,
    pub name: String,
    pub global_type: GlobalType,
}

impl WasmEncode for GlobalImport {
    fn size(&self) -> usize {
        self.module.size() + self.name.size() + self.global_type.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.module.encode(v);
        self.name.encode(v);
        v.push(3);
        self.global_type.encode(v);
    }
}

/// The set of all exports provided by the module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exports {
    pub functions: Vec<Export>,
    pub tables: Vec<Export>,
    pub memories: Vec<Export>,
    pub globals: Vec<Export>,
}

impl Exports {
    /// The smallest set of exports: nothing.
    pub const fn empty() -> Self {
        Self {
            functions: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
        }
    }

    /// The total number of exports
    pub fn len(&self) -> usize {
        self.functions.len() + self.tables.len() + self.memories.len() + self.globals.len()
    }

    /// Are there no exports?
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for Exports {
    fn default() -> Self {
        Self::empty()
    }
}

impl WasmEncode for Exports {
    fn size(&self) -> usize {
        (self.len() as u32).size()
            + self
                .functions
                .iter()
                .map(|x| x.name.size() + 1 + x.idx.size())
                .sum::<usize>()
            + self
                .tables
                .iter()
                .map(|x| x.name.size() + 1 + x.idx.size())
                .sum::<usize>()
            + self
                .memories
                .iter()
                .map(|x| x.name.size() + 1 + x.idx.size())
                .sum::<usize>()
            + self
                .globals
                .iter()
                .map(|x| x.name.size() + 1 + x.idx.size())
                .sum::<usize>()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        (self.len() as u32).encode(v);
        // Can't write the len of each vec, so write the elements directly.
        for func in &self.functions {
            (&func.name, 0u8, &func.idx).encode(v);
        }
        for table in &self.tables {
            (&table.name, 1u8, &table.idx).encode(v);
        }
        for memory in &self.memories {
            (&memory.name, 2u8, &memory.idx).encode(v);
        }
        for global in &self.globals {
            (&global.name, 3u8, &global.idx).encode(v);
        }
    }
}

impl WasmDecode for Exports {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::ErrorKind> {
        let len = u32::decode(buf)? as usize;
        let mut functions = Vec::with_capacity(len);
        let mut tables = Vec::with_capacity(len);
        let mut memories = Vec::with_capacity(len);
        let mut globals = Vec::with_capacity(len);

        for _ in 0..len {
            let name = String::decode(buf)?;
            let discriminant = u8::decode(buf)?;
            let idx = u32::decode(buf)?;
            let export = Export { name, idx };
            match discriminant {
                0 => functions.push(export),
                1 => tables.push(export),
                2 => memories.push(export),
                3 => globals.push(export),
                _ => return Err(crate::ErrorKind::InvalidDiscriminant(discriminant)),
            }
        }
        Ok(Self {
            functions,
            tables,
            memories,
            globals,
        })
    }
}

/// The name and index of an exported item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Export {
    pub name: String,
    pub idx: u32,
}
