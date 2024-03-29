//! A crate for creating WebAssembly modules.
//!
//! # Example
//!
//! ```
//! let mut module = wabam::Module::default();
//!
//! module.types = vec![
//!     wabam::func_type!((param) (result)),
//!     wabam::func_type!((param i32 i32 i32 i32) (result i32)),
//! ];
//!
//! // Import WASI's `fd_write`, to print to the terminal
//! let fd_write = wabam::interface::FuncImport {
//!     module: "wasi_snapshot_preview1".into(),
//!     name: "fd_write".into(),
//!     type_idx: 1, // types[1], see above
//! };
//! module.imports.functions.push(fd_write);
//!
//! // Define memory
//! let memory = wabam::Limit {
//!     start: 1,
//!     end: None,
//! };
//! module.memories.push(memory);
//!
//! let text = "Hello, wasm!";
//! let text_ptr = 12;
//!
//! // Load the text into memory
//! let data = wabam::Data::Active {
//!     mem_index: 0,
//!     offset: wabam::instr!(i32.const { text_ptr }).into(),
//!     data: text.into(),
//! };
//! module.datas.push(data);
//!
//! let body = wabam::instrs!(
//!     (i32.const 0) // Where the `(ptr, len)` pair is
//!     (i32.const { text_ptr })
//!     (i32.store) // Write ptr
//!     
//!     (i32.const 0) // Where the `(ptr, len)` pair is
//!     (i32.const { text.len() as i32 })
//!     (i32.store offset=4) // Write len
//!
//!     (i32.const 1) // File descriptor, stdout is 1.
//!     (i32.const 0) // Where the `(ptr, len)` pair is
//!     (i32.const 1) // How many `(ptr, len)` pairs there are
//!     (i32.const 8) // Where to write the number of written bytes
//!     (call 0) // imported functions are at the start of the address space
//!     // this current function would be 1 (this is important later!)
//!     (drop) // Ignore the error, this is just an example after all!
//! );
//!
//! let func = wabam::functions::Function {
//!     type_idx: 0, // types[0], see above
//!     locals: vec![], // no local variables
//!     body: body.into(),
//! };
//! module.functions.push(func);
//!
//! // Export the start function
//! let func_export = wabam::interface::Export {
//!     name: "_start".into(),
//!     idx: 1, // this is where that's important
//! };
//!
//! // Export memory
//! let mem_export = wabam::interface::Export {
//!     name: "memory".into(),
//!     idx: 0,
//! };
//! module.exports.functions.push(func_export);
//! module.exports.memories.push(mem_export);
//!
//! let output = module.build();
//!
//! # mod std {
//! #    pub mod fs {
//! #        pub fn write(_s: &str, _d: &[u8]) -> Result<(), ()> { Ok(()) }
//! #    }
//! # }
//! std::fs::write("./hello.wasm", &output).unwrap();
//! # ::std::fs::write("./src/tests/hello.wasm", &output).unwrap();
//! ```
//!
//! Then run it:
//!
//! ```txt
//! $ wasmtime ./hello.wasm
//! Hello, wasm!
//! ```

mod encode;
mod val_types;

pub mod customs;
pub mod functions;
pub mod interface;
pub mod tables;

use customs::CustomSection;
use encode::*;
use functions::*;
use interface::*;
use tables::*;
pub use val_types::*;

pub use encode::ErrorKind;

/// An error that occured when reading a module, and where.
/// # Example
/// ```
/// # use wabam::{Module, ErrorKind};
/// let bytes = [1, 2, 3, 4, 5, 6, 7, 8];
///
/// let err = Module::load(&bytes).unwrap_err();
/// assert_eq!(err.kind(), &ErrorKind::BadHeader(bytes));
/// ```
///
/// ```
/// # use wabam::{Module, ErrorKind};
/// let bytes = [1, 2, 3, 4, 5];
///
/// let err = Module::load(&bytes).unwrap_err();
/// assert_eq!(err.kind(), &ErrorKind::TooShort);
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    offset: usize,
    error: ErrorKind,
}

impl Error {
    /// Where in the file the error is
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// What error occured
    pub fn kind(&self) -> &ErrorKind {
        &self.error
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "wasm parsing failed at {:#08X}: {}",
            self.offset, self.error
        )
    }
}

impl std::error::Error for Error {}

#[doc(hidden)]
pub use functions::Instruction as I;

const HEADER: [u8; 8] = *b"\x00asm\x01\x00\x00\x00";

/// A WebAssembly module
///
/// # Building from scratch
///
/// A module can be created by starting with an empty via `Module::default`, then
/// adding components by modified struct fields.
///
/// When the module is finished, it can be assembled into a WebAssembly binary
/// file format with `Module::build`.
///
/// ## Example
/// Creating a simple `add.wasm`
///
/// ```
/// # use wabam::{func_type, instrs, interface::{Export}, functions::Function, Module};
/// let mut module = Module::default();
///
/// let fn_type = func_type!((param i32 i32) (result i32));
/// module.types.push(fn_type);
///
/// let body = instrs!(
///     (local.get 0)
///     (local.get 1)
///     (i32.add)
/// ).into();
///
/// let func = Function {
///     type_idx: 0,
///     locals: vec![],
///     body,
/// };
/// module.functions.push(func);
///
/// let export = Export {
///     name: "add".into(),
///     idx: 0,
/// };
/// module.exports.functions.push(export);
///
/// let wasm = module.build();
///
/// # mod std {
/// #    pub mod fs {
/// #        pub fn write(_s: &str, _d: &[u8]) -> Result<(), ()> { Ok(()) }
/// #    }
/// # }
/// std::fs::write("./add.wasm", &wasm).unwrap();
/// # ::std::fs::write("./src/tests/add.wasm", &wasm).unwrap();
/// ```
///
/// # Loading modules
///
/// Modules can be loaded from the WebAssembly binary format with `Module::load`,
/// then modified or read from as usual.
///
/// ## Example
///
/// ```
/// # use wabam::Module;
/// # mod std {
/// #    pub mod fs {
/// #        pub fn read(_s: &str) -> Result<(), ()> { Ok(()) }
/// #    }
/// # }
/// let bytes = std::fs::read("./add.wasm").unwrap();
/// # let bytes = ::std::fs::read("./src/tests/add.wasm").unwrap();
/// let module = Module::load(&bytes).unwrap();
///
/// assert_eq!(module.functions.len(), 1);
/// assert_eq!(module.exports.functions[0].name, "add");
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub custom_sections: Vec<CustomSection>,
    pub types: Vec<FuncType>,
    pub imports: Imports,
    pub functions: Vec<Function>,
    pub tables: Vec<TableType>,
    pub memories: Vec<Limit>,
    pub globals: Vec<Global>,
    pub exports: Exports,
    pub start: Option<u32>,
    pub elems: Vec<Element>,
    pub datas: Vec<Data>,
}

impl Module {
    /// An entirely empty module.
    /// 
    /// Calling `Module::build` on this will just return the 8-byte header.
    pub const EMPTY: Self = Self {
        custom_sections: Vec::new(),
        types: Vec::new(),
        imports: Imports::empty(),
        functions: Vec::new(),
        tables: Vec::new(),
        memories: Vec::new(),
        globals: Vec::new(),
        exports: Exports::empty(),
        start: None,
        elems: Vec::new(),
        datas: Vec::new(),
    };

    /// An entirely empty module.
    /// 
    /// Calling `Module::build` on this will just return the 8-byte header.
    pub const fn empty() -> Self {
        Self::EMPTY
    }

    /// Assembles the module into the wasm binary format.
    pub fn build(&self) -> Vec<u8> {
        let mut v = Vec::with_capacity(self.size());
        self.encode(&mut v);
        v
    }

    /// Disassembles a binary wasm file
    pub fn load(data: &[u8]) -> Result<Self, Error> {
        let mut buf = Buf::new(data);
        Module::decode(&mut buf)
    }

    fn decode(buf: &mut Buf<'_>) -> Result<Self, Error> {
        let header = <[u8; 8]>::decode(buf).map_err(|e| e.at(buf))?;
        if header != HEADER {
            return Err(ErrorKind::BadHeader(header).at(buf));
        }

        fn load_section<T: WasmDecode>(mut buf: Buf) -> Result<T, Error> {
            T::decode(&mut buf).map_err(|e| e.at(&buf))
        }

        let mut module = Module::EMPTY;

        let mut functions = None;
        let mut code = None;

        let mut last_section = 0;
        while !buf.exhausted() {
            let section_id = u8::decode(buf).map_err(|e| e.at(buf))?;
            if section_id > 0 && section_id <= last_section {
                return Err(ErrorKind::SectionOutOfOrder {
                    prev: last_section,
                    this: section_id,
                }
                .at(buf));
            }
            let len = u32::decode(buf).map_err(|e| e.at(buf))?;
            let section_start = buf.consumed();
            let section_bytes = buf
                .take(len as usize)
                .ok_or(ErrorKind::TooShort)
                .map_err(|e| e.at(buf))?;
            let mut section = Buf::with_consumed(section_bytes, section_start);
            match section_id {
                0 => module
                    .custom_sections
                    .push(load_section::<CustomSection>(section)?),
                1 => module.types = load_section::<Vec<FuncType>>(section)?,
                2 => module.imports = load_section::<Imports>(section)?,
                3 => functions = Some(section),
                4 => module.tables = load_section::<Vec<TableType>>(section)?,
                5 => module.memories = load_section::<Vec<Limit>>(section)?,
                6 => module.globals = load_section::<Vec<Global>>(section)?,
                7 => module.exports = load_section::<Exports>(section)?,
                8 => module.start = Some(u32::decode(&mut section).map_err(|e| e.at(buf))?),
                9 => module.elems = load_section::<Vec<Element>>(section)?,
                10 => code = Some(section),
                11 => module.datas = load_section::<Vec<Data>>(section)?,
                _ => return Err(ErrorKind::InvalidSectionId(section_id).at(buf)),
            }

            last_section = section_id;
        }

        match (functions, code) {
            (None, None) => (/* nothing changed */),
            (None, Some(_)) => return Err(ErrorKind::FuncWithoutCode.at(buf)),
            (Some(_), None) => return Err(ErrorKind::CodeWithoutFunc.at(buf)),
            (Some(mut functions), Some(mut code)) => {
                module.functions =
                    Function::decode(&mut functions, &mut code).map_err(|e| e.at(buf))?
            }
        }

        Ok(module)
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::EMPTY
    }
}

impl WasmEncode for Module {
    fn size(&self) -> usize {
        let customs_size = self
            .custom_sections
            .iter()
            .map(|c| 1 + c.size())
            .sum::<usize>();
        let type_size = 1 + self.types.size();
        let import_size = 1 + self.imports.size();
        let func_size = 1 + Function::size_func(&self.functions);
        let table_size = 1 + self.tables.size();
        let memory_size = 1 + self.memories.size();
        let global_size = 1 + self.globals.size();
        let export_size = 1 + self.exports.size();
        let start_size = 1 + self.start.map(|x| x.size()).unwrap_or_default();
        let element_size = 1 + self.elems.size();
        let code_size = 1 + Function::size_code(&self.functions);
        let data_size = 1 + self.datas.size();

        HEADER.len()
            + customs_size
            + type_size
            + import_size
            + func_size
            + table_size
            + memory_size
            + global_size
            + export_size
            + start_size
            + element_size
            + code_size
            + data_size
    }

    fn encode(&self, v: &mut Vec<u8>) {
        HEADER.encode(v);
        macro_rules! encode_section {
            ($x:ident: $id:literal) => {
                if !self.$x.is_empty() {
                    v.push($id);
                    (self.$x.size() as u32).encode(v);
                    self.$x.encode(v);
                }
            };
        }

        encode_section!(types: 1);
        encode_section!(imports: 2);

        if !self.functions.is_empty() {
            v.push(3);
            (Function::size_func(&self.functions) as u32).encode(v);
            Function::encode_func(&self.functions, v);
        }

        encode_section!(tables: 4);
        encode_section!(memories: 5);
        encode_section!(globals: 6);
        encode_section!(exports: 7);

        if let Some(start_idx) = self.start {
            v.push(8);
            (start_idx.size() as u32).encode(v);
            start_idx.encode(v);
        }

        encode_section!(elems: 9);

        if !self.functions.is_empty() {
            v.push(10);
            (Function::size_code(&self.functions) as u32).encode(v);
            Function::encode_code(&self.functions, v);
        }

        encode_section!(datas: 11);

        for i in &self.custom_sections {
            v.push(0);
            (i.size() as u32).encode(v);
            i.encode(v);
        }
    }
}

/// The minimum and optional maximum sizes of tables and memories.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Limit {
    pub start: u32,
    pub end: Option<u32>,
}

impl WasmEncode for Limit {
    fn size(&self) -> usize {
        1 + self.start.size()
            + match self.end {
                Some(end) => end.size(),
                None => 0,
            }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        match self.end {
            Some(end) => {
                v.push(1);
                self.start.encode(v);
                end.encode(v);
            }
            None => {
                v.push(0);
                self.start.encode(v);
            }
        }
    }
}

impl WasmDecode for Limit {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let d = u8::decode(buf)?;
        match d {
            0 => Ok(Self {
                start: u32::decode(buf)?,
                end: None,
            }),
            1 => Ok(Self {
                start: u32::decode(buf)?,
                end: Some(u32::decode(buf)?),
            }),
            _ => Err(ErrorKind::InvalidDiscriminant(d)),
        }
    }
}

/// The value type and mutability of global variables.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct GlobalType {
    pub mutable: bool,
    pub vtype: ValType,
}

impl WasmEncode for GlobalType {
    fn size(&self) -> usize {
        2
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.vtype.encode(v);
        self.mutable.encode(v);
    }
}

impl WasmDecode for GlobalType {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let vtype = ValType::decode(buf)?;
        let mutable = bool::decode(buf)?;
        Ok(Self { mutable, vtype })
    }
}

/// A global variable
#[derive(PartialEq, Debug, Clone)]
pub struct Global {
    pub global_type: GlobalType,
    pub expr: Expr,
}

impl WasmEncode for Global {
    fn size(&self) -> usize {
        self.global_type.size() + self.expr.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.global_type.encode(v);
        self.expr.encode(v);
    }
}

impl WasmDecode for Global {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let global_type = GlobalType::decode(buf)?;
        let expr = Expr::decode(buf)?;
        Ok(Self { global_type, expr })
    }
}

/// A series of bytes to be loaded into linear memory
///
/// Linear memory is initialized to all zeroes, these are used to load data into it.
///
/// Active data segments are loaded at instantiation-time, while passive
/// segments are loaded at run-time using the `memory.init` instruction.
#[derive(PartialEq, Debug, Clone)]
pub enum Data {
    Active {
        mem_index: u32,
        offset: Expr,
        data: Vec<u8>,
    },
    Passive(Vec<u8>),
}

impl WasmEncode for Data {
    fn size(&self) -> usize {
        use Data::*;
        1 + match self {
            Active {
                mem_index,
                offset,
                data,
            } if *mem_index != 0 => mem_index.size() + offset.size() + data.size(),
            Active {
                mem_index: _,
                offset,
                data,
            } => offset.size() + data.size(),
            Passive(data) => data.size(),
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        use Data::*;
        match self {
            Active {
                mem_index,
                offset,
                data,
            } if *mem_index != 0 => {
                v.push(2);
                mem_index.encode(v);
                offset.encode(v);
                data.encode(v);
            }
            Active {
                mem_index: _,
                offset,
                data,
            } => {
                v.push(0);
                offset.encode(v);
                data.encode(v);
            }
            Passive(data) => {
                v.push(1);
                data.encode(v);
            }
        }
    }
}

impl WasmDecode for Data {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let d = u8::decode(buf)?;
        let data = match d {
            0 => Self::Active {
                mem_index: 0,
                offset: Expr::decode(buf)?,
                data: Vec::<u8>::decode(buf)?,
            },
            1 => Self::Passive(Vec::<u8>::decode(buf)?),
            2 => Self::Active {
                mem_index: u32::decode(buf)?,
                offset: Expr::decode(buf)?,
                data: Vec::<u8>::decode(buf)?,
            },
            _ => return Err(ErrorKind::InvalidDiscriminant(d)),
        };
        Ok(data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(Module::EMPTY.build(), HEADER);
    }

    fn make_factorial_module() -> Module {
        Module {
            types: vec![FuncType {
                inputs: vec![ValType::I64],
                outputs: vec![ValType::I64],
            }],
            functions: vec![Function {
                type_idx: 0,
                locals: vec![],
                body: vec![
                    Instruction::LocalGet(0),
                    Instruction::I64Const(1),
                    Instruction::U64LessThan,
                    Instruction::If(Some(ValType::I64)),
                    Instruction::I64Const(1),
                    Instruction::Else,
                    Instruction::LocalGet(0),
                    Instruction::LocalGet(0),
                    Instruction::I64Const(1),
                    Instruction::I64Sub,
                    Instruction::Call(0),
                    Instruction::I64Mul,
                    Instruction::End,
                ]
                .into(),
            }],

            exports: Exports {
                functions: vec![Export {
                    name: "fac".into(),
                    idx: 0,
                }],
                ..Default::default()
            },

            ..Default::default()
        }
    }

    #[test]
    fn factorial() {
        let module = make_factorial_module();
        let result = module.build();

        assert_eq!(result, include_bytes!("tests/fac.wasm"));
    }

    #[test]
    fn macros() {
        assert_eq!(func_type!(), FuncType::default(),);
        assert_eq!(
            func_type!((param i32)),
            FuncType {
                inputs: vec![ValType::I32],
                outputs: vec![]
            }
        );
        assert_eq!(
            func_type!((result i32)),
            FuncType {
                inputs: vec![],
                outputs: vec![ValType::I32]
            }
        );
        let ty = ValType::F32;
        assert_eq!(
            func_type!((param i64 { ty }) (result f64)),
            FuncType {
                inputs: vec![ValType::I64, ValType::F32],
                outputs: vec![ValType::F64]
            }
        );
    }

    #[test]
    fn read_factorial() {
        let bytes = include_bytes!("tests/fac.wasm");
        let module = Module::load(bytes).unwrap();
        let expected = make_factorial_module();
        assert_eq!(module, expected);
    }

    #[test]
    fn read_empty() {
        let bytes = [0, b'a', b's', b'm', 1, 0, 0, 0];
        let module = Module::load(&bytes).unwrap();
        assert_eq!(module, Module::EMPTY);
    }
}
