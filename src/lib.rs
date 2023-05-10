//! A crate for creating WebAssembly modules. Start at the [`Module`] struct

mod encode;
mod val_types;

pub mod customs;
pub mod functions;
pub mod tables;
pub mod interface;

use customs::CustomSection;
use functions::*;
use tables::*;
use interface::*;
use encode::*;
pub use val_types::*;

#[doc(hidden)]
pub use functions::Instruction as I;

const HEADER: [u8; 8] = *b"\x00asm\x01\x00\x00\x00";

#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub custom_sections: Vec<CustomSection>,
    pub types: Vec<FuncType>,
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    pub tables: Vec<TableType>,
    pub memories: Vec<Limit>,
    pub globals: Vec<Global>,
    pub exports: Vec<Export>,
    pub start: Option<u32>,
    pub elems: Vec<Element>,
    pub datas: Vec<Data>,
}

impl Module {
    pub const EMPTY: Self = Self {
        custom_sections: Vec::new(),
        types: Vec::new(),
        imports: Vec::new(),
        functions: Vec::new(),
        tables: Vec::new(),
        memories: Vec::new(),
        globals: Vec::new(),
        exports: Vec::new(),
        start: None,
        elems: Vec::new(),
        datas: Vec::new(),
    };

    pub fn build(&self) -> Vec<u8> {
        let mut v = Vec::with_capacity(self.size());
        self.encode(&mut v);
        v
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::EMPTY
    }
}

impl WasmEncode for Module {
    fn size(&self) -> usize {
        let customs_size = self.custom_sections.iter().map(|c| 1 + c.size()).sum::<usize>();
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
            } if *mem_index == 0 => mem_index.size() + offset.size() + data.size(),
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
            } if *mem_index == 0 => {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(Module::EMPTY.build(), HEADER);
    }

    #[test]
    fn factorial() {
        let module = Module {
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
                ].into(),
            }],

            exports: vec![Export {
                name: "fac".into(),
                desc: ExportDesc::Func { func_idx: 0 },
            }],

            ..Default::default()
        };

        let result = module.build();

        assert_eq!(result, include_bytes!("tests/fac.wasm"));
    }

    #[test]
    fn macros() {
        assert_eq!(
            func_type!(),
            FuncType::default(),
        );
        assert_eq!(
            func_type!((param i32)),
            FuncType { inputs: vec![ValType::I32], outputs: vec![] }
        );
        assert_eq!(
            func_type!((result i32)),
            FuncType { inputs: vec![], outputs: vec![ValType::I32] }
        );
        let ty = ValType::F32;
        assert_eq!(
            func_type!((param i64 { ty }) (result f64)),
            FuncType { inputs: vec![ValType::I64, ValType::F32], outputs: vec![ValType::F64] }
        );
    }
}
