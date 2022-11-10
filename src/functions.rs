use crate::{ValType, WasmEncode};

mod instructions;
pub use instructions::*;

pub struct Function {
    pub type_idx: u32,
    pub locals: Vec<ValType>,
    pub body: Expr,
}

impl Function {
    pub(crate) fn size_func(these: &[Self]) -> usize {
        let size = these
            .iter()
            .map(|func| func.type_idx.size())
            .sum::<usize>();

        size + (these.len() as u32).size()
    }

    pub(crate) fn encode_func(these: &[Self], v: &mut Vec<u8>) {
        (these.len() as u32).encode(v);

        for func in these {
            func.type_idx.encode(v);
        }
    }

    pub(crate) fn size_code(these: &[Self]) -> usize {
        let mut size = (these.len() as u32).size();
        for func in these {
            let locals = compress_locals(&func.locals);
            size += ((locals.size() + func.body.size()) as u32).size();
            size += locals.size();
            size += func.body.size();
        }
        size
    }

    pub(crate) fn encode_code(these: &[Self], v: &mut Vec<u8>) {
        (these.len() as u32).encode(v);
        for func in these {
            let locals = compress_locals(&func.locals);
            ((locals.size() + func.body.size()) as u32).encode(v);
            locals.encode(v);
            func.body.encode(v);
        }
    }
}

fn compress_locals(locals: &[ValType]) -> Vec<(u32, ValType)> {
    let mut locals_compressed = Vec::new();
    for local in locals {
        match locals_compressed.last_mut() {
            Some((count, ty)) if ty == local => *count += 1,
            _ => locals_compressed.push((1, *local)),
        }
    }
    locals_compressed
}


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct FuncType {
    pub inputs: Vec<ValType>,
    pub outputs: Vec<ValType>,
}

impl FuncType {
    pub const EMPTY: Self = Self { inputs: vec![], outputs: vec![] };
}

impl WasmEncode for FuncType {
    fn size(&self) -> usize {
        1 + self.inputs.size() + self.outputs.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        v.push(0x60);
        self.inputs.encode(v);
        self.outputs.encode(v);
    }
}

#[macro_export]
macro_rules! func_type {
    () => {
        $crate::FuncType::default()
    };
    ((param $($in:tt)*)) => {
        $crate::FuncType {
            inputs: vec![
                $(
                    $crate::mlt!($in),
                )*
            ],
            outputs: vec![],
        }
    };
    ((result $($out:tt)*)) => {
        $crate::FuncType {
            inputs: vec![],
            outputs: vec![
                $(
                    $crate::mlt!($out),
                )*
            ],
        }
    };
    ((param $($in:tt)*) (result $($out:tt)*)) => {
        $crate::FuncType {
            inputs: vec![
                $(
                    $crate::mlt!($in),
                )*
            ],
            outputs: vec![
                $(
                    $crate::mlt!($out),
                )*
            ],
        }
    }
}