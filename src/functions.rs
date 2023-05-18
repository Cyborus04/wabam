//! The main star of a module, code!

use crate::{
    encode::{Buf, ErrorKind, WasmDecode},
    ValType, WasmEncode,
};

mod instructions;
pub use instructions::*;

/// A callable or exportable expression with local variables.
// Yeah, you try describing what a function is!
#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub type_idx: u32,
    pub locals: Vec<ValType>,
    pub body: Expr,
}

impl Function {
    pub(crate) fn size_func(these: &[Self]) -> usize {
        let size = these.iter().map(|func| func.type_idx.size()).sum::<usize>();

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

    pub(crate) fn decode(
        func_buf: &mut Buf<'_>,
        code_buf: &mut Buf<'_>,
    ) -> Result<Vec<Self>, ErrorKind> {
        let func_len = u32::decode(func_buf)?;
        let code_len = u32::decode(code_buf)?;
        if func_len != code_len {
            return Err(ErrorKind::FuncCodeMismatch { func_len, code_len });
        }

        let mut out = Vec::new();
        for _ in 0..func_len {
            let type_idx = u32::decode(func_buf)?;

            let _code_size = u32::decode(code_buf)?;

            let mut locals = Vec::new();
            let local_len = u32::decode(code_buf)?;
            for _ in 0..local_len {
                let count = u32::decode(code_buf)?;
                let val_type = ValType::decode(code_buf)?;
                locals.extend(std::iter::repeat(val_type).take(count as usize));
            }

            let body = Expr::decode(code_buf)?;

            out.push(Function {
                type_idx,
                locals,
                body,
            });
        }

        Ok(out)
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

/// A function signature, mapping inputs types to output types.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct FuncType {
    pub inputs: Vec<ValType>,
    pub outputs: Vec<ValType>,
}

impl FuncType {
    pub const EMPTY: Self = Self {
        inputs: vec![],
        outputs: vec![],
    };
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

impl WasmDecode for FuncType {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, crate::encode::ErrorKind> {
        let tag = u8::decode(buf)?;
        if tag != 0x60 {
            return Err(ErrorKind::InvalidDiscriminant(tag));
        }
        let inputs = Vec::<ValType>::decode(buf)?;
        let outputs = Vec::<ValType>::decode(buf)?;
        Ok(Self { inputs, outputs })
    }
}

/// A macro to define a [`FuncType`].
#[macro_export]
macro_rules! func_type {
    () => {
        $crate::functions::FuncType::default()
    };
    ((param $($in:tt)*)) => {
        $crate::functions::FuncType {
            inputs: vec![
                $(
                    $crate::mlt!($in),
                )*
            ],
            outputs: vec![],
        }
    };
    ((result $($out:tt)*)) => {
        $crate::functions::FuncType {
            inputs: vec![],
            outputs: vec![
                $(
                    $crate::mlt!($out),
                )*
            ],
        }
    };
    ((param $($in:tt)*) (result $($out:tt)*)) => {
        $crate::functions::FuncType {
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
