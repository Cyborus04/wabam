//! A crate for creating WebAssembly modules. Start at the [`Module`] struct

mod instructions;

pub use instructions::{Expr, Instruction};

const HEADER: [u8; 8] = *b"\x00asm\x01\x00\x00\x00";

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
        let start_size = 1 + self.start.map(|x| (x as u32).size()).unwrap_or_default();
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

pub struct CustomSection {
    pub name: String,
    pub data: Vec<u8>,
}

impl WasmEncode for CustomSection {
    fn size(&self) -> usize {
        self.name.size() + self.data.len()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.name.encode(v);
        v.extend_from_slice(&self.data);
    }
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

pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDescription,
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

pub enum ImportDescription {
    Func { type_idx: u32 },
    Table { table_type: TableType },
    Memory { mem_type: Limit },
    Global { global_type: GlobalType },
}

impl WasmEncode for ImportDescription {
    fn size(&self) -> usize {
        use ImportDescription::*;
        1 + match self {
            Func { type_idx } => (*type_idx as u32).size(),
            Table { table_type } => table_type.size(),
            Memory { mem_type } => mem_type.size(),
            Global { global_type } => global_type.size(),
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        use ImportDescription::*;
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

pub struct TableType {
    pub ref_type: RefType,
    pub limits: Limit,
}

impl WasmEncode for TableType {
    fn size(&self) -> usize {
        self.ref_type.size() + self.limits.size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.ref_type.encode(v);
        self.limits.encode(v);
    }
}

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

pub struct Element {
    pub kind: ElemKind,
    pub init: ElemInit,
    pub mode: ElemMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElemKind {
    FuncRef,
}

pub enum ElemInit {
    Indices(Vec<u32>),
    Expressions(Vec<Expr>),
}

pub enum ElemMode {
    Active { table_idx: u32, offset: Expr },
    Passive,
    Declarative,
}

impl WasmEncode for Element {
    fn size(&self) -> usize {
        use ElemInit::*;
        use ElemMode::*;
        1 + match *self {
            // 0
            Element {
                kind,
                init: Indices(ref indices),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } if kind == ElemKind::FuncRef && table_idx == 0 => offset.size() + indices.size(),

            // 1
            Element {
                kind: _,
                init: Indices(ref indices),
                mode: Passive,
            } => 1 + indices.size(),

            // 2
            Element {
                kind: _,
                init: Indices(ref indices),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } => table_idx.size() + offset.size() + 1 + indices.size(),

            // 3
            Element {
                kind: _,
                init: Indices(ref indices),
                mode: Declarative,
            } => 1 + indices.size(),

            // 4
            Element {
                kind,
                init: Expressions(ref exprs),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } if kind == ElemKind::FuncRef && table_idx == 0 => offset.size() + exprs.size(),

            // 5
            Element {
                kind: _,
                init: Expressions(ref exprs),
                mode: Passive,
            } => 1 + exprs.size(),

            // 6
            Element {
                kind: _,
                init: Expressions(ref exprs),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } => table_idx.size() + offset.size() + 1 + exprs.size(),

            // 7
            Element {
                kind: _,
                init: Expressions(ref exprs),
                mode: Declarative,
            } => 1 + exprs.size(),
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        use ElemInit::*;
        use ElemMode::*;
        match *self {
            // 0
            Element {
                kind,
                init: Indices(ref indices),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } if kind == ElemKind::FuncRef && table_idx == 0 => {
                v.push(0);
                offset.encode(v);
                indices.encode(v);
            }

            // 1
            Element {
                kind: _,
                init: Indices(ref indices),
                mode: Passive,
            } => {
                v.push(1);
                v.push(0);
                indices.encode(v)
            }

            // 2
            Element {
                kind: _,
                init: Indices(ref indices),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } => {
                v.push(2);
                table_idx.encode(v);
                offset.encode(v);
                v.push(0);
                indices.encode(v);
            }

            // 3
            Element {
                kind: _,
                init: Indices(ref indices),
                mode: Declarative,
            } => {
                v.push(3);
                v.push(0);
                indices.encode(v);
            }

            // 4
            Element {
                kind,
                init: Expressions(ref exprs),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } if kind == ElemKind::FuncRef && table_idx == 0 => {
                v.push(4);
                offset.encode(v);
                exprs.encode(v);
            }

            // 5
            Element {
                kind: _,
                init: Expressions(ref exprs),
                mode: Passive,
            } => {
                v.push(5);
                v.push(0);
                exprs.encode(v);
            }

            // 6
            Element {
                kind: _,
                init: Expressions(ref exprs),
                mode:
                    Active {
                        table_idx,
                        ref offset,
                    },
            } => {
                v.push(6);
                table_idx.encode(v);
                offset.encode(v);
                v.push(0);
                exprs.encode(v);
            }

            // 7
            Element {
                kind: _,
                init: Expressions(ref exprs),
                mode: Declarative,
            } => {
                v.push(7);
                v.push(0);
                exprs.encode(v);
            }
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

pub struct Function {
    pub type_idx: u32,
    pub locals: Vec<ValType>,
    pub body: Expr,
}

impl Function {
    fn size_func(these: &[Self]) -> usize {
        let size = these
            .iter()
            .map(|func| (func.type_idx as u32).size())
            .sum::<usize>();

        size + (these.len() as u32).size()
    }

    fn encode_func(these: &[Self], v: &mut Vec<u8>) {
        (these.len() as u32).encode(v);

        for func in these {
            (func.type_idx as u32).encode(v);
        }
    }

    fn size_code(these: &[Self]) -> usize {
        let mut size = (these.len() as u32).size();
        for func in these {
            let locals = compress_locals(&func.locals);
            size += ((locals.size() + func.body.size()) as u32).size();
            size += locals.size();
            size += func.body.size();
        }
        size
    }

    fn encode_code(these: &[Self], v: &mut Vec<u8>) {
        (these.len() as u32).encode(v);
        for func in these {
            let locals = compress_locals(&func.locals);
            ((locals.size() + func.body.size()) as u32).encode(v);
            locals.encode(v);
            func.body.encode(v);
        }
    }
}

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
            } if *mem_index == 0 => (*mem_index as u32).size() + offset.size() + data.size(),
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
                (*mem_index as u32).encode(v);
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

trait WasmEncode {
    fn size(&self) -> usize;
    fn encode(&self, v: &mut Vec<u8>);
}

impl<T: WasmEncode> WasmEncode for &T {
    fn size(&self) -> usize {
        T::size(self)
    }

    fn encode(&self, v: &mut Vec<u8>) {
        T::encode(self, v);
    }
}

macro_rules! wasm_encode_tuples {
    ($(($($t:ident $x:ident),*);)*) => {
        $(
            impl<$($t: WasmEncode,)*> WasmEncode for ($($t,)*) {
                fn size(&self) -> usize {
                    let ($($x,)*) = self;
                    0 $(+ $x.size())*
                }

                #[allow(unused)]
                fn encode(&self, v: &mut Vec<u8>) {
                    let ($($x,)*) = self;
                    $($x.encode(v);)*
                }
            }
        )*
    };
}

wasm_encode_tuples! {
    ();
    (A a);
    (A a, B b);
    (A a, B b, C c);
    (A a, B b, C c, D d);
    (A a, B b, C c, D d, E e);
    (A a, B b, C c, D d, E e, F f);
    (A a, B b, C c, D d, E e, F f, G g);
    (A a, B b, C c, D d, E e, F f, G g, H h);
    (A a, B b, C c, D d, E e, F f, G g, H h, I i);
    (A a, B b, C c, D d, E e, F f, G g, H h, I i, J j);
    (A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k);
    (A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k, L l);
}

impl<T: WasmEncode, const N: usize> WasmEncode for [T; N] {
    fn size(&self) -> usize {
        self.iter().map(|x| x.size()).sum::<usize>()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        for i in self {
            i.encode(v);
        }
    }
}

impl<T: WasmEncode> WasmEncode for [T] {
    fn size(&self) -> usize {
        (self.len() as u32).size() + self.iter().map(|x| x.size()).sum::<usize>()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        (self.len() as u32).encode(v);
        for i in self {
            i.encode(v)
        }
    }
}

impl<T: WasmEncode> WasmEncode for Vec<T> {
    fn size(&self) -> usize {
        self.as_slice().size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.as_slice().encode(v)
    }
}

impl WasmEncode for str {
    fn size(&self) -> usize {
        (self.len() as u32).size() + self.len()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.as_bytes().encode(v)
    }
}

impl WasmEncode for String {
    fn size(&self) -> usize {
        self.as_str().size()
    }

    fn encode(&self, v: &mut Vec<u8>) {
        self.as_str().encode(v)
    }
}

impl WasmEncode for u8 {
    fn size(&self) -> usize {
        1
    }

    fn encode(&self, v: &mut Vec<u8>) {
        v.push(*self);
    }
}

impl WasmEncode for bool {
    fn size(&self) -> usize {
        1
    }

    fn encode(&self, v: &mut Vec<u8>) {
        v.push(*self as u8);
    }
}

impl WasmEncode for u32 {
    fn size(&self) -> usize {
        match *self {
            0..=127 => 1,
            128..=16383 => 2,
            16384..=2097151 => 3,
            2097152..=268435455 => 4,
            268435456.. => 5,
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        let mut x = *self;
        for _ in 0..5 {
            let byte = x as u8 & 0x7f;
            x = x.wrapping_shr(7);

            if x == 0 {
                v.push(byte);
                break;
            } else {
                v.push(byte | 0x80);
            }
        }
    }
}

impl WasmEncode for i32 {
    fn size(&self) -> usize {
        match *self {
            -64..=63 => 1,
            -8192..=-65 | 64..=8191 => 2,
            -1048576..=-8193 | 8192..=1048575 => 3,
            -134217728..=-1048577 | 1048576..=134217727 => 4,
            -2147483648..=-134217729 | 134217728.. => 5,
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        let mut x = *self;
        for _ in 0..5 {
            let byte = x as u8 & 0x7f;
            x = x.wrapping_shr(7);

            if (x == 0 && byte & 0x40 == 0) || (x == -1 && byte & 0x40 != 0) {
                v.push(byte);
                break;
            } else {
                v.push(byte | 0x80);
            }
        }
    }
}

impl WasmEncode for u64 {
    fn size(&self) -> usize {
        match *self {
            0..=127 => 1,
            128..=16383 => 2,
            16384..=2097151 => 3,
            2097152..=268435455 => 4,
            268435456..=34359738367 => 5,
            34359738368..=4398046511103 => 6,
            4398046511104..=562949953421311 => 7,
            562949953421312..=72057594037927935 => 8,
            72057594037927936..=9223372036854775807 => 9,
            9223372036854775808.. => 10,
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        let mut x = *self;
        for _ in 0..10 {
            let byte = x as u8 & 0x7f;
            x = x.wrapping_shr(7);

            if x == 0 && byte & 0x40 == 0 {
                v.push(byte);
                break;
            } else {
                v.push(byte | 0x80);
            }
        }
    }
}

impl WasmEncode for i64 {
    fn size(&self) -> usize {
        match *self {
            -64..=63 => 1,
            -8192..=-65 | 64..=8191 => 2,
            -1048576..=-8193 | 8192..=1048575 => 3,
            -134217728..=-1048577 | 1048576..=134217727 => 4,
            -17179869184..=-134217729 | 134217728..=17179869183 => 5,
            -2199023255552..=-17179869185 | 17179869184..=2199023255551 => 6,
            -281474976710656..=-2199023255553 | 2199023255552..=281474976710655 => 7,
            -36028797018963968..=-281474976710657 | 281474976710656..=36028797018963967 => 8,
            -4611686018427387904..=-36028797018963969 | 36028797018963968..=4611686018427387903 => {
                9
            }
            -9223372036854775808..=-4611686018427387905 | 4611686018427387904.. => 10,
        }
    }

    fn encode(&self, v: &mut Vec<u8>) {
        let mut x = *self;
        for _ in 0..10 {
            let byte = x as u8 & 0x7f;
            x = x.wrapping_shr(7);

            if x == 0 && byte & 0x40 == 0 {
                v.push(byte);
                break;
            } else {
                v.push(byte | 0x80);
            }
        }
    }
}

impl WasmEncode for f32 {
    fn size(&self) -> usize {
        4
    }

    fn encode(&self, v: &mut Vec<u8>) {
        v.extend(self.to_le_bytes())
    }
}

impl WasmEncode for f64 {
    fn size(&self) -> usize {
        8
    }

    fn encode(&self, v: &mut Vec<u8>) {
        v.extend(self.to_le_bytes())
    }
}

pub struct NameSection {
    pub module_name: Option<String>,
    pub function_names: Vec<(u32, String)>,
    pub local_names: Vec<(u32, Vec<(u32, String)>)>,
}

impl NameSection {
    pub fn to_custom(&self) -> CustomSection {
        let mut data = Vec::with_capacity(self.size());
        self.encode(&mut data);
        CustomSection { name: "name".into(), data }
    }
}

impl WasmEncode for NameSection {
    fn size(&self) -> usize {
        let mut size = 0;
        if let Some(module_name) = &self.module_name {
            size += 1 + (module_name.size() as u32).size() + module_name.size();
        }
        if !self.function_names.is_empty() {
            size += 1 + (self.function_names.size() as u32).size() + self.function_names.size();
        }
        if !self.local_names.is_empty() {
            size += 1 + (self.local_names.size() as u32).size() + self.local_names.size();
        }
        size
    }

    fn encode(&self, v: &mut Vec<u8>) {
        if let Some(module_name) = &self.module_name {
            v.push(0);
            (module_name.size() as u32).encode(v);
            module_name.encode(v);
        }
        if !self.function_names.is_empty() {
            v.push(1);
            (self.function_names.size() as u32).encode(v);
            self.function_names.encode(v);
        }
        if !self.local_names.is_empty() {
            v.push(2);
            (self.local_names.size() as u32).encode(v);
            self.local_names.encode(v);
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
                body: Expr {
                    instructions: vec![
                        Instruction::LocalGet(0),
                        Instruction::I64Const(1),
                        Instruction::U64LessThan,
                        Instruction::If(Some(ValType::F64)),
                        Instruction::I64Const(1),
                        Instruction::Else,
                        Instruction::LocalGet(0),
                        Instruction::LocalGet(0),
                        Instruction::I64Const(1),
                        Instruction::I64Sub,
                        Instruction::Call(0),
                        Instruction::F64Mul,
                        Instruction::End,
                    ],
                },
            }],

            exports: vec![Export {
                name: "fac".into(),
                desc: ExportDesc::Func(0),
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
