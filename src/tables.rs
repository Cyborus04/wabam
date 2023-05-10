use crate::{RefType, Limit, WasmEncode, Expr};


#[derive(PartialEq, Eq, Debug, Clone)]
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

#[derive(PartialEq, Debug, Clone)]
pub struct Element {
    pub kind: ElemKind,
    pub init: ElemInit,
    pub mode: ElemMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElemKind {
    FuncRef,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ElemInit {
    Indices(Vec<u32>),
    Expressions(Vec<Expr>),
}

#[derive(PartialEq, Debug, Clone)]
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