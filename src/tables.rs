use crate::{
    encode::{DecodeError, WasmDecode},
    Expr, Limit, RefType, WasmEncode,
};

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

impl WasmDecode for TableType {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, DecodeError> {
        let ref_type = RefType::decode(buf)?;
        let limits = Limit::decode(buf)?;
        Ok(Self { ref_type, limits })
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

impl WasmDecode for ElemKind {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, DecodeError>
    where
        Self: Sized {
        let d = u8::decode(buf)?;
        match d {
            0 => Ok(Self::FuncRef),
            _ => Err(DecodeError::InvalidType(d)),
        }
    }
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

impl WasmDecode for Element {
    fn decode(buf: &mut crate::encode::Buf<'_>) -> Result<Self, DecodeError> {
        use ElemInit::*;
        use ElemMode::*;

        let d = u8::decode(buf)?;
        let elem = match d {
            0 => {
                let offset = Expr::decode(buf)?;
                let indices = Vec::<u32>::decode(buf)?;
                Element {
                    kind: ElemKind::FuncRef,
                    init: Indices(indices),
                    mode: Active {
                        table_idx: 0,
                        offset,
                    },
                }
            }
            1 => {
                let kind = ElemKind::decode(buf)?;
                let indices = Vec::<u32>::decode(buf)?;
                Element {
                    kind,
                    init: Indices(indices),
                    mode: Passive,
                }
            }
            2 => {
                let table_idx = u32::decode(buf)?;
                let offset = Expr::decode(buf)?;
                let kind = ElemKind::decode(buf)?;
                let indices = Vec::<u32>::decode(buf)?;
                Element {
                    kind,
                    init: Indices(indices),
                    mode: Active { table_idx, offset },
                }
            }
            3 => {
                let kind = ElemKind::decode(buf)?;
                let exprs = Vec::<Expr>::decode(buf)?;
                Element {
                    kind,
                    init: Expressions(exprs),
                    mode: Passive,
                }
            }
            4 => {
                let offset = Expr::decode(buf)?;
                let exprs = Vec::<Expr>::decode(buf)?;
                Element {
                    kind: ElemKind::FuncRef,
                    init: Expressions(exprs),
                    mode: Active {
                        table_idx: 0,
                        offset,
                    },
                }
            }
            5 => {
                let kind = ElemKind::decode(buf)?;
                let exprs = Vec::<Expr>::decode(buf)?;
                Element {
                    kind,
                    init: Expressions(exprs),
                    mode: Passive,
                }
            }
            6 => {
                let table_idx = u32::decode(buf)?;
                let offset = Expr::decode(buf)?;
                let kind = ElemKind::decode(buf)?;
                let exprs = Vec::<Expr>::decode(buf)?;
                Element {
                    kind,
                    init: Expressions(exprs),
                    mode: Active { table_idx, offset },
                }
            }
            7 => {
                let kind = ElemKind::decode(buf)?;
                let exprs = Vec::<Expr>::decode(buf)?;
                Element {
                    kind,
                    init: Expressions(exprs),
                    mode: Passive,
                }
            }
            _ => return Err(DecodeError::InvalidDiscriminant(d)),
        };

        Ok(elem)
    }
}
