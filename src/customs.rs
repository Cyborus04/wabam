//! Customs sections are named series of arbitrary bytes, often used for debug info.

use std::collections::BTreeMap;

use crate::{
    encode::{Buf, ErrorKind, WasmDecode},
    WasmEncode,
};

/// A section containing arbitrary bytes.
#[derive(PartialEq, Eq, Debug, Clone)]
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

impl WasmDecode for CustomSection {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let name = String::decode(buf)?;
        let data = buf.take_rest().to_vec();
        Ok(Self { name, data })
    }
}

/// A custom section that defines debug names for the module, functions, and
/// local variables.
///
/// # Example
///
/// ```
/// # use wabam::{Module, customs::NameSection};
/// # let mut module = Module::default();
/// // ...continuing from `add.wasm` example from `Module`'s docs
///
/// let names = NameSection {
///     module_name: Some("add module".into()),
///     function_names: [(0, "add".into())].into(),
///     local_names: [].into(),
/// };
///
/// module.custom_sections.push(names.to_custom());
/// ```
pub struct NameSection {
    pub module_name: Option<String>,
    pub function_names: BTreeMap<u32, String>,
    pub local_names: BTreeMap<u32, BTreeMap<u32, String>>,
}

impl NameSection {
    pub fn to_custom(&self) -> CustomSection {
        let mut data = Vec::with_capacity(self.size());
        self.encode(&mut data);
        CustomSection {
            name: "name".into(),
            data,
        }
    }

    pub fn from_custom(custom: &[u8]) -> Result<NameSection, crate::Error> {
        let mut buf = Buf::new(custom);
        Self::decode(&mut buf).map_err(|e| e.at(&buf))
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

impl WasmDecode for NameSection {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let mut module_name = None;
        let mut function_names = BTreeMap::new();
        let mut local_names = BTreeMap::new();
        while !buf.exhausted() {
            let discriminant = u8::decode(buf)?;
            match discriminant {
                0 => {
                    let _ = u32::decode(buf)?;
                    module_name = Some(String::decode(buf)?)
                },
                1 => {
                    let _ = u32::decode(buf)?;
                    function_names = BTreeMap::<u32, String>::decode(buf)?
                },
                2 => {
                    let _ = u32::decode(buf)?;
                    local_names = BTreeMap::<u32, BTreeMap<u32, String>>::decode(buf)?
                },
                _ => return Err(ErrorKind::InvalidDiscriminant(discriminant)),
            }
        }
        Ok(Self {
            module_name,
            function_names,
            local_names,
        })
    }
}