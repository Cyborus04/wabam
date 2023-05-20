use std::mem::MaybeUninit;

pub(crate) trait WasmEncode {
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

            if x == 0 {
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

            if (x == 0 && byte & 0x40 == 0) || (x == -1 && byte & 0x40 != 0) {
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

pub(crate) struct Buf<'a> {
    buf: &'a [u8],
    consumed: usize,
    prev_consumed: usize,
}

impl<'a> Buf<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Self {
            buf,
            consumed: 0,
            prev_consumed: 0,
        }
    }

    pub fn with_consumed(buf: &'a [u8], consumed: usize) -> Self {
        Self {
            buf,
            consumed,
            prev_consumed: consumed,
        }
    }

    pub fn take(&mut self, n: usize) -> Option<&'a [u8]> {
        if n > self.buf.len() {
            return None;
        }
        let (ret, new_self) = self.buf.split_at(n);
        self.buf = new_self;
        self.prev_consumed = self.consumed;
        self.consumed += ret.len();
        Some(ret)
    }

    pub fn take_one(&mut self) -> Option<u8> {
        let (ret, new_self) = self.buf.split_first()?;
        self.buf = new_self;
        self.prev_consumed = self.consumed;
        self.consumed += 1;
        Some(*ret)
    }

    pub fn take_rest(&mut self) -> &[u8] {
        let x = std::mem::take(&mut self.buf);
        self.prev_consumed = self.consumed;
        self.consumed += x.len();
        x
    }

    pub fn consumed(&self) -> usize {
        self.consumed
    }

    pub fn error_location(&self) -> usize {
        self.prev_consumed
    }

    pub fn exhausted(&self) -> bool {
        self.buf.is_empty()
    }
}

/// Errors that can happen when reading a wasm module.
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    /// The magic value of `\0asm\1\0\0\0` was not found at the start of the file.
    BadHeader([u8; 8]),
    /// Section appeared after when it should.
    SectionOutOfOrder { prev: u8, this: u8 },
    /// Unknown section (id > 11) was found.
    InvalidSectionId(u8),
    /// There was a function section, but no code section
    FuncWithoutCode,
    /// There was a code section, but no function section
    CodeWithoutFunc,
    /// The lengths of the code and function sections are not the same
    FuncCodeMismatch { func_len: u32, code_len: u32 },
    /// The file was too short
    TooShort,
    /// A boolean with a value other than 0 or 1 was found
    BadBool,
    /// A number was encoded with too many bytes
    NumTooLong,
    /// Invalid UTF-8
    InvalidUtf8(std::string::FromUtf8Error),
    /// Unknown type id
    InvalidType(u8),
    /// Unknown variant found
    InvalidDiscriminant(u8),
    /// Unknown instruction found
    InvalidInstruction(u8, Option<u32>),
    /// Memory index other than 0 was used
    MemIndexOutOfBounds(u32),
}

impl ErrorKind {
    pub(crate) fn at(self, at: &Buf<'_>) -> crate::Error {
        crate::Error {
            offset: at.error_location(),
            error: self,
        }
    }
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            ErrorKind::BadHeader(found) => {
                write!(f, "expected magic number \"\\0asm\", found {:?}", found)
            }
            ErrorKind::SectionOutOfOrder { prev, this } => {
                write!(f, "section {} found after section {}", this, prev)
            }
            ErrorKind::InvalidSectionId(id) => write!(f, "found section with id {}", id),
            ErrorKind::FuncWithoutCode => {
                write!(f, "function section was found but code section was not")
            }
            ErrorKind::CodeWithoutFunc => {
                write!(f, "code section was found but function section was not")
            }
            ErrorKind::FuncCodeMismatch { func_len, code_len } => write!(
                f,
                "function section length ({}b) is not equal to code section length ({}b)",
                func_len, code_len
            ),
            ErrorKind::TooShort => write!(f, "file ended before was expected"),
            ErrorKind::BadBool => write!(f, "bool was not 0 or 1"),
            ErrorKind::NumTooLong => write!(f, "number took too many bytes"),
            ErrorKind::InvalidUtf8(ref e) => e.fmt(f),
            ErrorKind::InvalidType(t) => write!(f, "type id {:#02X} is not valid", t),
            ErrorKind::InvalidDiscriminant(d) => {
                write!(f, "variant discriminant {:#02X} is not valid", d)
            }
            ErrorKind::InvalidInstruction(x, y) => match y {
                Some(y) => write!(f, "{x:#02X}-{y:08X} is not a valid instruction"),
                None => write!(f, "{x:#02X} is not a valid instruction"),
            },
            ErrorKind::MemIndexOutOfBounds(idx) => {
                write!(f, "memory idx {idx} is greater than zero")
            }
        }
    }
}

impl std::error::Error for ErrorKind {}

impl From<std::string::FromUtf8Error> for ErrorKind {
    fn from(value: std::string::FromUtf8Error) -> Self {
        Self::InvalidUtf8(value)
    }
}

pub(crate) trait WasmDecode {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind>
    where
        Self: Sized;
}

impl<T: WasmDecode, const N: usize> WasmDecode for [T; N] {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let mut out: MaybeUninit<[T; N]> = MaybeUninit::uninit();
        let ptr = out.as_mut_ptr().cast::<T>();

        for i in 0..N {
            let x = T::decode(buf);
            match x {
                Ok(x) => unsafe { ptr.add(i).write(x) },
                Err(e) => {
                    // Drop all previously decoded elements
                    let init = std::ptr::slice_from_raw_parts_mut(ptr, i);
                    unsafe { std::ptr::drop_in_place(init) };

                    return Err(e);
                }
            }
        }

        Ok(unsafe { out.assume_init() })
    }
}

impl WasmDecode for u8 {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        buf.take_one().ok_or(ErrorKind::TooShort)
    }
}

impl WasmDecode for bool {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        match u8::decode(buf)? {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(ErrorKind::BadBool),
        }
    }
}

impl WasmDecode for u32 {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let mut out = 0;
        for i in 0..5 {
            let b = u8::decode(buf)?;
            out |= ((b & 0x7F) as u32) << (i * 7);
            if b & 0x80 == 0 {
                return Ok(out);
            }
        }
        Err(ErrorKind::NumTooLong)
    }
}

impl WasmDecode for i32 {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let mut out = 0;
        for i in 0..5 {
            let b = u8::decode(buf)?;
            out |= ((b & 0x7F) as u32).wrapping_shl(i * 7);
            if b & 0x80 == 0 {
                let x = if b & 0x40 != 0 && ((i + 1) * 7) < 32 {
                    out | (u32::MAX.wrapping_shl((i + 1) * 7))
                } else {
                    out
                };
                return Ok(x as i32);
            }
        }
        Err(ErrorKind::NumTooLong)
    }
}

impl WasmDecode for u64 {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let mut out = 0;
        for i in 0..10 {
            let b = u8::decode(buf)?;
            out |= ((b & 0x7F) as u64) << (i * 7);
            if b & 0x80 == 0 {
                return Ok(out);
            }
        }
        Err(ErrorKind::NumTooLong)
    }
}

impl WasmDecode for i64 {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let mut out = 0;
        for i in 0..10 {
            let b = u8::decode(buf)?;
            out |= ((b & 0x7F) as u64).wrapping_shl(i * 7);

            if b & 0x80 == 0 {
                let x = if b & 0x40 != 0 && ((i + 1) * 7) < 64 {
                    out | (u64::MAX.wrapping_shl((i + 1) * 7))
                } else {
                    out
                };
                return Ok(x as i64);
            }
        }
        Err(ErrorKind::NumTooLong)
    }
}

impl WasmDecode for f32 {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        Ok(f32::from_le_bytes(<[u8; 4]>::decode(buf)?))
    }
}

impl WasmDecode for f64 {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        Ok(f64::from_le_bytes(<[u8; 8]>::decode(buf)?))
    }
}

impl<T: WasmDecode> WasmDecode for Vec<T> {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let len = u32::decode(buf)? as usize;
        let mut v = Vec::with_capacity(len);
        for _ in 0..len {
            v.push(T::decode(buf)?);
        }
        Ok(v)
    }
}

impl WasmDecode for String {
    fn decode(buf: &mut Buf<'_>) -> Result<Self, ErrorKind> {
        let s = String::from_utf8(Vec::<u8>::decode(buf)?)?;
        Ok(s)
    }
}

#[test]
fn integer_round_trip() {
    let mut v = Vec::new();

    1i32.encode(&mut v);
    1000i32.encode(&mut v);
    1_000_000i32.encode(&mut v);
    (-1i32).encode(&mut v);
    (-25i32).encode(&mut v);

    let mut buf = Buf::new(&v);

    assert_eq!(i32::decode(&mut buf), Ok(1i32));
    assert_eq!(i32::decode(&mut buf), Ok(1000i32));
    assert_eq!(i32::decode(&mut buf), Ok(1_000_000i32));
    assert_eq!(i32::decode(&mut buf), Ok(-1i32));
    assert_eq!(i32::decode(&mut buf), Ok(-25i32));
}
