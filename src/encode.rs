
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