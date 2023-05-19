# wabam

A crate for creating WebAssembly modules.

## Example

```rust
let mut module = wabam::Module::default();

module.types = vec![
    wabam::func_type!((param) (result)),
    wabam::func_type!((param i32 i32 i32 i32) (result i32)),
];

// Import WASI's `fd_write`, to print to the terminal
let fd_write = wabam::interface::Import {
    module: "wasi_snapshot_preview1".into(),
    name: "fd_write".into(),
    desc: wabam::interface::ImportDesc::Func {
        type_idx: 1, // types[1], see above
    }
};
module.imports.push(fd_write);

// Define memory
let memory = wabam::Limit {
    start: 1,
    end: None,
};
module.memories.push(memory);

let text = "Hello, wasm!";
let text_ptr = 12;

// Load the text into memory
let data = wabam::Data::Active {
    mem_index: 0,
    offset: wabam::instr!(i32.const { text_ptr }).into(),
    data: text.into(),
};
module.datas.push(data);

let body = wabam::instrs!(
    (i32.const 0) // Where the `(ptr, len)` pair is
    (i32.const { text_ptr })
    (i32.store) // Write ptr

    (i32.const 0) // Where the `(ptr, len)` pair is
    (i32.const { text.len() as i32 })
    (i32.store offset=4) // Write len

    (i32.const 1) // File descriptor, stdout is 1.
    (i32.const 0) // Where the `(ptr, len)` pair is
    (i32.const 1) // How many `(ptr, len)` pairs there are
    (i32.const 8) // Where to write the number of written bytes
    (call 0) // imported functions are at the start of the address space
    // this current function would be 1 (this is important later!)
    (drop) // Ignore the error, this is just an example after all!
);

let func = wabam::functions::Function {
    type_idx: 0, // types[0], see above
    locals: vec![], // no local variables
    body: body.into(),
};
module.functions.push(func);

// Export the start function
let func_export = wabam::interface::Export {
    name: "_start".into(),
    desc: wabam::interface::ExportDesc::Func {
        func_idx: 1, // this is where that's important
    }
};

// Export memory
let mem_export = wabam::interface::Export {
    name: "memory".into(),
    desc: wabam::interface::ExportDesc::Memory {
        mem_idx: 0,
    }
};
module.exports.push(func_export);
module.exports.push(mem_export);

let output = module.build();

std::fs::write("./hello.wasm", &output).unwrap();
```

Then run it:

```txt
$ wasmtime ./hello.wasm
Hello, wasm!
```

# Licensing

This project is licensed under either
[Apache License Version 2.0](LICENSE-APACHE) or [MIT License](LICENSE-MIT)
at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
