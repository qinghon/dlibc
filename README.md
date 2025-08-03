# dlibc - Dynamic Libc Bindings for Rust

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<p >
    <span>
        <a href="README.zh_CN.md">
            <b>中文</b>
        </a>
    </span>
    <span> • </span>
    <span>
        <b>English</b>
    </span>
</p>

Dynamically generated glibc/musl/... bindings created at compile time using your system's actual header files.

The current `libc` crate in the Rust ecosystem has some issues:
- Unable to quickly follow system changes (insufficient manpower)
- Cannot handle conflicting symbols between different versions (e.g., time_t/time64)
- Cannot support xxx_MAX symbols (libc intentionally prevents adding such symbols to avoid incompatibility)

This crate can serve as a supplement to libc, capable of using the actual header files from your current system. However, compared to `libc`, there are some requirements:
- The system needs to support bindgen and its dependencies like `clang-dev`
- If you need to interact with the kernel, you also need to install kernel header files, such as: `linux-libc-dev`

## Features

- 🚀 All bindings generated at compile time using bindgen
- 📁 File path structure mapped to Rust module hierarchy, providing consistent experience with C programs

## Usage

Add to your `Cargo.toml`:
```toml
[dependencies]
dlibc = "0.1"

```

Usage:

```rust
use dlibc::stdio;

fn main() {
    unsafe {
        // Call system functions directly
        stdio::printf(c"Hello, dynamic libc!\n".as_ptr());
    }
}
```

## TODO

- [ ] BSD support
- [ ] Support for function macros
- [ ] Support for detecting symbol existence in dependent packages, i.e., `ifdef`

## Note

Some feature PRs that the current bindgen dependency relies on have not been merged yet and haven't been published to crates.io. If needed, please use the GitHub repository directly. 
