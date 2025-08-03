# dlibc - Dynamic Libc Bindings for Rust

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<p >
    <span>
        <b>中文</b>
    </span>
    <span> • </span>
    <a href="README.md">
        English
    </a>
</p>

在编译时间创建使用实际系统头文件动态生成的glibc/musl/...绑定。

当前rust 生态中使用的`libc` 存在一些问题:
- 无法快速跟进系统变化 (人力不足)
- 无法处理在不同版本间冲突的符号(eg: time_t/time64)
- 无法支持xxx_MAX 符号(libc 有意防止添加此类符号以避免不兼容)

此crate 可作为libc 的补充, 能够使用当前系统下实际的头文件,
当然, 相比与`libc`, 也存在一些要求:
- 系统需要支持bindgen 及其依赖`clang-dev`
- 如果需要与内核交互, 还需要按照内核头文件, 如: `linux-libc-dev` 

## Features

- 🚀 使用bindgen 编译时生成所有绑定
- 📁 文件路径结构映射到Rust模块层次结构, 使用体验与C程序一致

## Usage

添加到 `Cargo.toml`:
```toml
[dependencies]
dlibc = "0.1"

```
使用:

```rust
use dlibc::stdio;

fn main() {
    unsafe {
        // Call system functions directly
        stdio::printf(c"Hello, dynamic libc!\n".as_ptr());
    }
}
```


TODO:
- [ ] bsd 支持
- [ ] 支持function macro
- [ ] 支持在依赖包中检测符号符号存在, 即`ifdef`

## note:

当前依赖的bindgen 一些feature PR尚未合入 , 还未发布到crates.io, 如有需要,请直接使用github 仓库
