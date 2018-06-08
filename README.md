# binaryen.cr

A Crystal binding of [Binaryen](https://github.com/WebAssembly/binaryen).

**WARNING :** lack enough test currently.

However most of bug could be fix quickly. And depending on the huge workload for testing code, I decide to develop my next project using this library (which is a Crystal -> WASM compiler), and test them together. Completely testing will be finished after several weeks I believe. Pull request for testing or bug fix is really appreciated.

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  binaryen:
    github: xqyww123/binaryen.cr
```

## Usage

```crystal
require "binaryen"
```

Basically warpped from Binaryen C-API by Crystal classes, with some OOP conception such as class `Module` and struct `Function` etc.

With enough WASM knowledge, `src/binaryen.cr` is clear enough without any comment.

Get more information from [Binaryen](https://github.com/WebAssembly/binaryen) C-API : [binaryen-c.h](https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h) and [WASM design](https://github.com/WebAssembly/design).

## Contributors

- [[xqyww123]](https://github.com/xqyww123) Shirotsu Essential - creator, maintainer
