# Binaryen.cr

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

mod = Binaryen::Module.new
ft = mod.add_function_type "name_of_function_type", Binaryen::Int32, [Binaryen::Int32]
# add_function_type name, result_type, params_type
f  = mod.add_function "name_of_function", ft, [Binaryen::Int64], mod.exp_return(mod.exp_get_local 0, Binaryen::Int32)
# add_function name, function_type, locals type, expression
mod.start_point = f
code = mod.compile # => Binaryen::Module::Codes
File.write "result", code.data # => Bytes
```

Basically warpped from Binaryen C-API by Crystal classes, with some OOP conception such as class `Module` and struct `Function` etc.

With enough WASM knowledge, [src/binaryen.cr](https://github.com/xqyww123/binaryen.cr/blob/master/src/binaryen.cr) is clear enough without any comment.

Get more information from [Binaryen](https://github.com/WebAssembly/binaryen) C-API ([binaryen-c.h](https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h)) and [WASM design](https://github.com/WebAssembly/design).

Find more example in spec.

## Contributors

- [[xqyww123]](https://github.com/xqyww123) Shirotsu Essential - creator, maintainer
