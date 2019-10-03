require "spec"
require "../src/binaryen"

include Binaryen

def new_expression
end

def temp_file
  File.tempfile "binaryen.cr-spec"
end

def wasm_dis(o : Code)
  f = temp_file
  File.write f.path, o.data
  system "wasm-dis #{f.path}"
  puts f.path
  f.delete
end
