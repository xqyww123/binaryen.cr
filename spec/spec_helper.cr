require "spec"
require "../src/binaryen"
require "tempfile"

include Binaryen

def new_expression
end

def temp_file
    Tempfile.new "binaryen.cr-spec"
end
def wasm_dis(o : Module::Codes)
    f = temp_file
    File.write f.path, o.data
    system "wasm-dis #{f.path}"
    puts f.path
    f.delete
end

