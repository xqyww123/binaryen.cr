require "../src/binaryen.cr"

include Binaryen
p 123
p Module.new.compile.data.bytesize == 0

