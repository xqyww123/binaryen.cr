require "./spec_helper"

p 123

describe Binaryen do
  # TODO: Write tests

  describe Module do
      it "could be created and compile simple code" do
          mod = Module.new
          ft = mod.add_function_type nil, Types::None, [Types::None] of Type
          eret = mod.exp_return
          mod.start_point = mod.add_function "foo", ft, [] of Type, eret
          mod.interpret
          p mod.compile
      end
  end

  describe Expression do
  end
end
