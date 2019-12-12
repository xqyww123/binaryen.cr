require "./spec_helper"

describe Binaryen do
  describe Module do
    it "could be created and compile simple code with import, function declaration" do
      mod = Module.new
      c1 = mod.exp_const 1_i32
      c2 = mod.exp_const 2_i32
      fti = mod.add_function_type nil, Types::Int32, [] of Type
      ft = mod.add_function_type nil, Types::None, [] of Type
      ftps = mod.add_function_type nil, Types::None, [Types::Int64]
      mod.add_function_import("fputs123", "global", "puts", ftps)
      ad = mod.exp Ops::AddInt32, c1, c2
      ret2 = mod.exp_return ad
      f2 = mod.add_function "foo2", fti, [] of Type, ret2
      mod.add_export f2, "xx1"
      c3 = mod.exp_const(3_i64)
      eret = mod.exp_call "fputs123", [c3], Types::None
      mod.start_point = mod.add_function "foo", ft, [] of Type, eret
      mod.interpret
      o = mod.compile
      wasm_dis o
      # o.size.should eq 27
    end
    it "could add / get / remove function_type" do
      mod = Module.new
      ft = mod.add_function_type "ooxx", Types::Int32, [Types::Int64]
      # ft = mod.add_function_type "ooxx2", Types::Int32, [Types::Int64]
      ft2 = mod.get_function_type "ooxx"
      ft2.name.should eq "ooxx"
      ft2 = mod.get_function_type Types::Int32, [Types::Int64]
      ft2.name.should eq "ooxx"
      mod.remove ft
      mod.remove ft
      wasm_dis mod.compile
    end
    it "could add / get / remove function, with its property and get / tee local" do
      mod = Module.new
      ft1 = mod.add_function_type nil, Types::Int32, [Types::Int64, Types::Int32]
      ft2 = mod.add_function_type nil, Types::Int32, [] of Type
      e0 = mod.exp_get_local 0, Types::Int32
      e06 = mod.exp Ops::ExtendSInt32, e0
      e1 = mod.exp_get_local 1, Types::Int64
      emu = mod.exp Ops::MulInt64, e06, e1
      ete = mod.exp_tee_local 2, emu
      ead = mod.exp Ops::AddInt64, ete, e1
      f1 = mod.add_function "foo", ft1, [Types::Int64] of Type, mod.exp_return(ete)
      f2 = mod.add_function "foo2", ft1, [Types::Int64] of Type, mod.exp_return(e0)
      mod.remove f2
      wasm_dis mod.compile
    end
    it "could set / load / store (default) linear memory" do
      mod = Module.new
      seg = MemorySettingSegment.new Bytes.new(64, 233_u8), mod.exp_const(2048)
      mod.memory_setting.segments << seg
      ft = mod.add_function_type nil, Types::None, [] of Type
      e0 = mod.exp_set_local 0, mod.exp_const(2048)
      e1 = mod.exp_load 4, true, 32, 0, Types::Int32, mod.exp_get_local(0, Types::Int32)
      e2 = mod.exp Ops::AddInt32, e1, mod.exp_const(233)
      e3 = mod.exp_store 4, 32, 0, mod.exp_get_local(0, Types::Int32),
        e2, Types::Int32
      f = mod.add_function "main", ft, [Types::Int32], mod.exp_block(nil,
        [e0, e3])
      mod.start_point = f
      wasm_dis mod.compile
    end

    it "block can not be argument" do
      mod = Module.new
      ft = mod.add_function_type nil, Types::Int32, [Types::Int32] of Type
      f = mod.add_function "f1", ft, ([] of Type), mod.exp_get_local 0, Types::Int32
      ft = mod.add_function_type nil, Types::Int32, [] of Type
      eb = mod.exp_block(nil, [
        mod.exp_set_local(0, mod.exp_const(2048)),
        mod.exp(Ops::AddInt32, mod.exp_get_local(0, Types::Int32), mod.exp_const 2),
      ], Types::Int32)
      f = mod.add_function "main", ft, ([Types::Int32] of Type),
        mod.exp_call "f1", [eb], Types::Int32
      mod.start_point = f
      # mod.print
      wasm_dis mod.compile
    end
  end

  describe Expression do
  end
end
