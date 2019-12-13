require "atomic"
require "./binaryen/*"

module Binaryen
  alias Type = LibBinaryen::Type

  module Types
    None        = LibBinaryen.type_none
    Int32       = LibBinaryen.type_int32
    Int64       = LibBinaryen.type_int64
    Float32     = LibBinaryen.type_float32
    Float64     = LibBinaryen.type_float64
    Auto        = LibBinaryen.type_auto
    Unreachable = LibBinaryen.type_unreachable
    Undefined   = Unreachable

    SymbolMap = {i32: Int32, i64: Int64, f32: Float32, f64: Float64}
    Info      = {% begin %}
      {
      {% for sym, type in SymbolMap %}
        {{type}} => {name: {{type.stringify}},
                     size: sizeof(::{{type}}),
                     crystal_type: ::{{type}},
                     wasm_type: {{type}}
        },
      {% end %}
    }
    {% end %}

    def self.size_of(t)
      (Info[t]? || raise "invalid type #{t}").size
    end

    ClassInWASM    = {::Int32 => Int32, ::Int64 => Int64, ::Float32 => Float32, ::Float64 => Float64, Nil => None, Void => None, ::UInt32 => Int32, ::UInt64 => Int64}
    ClassInCrystal = {Int32 => ::Int32, Int64 => ::Int64, Float32 => ::Float32, Float64 => ::Float64, None => Nil}
  end

  module ExpressionIds
    Invalid = LibBinaryen.invalid_id
    Block   = LibBinaryen.block_id
    If      = LibBinaryen.if_id
    Loop    = LibBinaryen.loop_id
    Break   = LibBinaryen.break_id
    Switch  = LibBinaryen.switch_id
    Call    = LibBinaryen.call_id
    # CallImport    = LibBinaryen.call_import_id
    CallIndirect  = LibBinaryen.call_indirect_id
    GetLocal      = LibBinaryen.local_get_id
    SetLocal      = LibBinaryen.local_set_id
    GetGlobal     = LibBinaryen.global_get_id
    SetGlobal     = LibBinaryen.global_set_id
    Load          = LibBinaryen.load_id
    Store         = LibBinaryen.store_id
    Const         = LibBinaryen.const_id
    Unary         = LibBinaryen.unary_id
    Binary        = LibBinaryen.binary_id
    Select        = LibBinaryen.select_id
    Drop          = LibBinaryen.drop_id
    Return        = LibBinaryen.return_id
    Host          = LibBinaryen.host_id
    Nop           = LibBinaryen.nop_id
    Unreachable   = LibBinaryen.unreachable_id
    AtomicCmpxchg = LibBinaryen.atomic_cmpxchg_id
    AtomicRMW     = LibBinaryen.atomic_rmw_id
    AtomicWait    = LibBinaryen.atomic_wait_id
    AtomicWake    = LibBinaryen.atomic_wake_id
  end

  alias ExternalKind = LibBinaryen::ExternalKind

  module ExternalKinds
    Function = LibBinaryen.external_function
    Table    = LibBinaryen.external_table
    Memory   = LibBinaryen.external_memory
    Global   = LibBinaryen.external_global
  end

  alias Operation = LibBinaryen::Op

  module Ops
    ClzInt32               = LibBinaryen.clz_int32
    CtzInt32               = LibBinaryen.ctz_int32
    PopcntInt32            = LibBinaryen.popcnt_int32
    NegFloat32             = LibBinaryen.neg_float32
    AbsFloat32             = LibBinaryen.abs_float32
    CeilFloat32            = LibBinaryen.ceil_float32
    FloorFloat32           = LibBinaryen.floor_float32
    TruncFloat32           = LibBinaryen.trunc_float32
    NearestFloat32         = LibBinaryen.nearest_float32
    SqrtFloat32            = LibBinaryen.sqrt_float32
    EqZInt32               = LibBinaryen.eq_z_int32
    ClzInt64               = LibBinaryen.clz_int64
    CtzInt64               = LibBinaryen.ctz_int64
    PopcntInt64            = LibBinaryen.popcnt_int64
    NegFloat64             = LibBinaryen.neg_float64
    AbsFloat64             = LibBinaryen.abs_float64
    CeilFloat64            = LibBinaryen.ceil_float64
    FloorFloat64           = LibBinaryen.floor_float64
    TruncFloat64           = LibBinaryen.trunc_float64
    NearestFloat64         = LibBinaryen.nearest_float64
    SqrtFloat64            = LibBinaryen.sqrt_float64
    EqZInt64               = LibBinaryen.eq_z_int64
    ExtendSInt32           = LibBinaryen.extend_s_int32
    ExtendUInt32           = LibBinaryen.extend_u_int32
    WrapInt64              = LibBinaryen.wrap_int64
    TruncSFloat32ToInt32   = LibBinaryen.trunc_s_float32_to_int32
    TruncSFloat32ToInt64   = LibBinaryen.trunc_s_float32_to_int64
    TruncUFloat32ToInt32   = LibBinaryen.trunc_u_float32_to_int32
    TruncUFloat32ToInt64   = LibBinaryen.trunc_u_float32_to_int64
    TruncSFloat64ToInt32   = LibBinaryen.trunc_s_float64_to_int32
    TruncSFloat64ToInt64   = LibBinaryen.trunc_s_float64_to_int64
    TruncUFloat64ToInt32   = LibBinaryen.trunc_u_float64_to_int32
    TruncUFloat64ToInt64   = LibBinaryen.trunc_u_float64_to_int64
    ReinterpretFloat32     = LibBinaryen.reinterpret_float32
    ReinterpretFloat64     = LibBinaryen.reinterpret_float64
    ConvertSInt32ToFloat32 = LibBinaryen.convert_s_int32_to_float32
    ConvertSInt32ToFloat64 = LibBinaryen.convert_s_int32_to_float64
    ConvertUInt32ToFloat32 = LibBinaryen.convert_u_int32_to_float32
    ConvertUInt32ToFloat64 = LibBinaryen.convert_u_int32_to_float64
    ConvertSInt64ToFloat32 = LibBinaryen.convert_s_int64_to_float32
    ConvertSInt64ToFloat64 = LibBinaryen.convert_s_int64_to_float64
    ConvertUInt64ToFloat32 = LibBinaryen.convert_u_int64_to_float32
    ConvertUInt64ToFloat64 = LibBinaryen.convert_u_int64_to_float64
    PromoteFloat32         = LibBinaryen.promote_float32
    DemoteFloat64          = LibBinaryen.demote_float64
    ReinterpretInt32       = LibBinaryen.reinterpret_int32
    ReinterpretInt64       = LibBinaryen.reinterpret_int64
    ExtendS8Int32          = LibBinaryen.extend_s8_int32
    ExtendS16Int32         = LibBinaryen.extend_s16_int32
    ExtendS8Int64          = LibBinaryen.extend_s8_int64
    ExtendS16Int64         = LibBinaryen.extend_s16_int64
    ExtendS32Int64         = LibBinaryen.extend_s32_int64
    AddInt32               = LibBinaryen.add_int32
    SubInt32               = LibBinaryen.sub_int32
    MulInt32               = LibBinaryen.mul_int32
    DivSInt32              = LibBinaryen.div_s_int32
    DivUInt32              = LibBinaryen.div_u_int32
    RemSInt32              = LibBinaryen.rem_s_int32
    RemUInt32              = LibBinaryen.rem_u_int32
    AndInt32               = LibBinaryen.and_int32
    OrInt32                = LibBinaryen.or_int32
    XorInt32               = LibBinaryen.xor_int32
    ShlInt32               = LibBinaryen.shl_int32
    ShrUInt32              = LibBinaryen.shr_u_int32
    ShrSInt32              = LibBinaryen.shr_s_int32
    RotLInt32              = LibBinaryen.rot_l_int32
    RotRInt32              = LibBinaryen.rot_r_int32
    EqInt32                = LibBinaryen.eq_int32
    NeInt32                = LibBinaryen.ne_int32
    LtSInt32               = LibBinaryen.lt_s_int32
    LtUInt32               = LibBinaryen.lt_u_int32
    LeSInt32               = LibBinaryen.le_s_int32
    LeUInt32               = LibBinaryen.le_u_int32
    GtSInt32               = LibBinaryen.gt_s_int32
    GtUInt32               = LibBinaryen.gt_u_int32
    GeSInt32               = LibBinaryen.ge_s_int32
    GeUInt32               = LibBinaryen.ge_u_int32
    AddInt64               = LibBinaryen.add_int64
    SubInt64               = LibBinaryen.sub_int64
    MulInt64               = LibBinaryen.mul_int64
    DivSInt64              = LibBinaryen.div_s_int64
    DivUInt64              = LibBinaryen.div_u_int64
    RemSInt64              = LibBinaryen.rem_s_int64
    RemUInt64              = LibBinaryen.rem_u_int64
    AndInt64               = LibBinaryen.and_int64
    OrInt64                = LibBinaryen.or_int64
    XorInt64               = LibBinaryen.xor_int64
    ShlInt64               = LibBinaryen.shl_int64
    ShrUInt64              = LibBinaryen.shr_u_int64
    ShrSInt64              = LibBinaryen.shr_s_int64
    RotLInt64              = LibBinaryen.rot_l_int64
    RotRInt64              = LibBinaryen.rot_r_int64
    EqInt64                = LibBinaryen.eq_int64
    NeInt64                = LibBinaryen.ne_int64
    LtSInt64               = LibBinaryen.lt_s_int64
    LtUInt64               = LibBinaryen.lt_u_int64
    LeSInt64               = LibBinaryen.le_s_int64
    LeUInt64               = LibBinaryen.le_u_int64
    GtSInt64               = LibBinaryen.gt_s_int64
    GtUInt64               = LibBinaryen.gt_u_int64
    GeSInt64               = LibBinaryen.ge_s_int64
    GeUInt64               = LibBinaryen.ge_u_int64
    AddFloat32             = LibBinaryen.add_float32
    SubFloat32             = LibBinaryen.sub_float32
    MulFloat32             = LibBinaryen.mul_float32
    DivFloat32             = LibBinaryen.div_float32
    CopySignFloat32        = LibBinaryen.copy_sign_float32
    MinFloat32             = LibBinaryen.min_float32
    MaxFloat32             = LibBinaryen.max_float32
    EqFloat32              = LibBinaryen.eq_float32
    NeFloat32              = LibBinaryen.ne_float32
    LtFloat32              = LibBinaryen.lt_float32
    LeFloat32              = LibBinaryen.le_float32
    GtFloat32              = LibBinaryen.gt_float32
    GeFloat32              = LibBinaryen.ge_float32
    AddFloat64             = LibBinaryen.add_float64
    SubFloat64             = LibBinaryen.sub_float64
    MulFloat64             = LibBinaryen.mul_float64
    DivFloat64             = LibBinaryen.div_float64
    CopySignFloat64        = LibBinaryen.copy_sign_float64
    MinFloat64             = LibBinaryen.min_float64
    MaxFloat64             = LibBinaryen.max_float64
    EqFloat64              = LibBinaryen.eq_float64
    NeFloat64              = LibBinaryen.ne_float64
    LtFloat64              = LibBinaryen.lt_float64
    LeFloat64              = LibBinaryen.le_float64
    GtFloat64              = LibBinaryen.gt_float64
    GeFloat64              = LibBinaryen.ge_float64
    PageSize               = LibBinaryen.page_size
    CurrentMemory          = LibBinaryen.current_memory
    GrowMemory             = LibBinaryen.grow_memory
    HasFeature             = LibBinaryen.has_feature

    module Atomic
      Add  = LibBinaryen.atomic_rmw_add
      Sub  = LibBinaryen.atomic_rmw_sub
      And  = LibBinaryen.atomic_rmw_and
      Or   = LibBinaryen.atomic_rmw_or
      Xor  = LibBinaryen.atomic_rmw_xor
      Xchg = LibBinaryen.atomic_rmw_xchg
      Ops  = [Add, Sub, And, Or, Xor, Xchg]
    end

    def self.is_atomic?(id : Operation) : Bool
      Atomic::Ops.include? id
    end
  end

  struct Expression
    NULL = Expression.new Pointer(Void).null
    alias Id = LibBinaryen::ExpressionId
    alias Ids = ExpressionIds

    def initialize(@ref : LibBinaryen::ExpressionRef)
    end

    def to_unsafe
      @ref
    end

    def_equals_and_hash @ref

    def id : Id
      return Ids::Invalid if @ref.null?
      LibBinaryen.expression_get_id(@ref)
    end

    def print
      # p @ref if @ref.null?
      LibBinaryen.expression_print(@ref)
    end

    def is_block? : Bool
      id == Ids::Block
    end

    def is_loop? : Bool
      id == Ids::Loop
    end

    def is_if? : Bool
      id == Ids::If
    end

    def is_const?
      id == Ids::Const
    end

    def name : String | Nil
      String.new case id
      when Ids::Block
        LibBinaryen.block_get_name(@ref).as(Pointer(UInt8))
      when Ids::Loop
        LibBinaryen.loop_get_name(@ref).as(Pointer(UInt8))
      when Ids::Break
        LibBinaryen.break_get_name(@ref).as(Pointer(UInt8))
      else
        return nil
      end
    end

    def type : Type
      LibBinaryen.expression_get_type(@ref)
    end

    struct SwitchNames
      getter exp : Expression
      include Indexable(String)

      def initialize(@exp : Expression)
      end

      def size : Int
        LibBinaryen.switch_get_num_names(exp)
      end

      def unsafe_fetch(i : Int) : String
        String.new LibBinaryen.switch_get_name(exp, i.to_u32)
      end

      def default : String
        String.new LibBinaryen.switch_get_default_name(exp)
      end
    end

    def switch_names
      raise "expression must be Switch, but #{id}" unless id == Ids::Switch
    end

    struct Children
      getter exp : Expression

      def initialize(@exp : Expression)
      end

      include Indexable(Expression)

      def size : Int
        case exp.id
        when Ids::Block
          LibBinaryen.block_get_num_children(exp)
          # when Ids::CallImport
          #  LibBinaryen.call_import_get_num_operands(exp)
        when Ids::Call
          LibBinaryen.call_get_num_operands(exp)
        when Ids::CallIndirect
          LibBinaryen.call_indirect_get_num_operands(exp)
        when Ids::Host
          LibBinaryen.host_get_num_operands(exp)
        else
          raise "bad expression type"
        end
      end

      def unsafe_fetch(i : Int) : Expression
        Expression.new case exp.id
        when Ids::Block
          LibBinaryen.block_get_child(exp, i.to_u32)
        when Ids::Call
          LibBinaryen.call_get_operand(exp, i.to_u32)
          # when Ids::CallImport
          #  LibBinaryen.call_import_get_operand(exp, i.to_u32)
        when Ids::CallIndirect
          LibBinaryen.call_indirect_get_operand(exp, i.to_u32)
        when Ids::Host
          LibBinaryen.host_get_operand(exp, i.to_u32)
        else
          raise "bad expression type"
        end
      end
    end

    def children
      raise "expression must be Block, but #{id}" unless is_block?
      Children.new self
    end

    def operands
      raise "expression must be Call, CallIndirect or Host, but #{id}" unless id == Ids::Call || id == Ids::CallIndirect ||
                                                                              id == Ids::Host
      Children.new self
    end

    def condition : Expression
      Expression.new case id
      when Ids::If
        LibBinaryen.if_get_condition(@ref)
      when Ids::Break
        LibBinaryen.break_get_condition(@ref)
      when Ids::Switch
        LibBinaryen.switch_get_condition(@ref)
      when Ids::Select
        LibBinaryen.select_get_condition(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def value : Expression | Int | String | Float
      Expression.new case id
      when Ids::Break
        LibBinaryen.break_get_value(@ref)
      when Ids::Switch
        LibBinaryen.switch_get_value(@ref)
      when Ids::SetLocal
        LibBinaryen.set_local_get_value(@ref)
      when Ids::SetGlobal
        LibBinaryen.global_set_get_value(@ref)
      when Ids::Store
        LibBinaryen.store_get_value(@ref)
      when Ids::Return
        LibBinaryen.return_get_value(@ref)
      when Ids::AtomicRMW
        LibBinaryen.atomic_rmw_get_value(@ref)
      when Ids::Const
        return case type
        when Types::Int32
          LibBinaryen.const_get_value_i32(@ref)
        when Types::Int64
          LibBinaryen.const_get_value_i64(@ref)
        when Types::Float32
          LibBinaryen.const_get_value_f32(@ref)
        when Types::Float64
          LibBinaryen.const_get_value_f64(@ref)
        else
          raise "bad Const value type"
        end
      else
        raise "bad Expression Id"
      end
    end

    struct AsConst
      getter exp : Expression

      def initialize(@exp)
      end

      {% for sym, type in Types::SymbolMap %}
        def to_{{sym}} : ::{{type}}
          raise "Should be {{type}} but #{Types::Info[@exp.type].name}" unless @exp.type == type
          LibBinaryen.const_get_value{{sym.id.camelcase}} @ref
        end
      {% end %}
    end

    def as_const
      raise "Expect Const but #{Types::Info[id].name}" unless is_const?
      AsConst.new self
    end

    def as_const?
      AsConst.new self if is_const?
    end

    def value_unary : Expression
      LibBinaryen.unary_get_value(@ref)
    end

    def value_left : Expression
      LibBinaryen.binary_get_left(@ref)
    end

    def value_right : Expression
      LibBinaryen.binary_get_right(@ref)
    end

    def if_true : Expression
      Expression.new case id
      when Ids::If
        LibBinaryen.if_get_if_true(@ref)
      when Ids::Select
        LibBinaryen.select_get_if_true(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def if_false : Expression
      Expression.new case id
      when Ids::If
        LibBinaryen.select_get_if_true(@ref)
      when Ids::Select
        LibBinaryen.select_get_if_false(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def body : Expression
      raise "expression must be Loop, but #{id}" unless is_loop?
      Expression.new LibBinaryen.loop_get_body(@ref)
    end

    def target : Expression
      raise "expression must be Call, but #{id}" unless id == Ids::Call
      Expression.new LibBinaryen.call_get_target(@ref)
    end

    # local_ind or global_ind
    def var_ind : Int | String
      case id
      when Ids::GetLocal
        LibBinaryen.get_local_get_index(@ref)
      when Ids::SetLocal
        LibBinaryen.set_local_get_index(@ref)
      when Ids::GetGlobal
        String.new LibBinaryen.global_get_name(@ref)
      when Ids::SetGlobal
        String.new LibBinaryen.global_set_get_name(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def op_unary : Operation
      LibBinaryen.unary_get_op(@ref)
    end

    def op_binary : Operation
      LibBinaryen.binary_get_op(@ref)
    end

    def op : Operation
      case id
      when Ids::Host
        LibBinaryen.host_get_op(@ref)
      when Ids::AtomicRMW
        LibBinaryen.atomic_rmw_get_op(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def is_atomic? : Bool
      case id
      when Ids::Load
        LibBinaryen.load_is_atomic(@ref).to_i32 != 0_u32
      when Ids::Store
        LibBinaryen.store_is_atomic(@ref).to_i32 != 0_u32
      else
        raise "bad Expression Id"
      end
    end

    def bytes : Int
      case id
      when Ids::Load
        LibBinaryen.load_get_bytes(@ref)
      when Ids::Store
        LibBinaryen.store_get_bytes(@ref)
      when Ids::AtomicRMW
        LibBinaryen.atomic_rmw_get_bytes(@ref)
      when Ids::AtomicCmpxchg
        LibBinaryen.atomic_cmpxchg_get_bytes(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def offset : Int
      case id
      when Ids::Load
        LibBinaryen.load_get_offset(@ref)
      when Ids::Store
        LibBinaryen.store_get_offset(@ref)
      when Ids::AtomicRMW
        LibBinaryen.atomic_rmw_get_offset(@ref)
      when Ids::AtomicCmpxchg
        LibBinaryen.atomic_cmpxchg_get_offset(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def align : Int
      case id
      when Ids::Load
        LibBinaryen.load_get_align(@ref)
      when Ids::Store
        LibBinaryen.store_get_align(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def ptr : Expression
      Expression.new case id
      when Ids::Load
        LibBinaryen.load_get_ptr(@ref)
      when Ids::Store
        LibBinaryen.store_get_ptr(@ref)
      when Ids::AtomicRMW
        LibBinaryen.atomic_rmw_get_ptr(@ref)
      when Ids::AtomicCmpxchg
        LibBinaryen.atomic_cmpxchg_get_ptr(@ref)
      when Ids::AtomicWait
        LibBinaryen.atomic_wait_get_ptr(@ref)
      when Ids::AtomicWake
        LibBinaryen.atomic_wake_get_ptr(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def expected : Expression
      Expression.new case id
      when Ids::AtomicCmpxchg
        LibBinaryen.atomic_cmpxchg_get_expected(@ref)
      when Ids::AtomicWait
        LibBinaryen.atomic_wait_get_expected(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def replacement : Expression
      Expression.new case id
      when Ids::AtomicCmpxchg
        LibBinaryen.atomic_cmpxchg_get_replacement(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def expected_type : Type
      case id
      when Ids::AtomicWait
        LibBinaryen.atomic_wait_get_expected_type(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def wake_count : Expression
      Expression.new case id
      when Ids::AtomicWait
        LibBinaryen.atomic_wake_get_wake_count(@ref)
      else
        raise "bad Expression Id"
      end
    end

    def timeout : Expression
      Expression.new case id
      when Ids::AtomicWait
        LibBinaryen.atomic_wait_get_timeout(@ref)
      else
        raise "bad Expression Id"
      end
    end
  end

  #    struct ExpressionFactory < Expression
  #        def initialize(modl : Module, name : String | Nil)
  #            super Pointer(Void).null
  #            @exps = [] of Expression
  #            @modl, @name = modl, name
  #        end
  #        getter exps : Array(Expression), modl : Module, name : String
  #        def ref : LibBinaryen::ExportRef
  #            @ref = @modl.exp_block nil, @exps
  #        end
  #        def is_block? : Bool
  #            true
  #        end
  #        def type
  #
  #        end
  #    end
  struct FunctionType
    def initialize(@ftref : LibBinaryen::FunctionTypeRef,
                   @modl : Module)
    end

    def to_unsafe
      @ftref
    end

    getter ftref : LibBinaryen::FunctionTypeRef, modl : Module

    def remove
      @modl.remove_function_type name
    end

    def name
      String.new LibBinaryen.function_type_get_name @ftref
    end

    struct Params
      include Indexable(String)

      def initialize(@function_type : FunctionType)
      end

      getter function_type

      def size
        LibBinaryen.function_type_get_num_params @function_type
      end

      def unsafe_fetch(i : Int)
        String.new LibBinaryen.function_type_get_param @functionType, i.to_u32
      end
    end

    def params
      Params.new self
    end

    def result : Type
      LibBinaryen.function_type_get_result @ftref
    end
  end

  struct Function
    def initialize(@ref : LibBinaryen::FunctionRef)
    end

    getter ref : LibBinaryen::FunctionRef

    NULL = new LibBinaryen::FunctionRef.null

    def self.null
      NULL
    end

    def to_unsafe
      @ref
    end

    struct Params
      def initialize(@f : Function)
      end

      getter f : Function
      include Indexable(Type)

      def size : Int
        LibBinaryen.function_get_num_params(f)
      end

      def unsafe_fetch(i : Int) : Type
        LibBinaryen.function_get_param(f, i)
      end
    end

    struct Vars
      def initialize(@f : Function)
      end

      getter f : Function
      include Indexable(Type)

      def size : Int
        LibBinaryen.function_get_num_vars(f)
      end

      def unsafe_fetch(i : Int) : Type
        LibBinaryen.function_get_var(f, i)
      end
    end

    def params : Params
      Params.new self
    end

    def vars : Vars
      Vars.new self
    end

    def result_type : Type
      LibBinaryen.function_get_result(@ref)
    end

    def body : Expression
      Expression.new LibBinaryen.function_get_body(@ref)
    end

    def name : String
      String.new LibBinaryen.function_get_name(@ref)
    end

    def type : String
      String.new LibBinaryen.function_get_type(@ref)
    end
  end

  struct Export
    def initialize(@ref : LibBinaryen::ExportRef)
    end

    getter ref : LibBinaryen::ExportRef

    def to_unsafe : LibBinaryen::ExportRef
      @ref
    end

    def internal_name : String
      String.new LibBinaryen.export_get_name @ref
    end

    def export_name : String
      String.new LibBinaryen.export_get_value @ref
    end

    def kind : ExternalKind
      LibBinaryen.export_get_kind @ref
    end
  end

  struct FunctionTable
    def initialize(@name : String = "_default")
    end

    getter name : String
    property functions : Array(Function) = [] of Function
  end

  module Tools
    def self.to_literial(val : Int | Float)
      case val
      when Int64, UInt64
        LibBinaryen.literal_int64(val)
      when Int32, UInt32
        LibBinaryen.literal_int32(val)
      when Float64
        LibBinaryen.literal_float32(val)
      when Float32
        LibBinaryen.literal_float64(val)
      else
        raise "Bad type : #{typeof(val)}"
      end
    end

    def self.to_literial(val : Int | Float, type : Type)
      case type
      when Types::Int32
        LibBinaryen.literal_int32 val.to_i32
      when Types::Int64
        LibBinaryen.literal_int64 val.to_i64
      when Types::Float32
        LibBinaryen.literal_float32 val.to_f32
      when Types::Float64
        LibBinaryen.literal_float64 val.to_f64
      else
        raise ArgumentError.new "bad type, must indicate a entity type"
      end
    end
  end

  class Code
    def initialize(@data)
    end

    getter data : Bytes

    def finalize
      LibC.free @data
    end

    forward_missing_to @data
  end

  struct BlockBuilder
    @deque = Deque(Expression).new
    getter modl : Module

    def initialize(@modl)
    end

    forward_missing_to @modl

    def blocks
      @deque.to_a
    end

    def block(name = nil, type = Types::Undefined)
      @modl.exp_block name, blocks
    end

    def <<(o)
      @deque.push o
    end

    def >>(o)
      @back.unshift o
    end
  end

  class MemorySettingSegment
    property data : Bytes,
      passive : Bool = false,
      offset : Expression

    def initialize(@data, @offset, @passive = false)
    end
  end

  class Module
    def initialize(@modl : LibBinaryen::ModuleRef = Pointer(Void).null)
      @modl ||= LibBinaryen.module_create
      @id = Atomic(UInt32).new(1_u32)
    end

    getter modl : LibBinaryen::ModuleRef
    # the default table
    getter table : FunctionTable = FunctionTable.new
    @function_types : Hash(String, FunctionType) = {} of String => FunctionType

    def finalize
      LibBinaryen.module_dispose(@modl)
    end

    def to_unsafe
      @modl
    end

    def add_function_type(name : String | Nil, result : Type,
                          params : Array(Type)) : FunctionType
      name ||= "_xrystal_ftype_#{@id.add 1_u32}"
      @function_types[name] ||= begin
        f = LibBinaryen.add_function_type(@modl, name,
          result, params, params.size.to_u32)
        FunctionType.new f, self
      end
    end

    def remove_function_type(name : String) : Void
      return unless @function_types.delete name
      LibBinaryen.remove_function_type(@modl, name)
    end

    def remove(ft : FunctionType) : Void
      remove_function_type ft.name
    end

    def get_function_type(name : String)
      @function_types[name]
    end

    def get_function_type(result : Type, params : Array(Type))
      FunctionType.new LibBinaryen.get_function_type_by_signature(@modl, result, params, params.size.to_u32), self
    end

    def add_function(name : String, type : FunctionType, var_types : Array(Type),
                     body : Expression) : Function
      puts "add function #{name}."
      Function.new LibBinaryen.add_function(@modl, name,
        type, var_types, var_types.size.to_u32, body)
    end

    def get_function(name : String)
      Function.new LibBinaryen.get_function(name)
    end

    def remove_function(name : String) : Void
      LibBinaryen.remove_function @modl, name
    end

    def remove(func : Function)
      remove_function func.name
    end

    def add_function_import(internal_name : String, external_module : String,
                            external_base : String, type : FunctionType)
      LibBinaryen.add_function_import(@modl, internal_name, external_module,
        external_base, type)
    end

    # # not support in MVP currently.
    # def add_tabel_import(internal_name : String, external_module : String,
    #                external_base : String) : Import
    #    Import.new LibBinaryen.add_table_import(@modl, internal_name, external_module,
    #                                                  external_base), @modl
    # end
    # # not support in MVP currently.
    # def add_memory_import(internal_name : String, external_module : String,
    #                external_base : String) : Import
    #    Import.new LibBinaryen.add_memory_import(@modl, internal_name, external_module,
    #                                                  external_base), @modl
    # end
    def add_global_import(internal_name : String, external_module : String,
                          external_base : String, type : Type) : Import
      Import.new LibBinaryen.add_global_import(@modl, internal_name, external_module,
        external_base, type), @modl
    end

    def remove_import(name : String)
      LibBinaryen.remove_import(@modl, name)
    end

    def remove(import : Import)
      remove_import(import.name)
    end

    def add_function_export(internal_name : String, external_name : String) : Export
      Export.new LibBinaryen.add_function_export @modl, internal_name, external_name
    end

    def add_memory_export(internal_name : String, external_name : String) : Export
      Export.new LibBinaryen.add_memory_export @modl, internal_name, external_name
    end

    def add_table_export(internal_name : String, external_name : String) : Export
      Export.new LibBinaryen.add_table_export @modl, internal_name, external_name
    end

    def add_global_export(internal_name : String, external_name : String) : Export
      Export.new LibBinaryen.add_global_export @modl, internal_name, external_name
    end

    def add_export(func : Function, extern_name : String) : Export
      Export.new LibBinaryen.add_function_export @modl, func.name, extern_name
    end

    def remove_export(external_name : String)
      LibBinaryen.remove_export external_name
    end

    def remove(export : Export)
      remove_export export.external_name
    end

    def add_global(name : String, type : Type, mutable : Bool, init : Expression)
      LibBinaryen.add_global @modl, name, type, (mutable ? 0_u8 : 1_u8), init
      name
    end

    record MemorySetting, initial : Int32 = 1, maximum : Int32 = 1,
      segments : Array(MemorySettingSegment) = [] of MemorySettingSegment,
      export_name : String = "default", shared : Bool = false
    getter memory_setting : MemorySetting = MemorySetting.new
    property start_point : Function?

    def memory_add_segment(offset : Int, data : Bytes)
      @memory_setting.segments << MemorySettingSegment.new data, exp_const offset
    end

    def compiled?
      @code
    end

    def code : Code
      @code || compile
    end

    getter source_map : Code?

    def compile(*, src_map_url : String? = nil, optimize = true)
      raise "The module has already been compiled" if @code
      if sp = start_point
        LibBinaryen.set_start @modl, sp
      end
      LibBinaryen.set_function_table @modl, 0, @table.functions.size,
        @table.functions.map(&.to_unsafe.as UInt8*), @table.functions.size
      LibBinaryen.set_memory @modl, memory_setting.initial, memory_setting.maximum,
        memory_setting.export_name, memory_setting.segments.map(&.data.to_unsafe),
        (memory_setting.segments.map { |x| x.passive ? 1i8 : 0i8 }),
        memory_setting.segments.map(&.offset.to_unsafe),
        memory_setting.segments.map(&.data.bytesize.to_u32),
        memory_setting.segments.size.to_u32,
        (memory_setting.shared ? 1u8 : 0u8)
      auto_drop
      optimize if optimize
      # run_passes ["flatten", "local-cse", "merge-blocks"]
      ret = LibBinaryen.module_allocate_and_write @modl, (src_map_url || Pointer(UInt8).null)
      @source_map = Code.new(Bytes.new ret.source_map, LibC.strlen(ret.source_map), read_only: true) if ret.source_map
      @code = Code.new(Bytes.new ret.binary.as(Pointer(UInt8)), ret.binary_bytes, read_only: true)
    end

    def write(src_map_url : String? = nil)
      compile src_map_url
    end

    def self.decompile(code : Bytes)
      Module.new LibBinaryen.module_read code, code.bytesize
    end

    def self.read(code : Bytes)
      decompile
    end

    def interpret
      LibBinaryen.module_interpret @modl
    end

    struct DebugInfo
      def initialize(@modl : Module)
      end

      getter modl

      struct Files
        def initialize(@modl : Module)
        end

        getter modl
        alias ID = LibBinaryen::Index

        def add(file : String) : ID
          LibBinaryen.module_add_debug_info_file_name @modl, file
        end

        def <<(file : String)
          push file
        end

        def add(*files : String)
          files.each { |a| push a }
        end

        def [](i : Int)
          String.new LibBinaryen.module_get_debug_info_file_name @modl, i.to_u32
        end
      end

      def files
        Files.new @modl
      end

      def set(f : Function, expr : Expression, file : Files::ID, line : Int, column : Int)
        LibBinaryen.function_set_debug_location f, expr, file, line.column
      end
    end

    def debug
      DebugInfo.new self
    end

    def print
      LibBinaryen.module_print self
    end

    def exp_block(name : String?, children : Array(Expression),
                  type : Type = Types::Undefined) : Expression
      Expression.new LibBinaryen.block(@modl, name, children.map(&.to_unsafe),
        children.size.to_u32, type)
    end

    def exp_block(name : String? = nil, type = Types::Undefined, &block) : Expression
      builder = BlockBuilder.new self
      yield builder
      builder.block name, type
    end

    def exp_block(name : String?, builder : BlockBuilder, type = Types::Undefined)
      builder.block name, type
    end

    def exp_if(condition : Expression, on_true : Expression? = nil,
               on_fail : Expression? = nil) : Expression
      on_fail ||= Expression::NULL
      on_true ||= exp_nop
      Expression.new LibBinaryen.if(@modl, condition, on_true, on_fail)
    end

    def exp_loop(name : String | Nil, body : Expression | BlockBuilder) : Expression
      body = body.block if body.is_a? BlockBuilder
      Expression.new LibBinaryen.loop(@modl, name, body)
    end

    def exp_break(name : String? = nil, condition : Expression | Nil = nil,
                  value : Expression | Nil = nil) : Expression
      condition ||= Expression::NULL
      value ||= Expression::NULL
      Expression.new LibBinaryen.break(@modl, name, condition, value)
    end

    def exp_switch(names : Array(String), default_name : String,
                   condition : Expression | Nil = nil, value : Expression | Nil = nil) : Expression
      ps = names.map &.to_unsafe
      condition ||= Expression::NULL
      value ||= Expression::NULL
      Expression.new LibBinaryen.switch(@modl, ps, names.size.to_u32,
        default_name, condition, value)
    end

    def exp_call(target : String, operands : Array(Expression),
                 return_type : Type) : Expression
      Expression.new LibBinaryen.call(@modl, target,
        operands.map(&.to_unsafe), operands.size.to_u32, return_type)
    end

    # def exp_call_import(target : String, operands : Array(Expression),
    #                    return_type : Type) : Expression
    #  Expression.new LibBinaryen.call_import(@modl, target,
    #    operands.map(&.to_unsafe), operands.size.to_u32, return_type)
    # end

    def exp_call_indirect(target : Expression, operands : Array(Expression),
                          type : FunctionType) : Expression
      Expression.new LibBinaryen.call_indirect(@modl, target, operands.map(&.to_unsafe),
        operands.size.to_u32, type.name)
    end

    def exp_call(func : Function, operands : Array(Expression)) : Expression
      # if func.is_a? Function
      exp_call func.name, operands, func.result_type
      # else
      #  exp_call_import func.name, operands, ret
      # end
    end

    def exp_call(func : Function, operands : Array(Expression)) : Expression
      # if func.is_a? Function
      exp_call func.name, operands, func.result_type
      # else
      #  exp_call_import func.name, operands, func.result_type
      # end
    end

    # TODO : function builder
    def exp_get_local(index : Int, type : Type) : Expression
      Expression.new LibBinaryen.local_get(@modl, index, type)
    end

    def exp_set_local(index : Int, val : Expression) : Expression
      Expression.new LibBinaryen.local_set(@modl, index, val)
    end

    def exp_tee_local(index : Int, val : Expression) : Expression
      Expression.new LibBinaryen.local_tee(@modl, index, val)
    end

    def exp_get_global(name : String, type : Type) : Expression
      Expression.new LibBinaryen.global_get(@modl, name, type)
    end

    def exp_set_global(name : String, val : Expression) : Expression
      Expression.new LibBinaryen.global_set(@modl, name, val)
    end

    def exp_load(bytes : Int, signed : Bool, offset : Int, align : Int,
                 type : Type, ptr : Expression) : Expression
      signed = (signed ? 1_u32 : 0_u32)
      Expression.new LibBinaryen.load(@modl, bytes.to_u32, signed,
        offset.to_u32, align.to_u32, type, ptr)
    end

    def exp_load(offset : Int, type : ::Class, ptr : Expression, align : Int = 0)
      signed = (type < Int::Signed ? 1_u32 : 0_u32)
      Expression.new LibBinaryen.load(@modl, type.size.to_u32, signed,
        offset.to_u32, align.to_u32, Types::ClassInWASM[type], ptr)
    end

    def exp_store(bytes : Int, offset : Int, align : Int, ptr : Expression,
                  val : Expression, type : Type = val.type) : Expression
      Expression.new LibBinaryen.store(@modl, bytes.to_u32, offset.to_u32, align.to_u32,
        ptr, val, type)
    end

    def exp_const(val : Int | Float) : Expression
      Expression.new LibBinaryen.const(@modl, Tools.to_literial(val))
    end

    def exp_const(val : Int | Float, type : Type) : Expression
      Expression.new LibBinaryen.const @modl, Tools.to_literial val, type
    end

    def exp(op : Operation, val : Expression) : Expression
      Expression.new LibBinaryen.unary(@modl, op, val)
    end

    def exp(op : Operation, v1 : Expression, v2 : Expression) : Expression
      Expression.new LibBinaryen.binary(@modl, op, v1, v2)
    end

    def exp_select(condition : Expression, if_true : Expression,
                   if_false : Expression) : Expression
      Expression.new LibBinaryen.select(@modl, condition, if_true, if_false)
    end

    def exp_drop(val : Expression) : Expression
      Expression.new LibBinaryen.drop(@modl, val)
    end

    def exp_return(val : Expression | Nil = nil) : Expression
      val = Expression::NULL unless val
      Expression.new LibBinaryen.return(@modl, val)
    end

    def exp_host(op : Operation, name : String,
                 operands : Array(Expression)) : Expression
      Expression.new LibBinaryen.host(@modl, op, name, operands,
        operands.size.to_u32)
    end

    def exp_nop : Expression
      Expression.new LibBinaryen.nop(@modl)
    end

    def exp_unreachable : Expression
      Expression.new LibBinaryen.unreachable(@modl)
    end

    def exp_atomic_load(bytes : Int, offset : Int, type : Type, ptr : Expression) : Expression
      Expression.new LibBinaryen.atomic_load(bytes.to_u32, offset.to_u32, type, ptr)
    end

    def exp_atomic_store(bytes : Int, offset : Int, ptr : Expression,
                         val : Expression, type : Type) : Expression
      Expression.new LibBinaryen.atomic_store(bytes.to_u32, offset.to_u32, ptr, val, type)
    end

    def exp_atomic(op : Operation, offset : Int, ptr : Expression, val : Expression,
                   type : BinaryenType) : Expression
      bytes = Types.size_of type
      Expression.new LibBinaryen.atomic_rmw(@modl, op, bytes, offset.to_u32, ptr, val, type)
    end

    def exp_atomic_cmpxchg(bytes : Int, offset : Int, ptr : Expression, expected : Expression,
                           replacement : Expression, type : Type) : Expression
      Expression.new LibBinaryen.atomic_cmpxchg(@modl, bytes.to_u32, offset.to_u32,
        ptr, expected, replacement, type)
    end

    def exp_atomic_wait(ptr : Expression, expected : Expression, timeout : Expression | Nil,
                        type : Type) : Expression
      timeout = todo
      Expression.new LibBinaryen.atomic_wait(@modl, ptr, expected, timeout, type)
    end

    def exp_atomic_wake(ptr : Expression, wake_cnt : Expression) : Expression
      Expression.new LibBinaryen.atomic_wake(@modl, ptr, wake_cnt)
    end

    def self.parse(s_exp : String) : Module
      Module.new LibBinaryen.module_parse s_exp
    end

    def print
      LibBinaryen.module_print @modl
    end

    def print_asmjs
      LibBinaryen.module_print_asmjs @modl
    end

    def valid : Bool
      LibBinaryen.module_validate(@modl) != 0
    end

    def optimize
      LibBinaryen.module_optimize @modl
    end

    def optimize(f : Function)
      LibBinaryen.function_optimize f, @modl
    end

    def run_passes(passes : Array(String))
      LibBinaryen.module_run_passes @modl, passes.map(&.to_unsafe), passes.size
    end

    def run_passes(passes : Array(String), f : Function)
      LibBinaryen.function_run_passes f, @modl, passes(&.to_unsafe), passes.size
    end

    def auto_drop
      LibBinaryen.module_auto_drop @modl
    end
  end

  def self.optimize_level : Int
    LibBinaryen.get_optimize_level
  end

  def self.optimize_level=(v : Int) : Int
    LibBinaryen.set_optimize_level v
    v
  end

  def self.shrink_level : Int
    LibBinaryen.get_shrink_level
  end

  def self.shrink_level=(v : Int) : Int
    LibBinaryen.set_shrink_level v
    v
  end

  def self.debug_info : Bool
    LibBinaryen.get_debug_info
  end

  def self.debug_info=(v : Bool) : Bool
    LibBinaryen.set_debug_info v
    v
  end

  def self.api_tracing=(on : Bool)
    LibBinaryen.set_api_tracing (on ? 1 : 0)
  end
end
