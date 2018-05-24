require "atomic"
require "./binaryen/*"

# TODO: Write documentation for `Binaryen`
module Binaryen
    alias Type = LibBinaryen::BinaryenType
    module Types
        None = LibBinaryen.BinaryenTypeNone()
        Int32= LibBinaryen.BinaryenTypeInt32()
        Int64= LibBinaryen.BinaryenTypeInt63()
        Float32= LibBinaryen.BinaryenTypeInt32()
        Float64= LibBinaryen.BinaryenTypeInt64()
        Auto = LibBinaryen.BinaryenTypeAuto()
        Unreachable = LibBinaryen.BinaryenTypeUnreachable()
        Undefined = Unreachable
        def self.size_of(t)
            case t
            when Types::Int32
                4_u32
            when Types::Float32
                4_u32
            when Types::Int64
                8_u32
            when Types::Float64
                8_u32
            else
                raise "invalid type #{type}"
            end
        end
    end
    module ExpressionIds
        Invalid = LibBinaryen.BinaryenInvalidId()
        Block = LibBinaryen.BinaryenBlockId()
        If = LibBinaryen.BinaryenIfId()
        Loop = LibBinaryen.BinaryenLoopId()
        Break = LibBinaryen.BinaryenBreakId()
        Switch = LibBinaryen.BinaryenSwitchId()
        Call = LibBinaryen.BinaryenCallId()
        CallImport = LibBinaryen.BinaryenCallImportId()
        CallIndirect = LibBinaryen.BinaryenCallIndirectId()
        GetLocal = LibBinaryen.BinaryenGetLocalId()
        SetLocal = LibBinaryen.BinaryenSetLocalId()
        GetGlobal = LibBinaryen.BinaryenGetGlobalId()
        SetGlobal = LibBinaryen.BinaryenSetGlobalId()
        Load = LibBinaryen.BinaryenLoadId()
        Store = LibBinaryen.BinaryenStoreId()
        Const = LibBinaryen.BinaryenConstId()
        Unary = LibBinaryen.BinaryenUnaryId()
        Binary = LibBinaryen.BinaryenBinaryId()
        Select = LibBinaryen.BinaryenSelectId()
        Drop = LibBinaryen.BinaryenDropId()
        Return = LibBinaryen.BinaryenReturnId()
        Host = LibBinaryen.BinaryenHostId()
        Nop = LibBinaryen.BinaryenNopId()
        Unreachable = LibBinaryen.BinaryenUnreachableId()
        AtomicCmpxchg = LibBinaryen.BinaryenAtomicCmpxchgId()
        AtomicRMW = LibBinaryen.BinaryenAtomicRMWId()
        AtomicWait = LibBinaryen.BinaryenAtomicWaitId()
        AtomicWake = LibBinaryen.BinaryenAtomicWakeId()
    end
    ExternalKind = LibBinaryen::ExternalKind
    module ExternalKinds
        Function = LibBinaryen.BinaryenExternalFunction()
        Table = LibBinaryen.BinaryenExternalTable()
        Memory = LibBinaryen.BinaryenExternalMemory()
        Global = LibBinaryen.BinaryenExternalGlobal()
    end
    Operation = LibBinaryen::BinaryenOp
    module Ops
        ClzInt32 = LibBinaryen.BinaryenClzInt32()
        CtzInt32 = LibBinaryen.BinaryenCtzInt32()
        PopcntInt32 = LibBinaryen.BinaryenPopcntInt32()
        NegFloat32 = LibBinaryen.BinaryenNegFloat32()
        AbsFloat32 = LibBinaryen.BinaryenAbsFloat32()
        CeilFloat32 = LibBinaryen.BinaryenCeilFloat32()
        FloorFloat32 = LibBinaryen.BinaryenFloorFloat32()
        TruncFloat32 = LibBinaryen.BinaryenTruncFloat32()
        NearestFloat32 = LibBinaryen.BinaryenNearestFloat32()
        SqrtFloat32 = LibBinaryen.BinaryenSqrtFloat32()
        EqZInt32 = LibBinaryen.BinaryenEqZInt32()
        ClzInt64 = LibBinaryen.BinaryenClzInt64()
        CtzInt64 = LibBinaryen.BinaryenCtzInt64()
        PopcntInt64 = LibBinaryen.BinaryenPopcntInt64()
        NegFloat64 = LibBinaryen.BinaryenNegFloat64()
        AbsFloat64 = LibBinaryen.BinaryenAbsFloat64()
        CeilFloat64 = LibBinaryen.BinaryenCeilFloat64()
        FloorFloat64 = LibBinaryen.BinaryenFloorFloat64()
        TruncFloat64 = LibBinaryen.BinaryenTruncFloat64()
        NearestFloat64 = LibBinaryen.BinaryenNearestFloat64()
        SqrtFloat64 = LibBinaryen.BinaryenSqrtFloat64()
        EqZInt64 = LibBinaryen.BinaryenEqZInt64()
        ExtendSInt32 = LibBinaryen.BinaryenExtendSInt32()
        ExtendUInt32 = LibBinaryen.BinaryenExtendUInt32()
        WrapInt64 = LibBinaryen.BinaryenWrapInt64()
        TruncSFloat32ToInt32 = LibBinaryen.BinaryenTruncSFloat32ToInt32()
        TruncSFloat32ToInt64 = LibBinaryen.BinaryenTruncSFloat32ToInt64()
        TruncUFloat32ToInt32 = LibBinaryen.BinaryenTruncUFloat32ToInt32()
        TruncUFloat32ToInt64 = LibBinaryen.BinaryenTruncUFloat32ToInt64()
        TruncSFloat64ToInt32 = LibBinaryen.BinaryenTruncSFloat64ToInt32()
        TruncSFloat64ToInt64 = LibBinaryen.BinaryenTruncSFloat64ToInt64()
        TruncUFloat64ToInt32 = LibBinaryen.BinaryenTruncUFloat64ToInt32()
        TruncUFloat64ToInt64 = LibBinaryen.BinaryenTruncUFloat64ToInt64()
        ReinterpretFloat32 = LibBinaryen.BinaryenReinterpretFloat32()
        ReinterpretFloat64 = LibBinaryen.BinaryenReinterpretFloat64()
        ConvertSInt32ToFloat32 = LibBinaryen.BinaryenConvertSInt32ToFloat32()
        ConvertSInt32ToFloat64 = LibBinaryen.BinaryenConvertSInt32ToFloat64()
        ConvertUInt32ToFloat32 = LibBinaryen.BinaryenConvertUInt32ToFloat32()
        ConvertUInt32ToFloat64 = LibBinaryen.BinaryenConvertUInt32ToFloat64()
        ConvertSInt64ToFloat32 = LibBinaryen.BinaryenConvertSInt64ToFloat32()
        ConvertSInt64ToFloat64 = LibBinaryen.BinaryenConvertSInt64ToFloat64()
        ConvertUInt64ToFloat32 = LibBinaryen.BinaryenConvertUInt64ToFloat32()
        ConvertUInt64ToFloat64 = LibBinaryen.BinaryenConvertUInt64ToFloat64()
        PromoteFloat32 = LibBinaryen.BinaryenPromoteFloat32()
        DemoteFloat64 = LibBinaryen.BinaryenDemoteFloat64()
        ReinterpretInt32 = LibBinaryen.BinaryenReinterpretInt32()
        ReinterpretInt64 = LibBinaryen.BinaryenReinterpretInt64()
        ExtendS8Int32 = LibBinaryen.BinaryenExtendS8Int32()
        ExtendS16Int32 = LibBinaryen.BinaryenExtendS16Int32()
        ExtendS8Int64 = LibBinaryen.BinaryenExtendS8Int64()
        ExtendS16Int64 = LibBinaryen.BinaryenExtendS16Int64()
        ExtendS32Int64 = LibBinaryen.BinaryenExtendS32Int64()
        AddInt32 = LibBinaryen.BinaryenAddInt32()
        SubInt32 = LibBinaryen.BinaryenSubInt32()
        MulInt32 = LibBinaryen.BinaryenMulInt32()
        DivSInt32 = LibBinaryen.BinaryenDivSInt32()
        DivUInt32 = LibBinaryen.BinaryenDivUInt32()
        RemSInt32 = LibBinaryen.BinaryenRemSInt32()
        RemUInt32 = LibBinaryen.BinaryenRemUInt32()
        AndInt32 = LibBinaryen.BinaryenAndInt32()
        OrInt32 = LibBinaryen.BinaryenOrInt32()
        XorInt32 = LibBinaryen.BinaryenXorInt32()
        ShlInt32 = LibBinaryen.BinaryenShlInt32()
        ShrUInt32 = LibBinaryen.BinaryenShrUInt32()
        ShrSInt32 = LibBinaryen.BinaryenShrSInt32()
        RotLInt32 = LibBinaryen.BinaryenRotLInt32()
        RotRInt32 = LibBinaryen.BinaryenRotRInt32()
        EqInt32 = LibBinaryen.BinaryenEqInt32()
        NeInt32 = LibBinaryen.BinaryenNeInt32()
        LtSInt32 = LibBinaryen.BinaryenLtSInt32()
        LtUInt32 = LibBinaryen.BinaryenLtUInt32()
        LeSInt32 = LibBinaryen.BinaryenLeSInt32()
        LeUInt32 = LibBinaryen.BinaryenLeUInt32()
        GtSInt32 = LibBinaryen.BinaryenGtSInt32()
        GtUInt32 = LibBinaryen.BinaryenGtUInt32()
        GeSInt32 = LibBinaryen.BinaryenGeSInt32()
        GeUInt32 = LibBinaryen.BinaryenGeUInt32()
        AddInt64 = LibBinaryen.BinaryenAddInt64()
        SubInt64 = LibBinaryen.BinaryenSubInt64()
        MulInt64 = LibBinaryen.BinaryenMulInt64()
        DivSInt64 = LibBinaryen.BinaryenDivSInt64()
        DivUInt64 = LibBinaryen.BinaryenDivUInt64()
        RemSInt64 = LibBinaryen.BinaryenRemSInt64()
        RemUInt64 = LibBinaryen.BinaryenRemUInt64()
        AndInt64 = LibBinaryen.BinaryenAndInt64()
        OrInt64 = LibBinaryen.BinaryenOrInt64()
        XorInt64 = LibBinaryen.BinaryenXorInt64()
        ShlInt64 = LibBinaryen.BinaryenShlInt64()
        ShrUInt64 = LibBinaryen.BinaryenShrUInt64()
        ShrSInt64 = LibBinaryen.BinaryenShrSInt64()
        RotLInt64 = LibBinaryen.BinaryenRotLInt64()
        RotRInt64 = LibBinaryen.BinaryenRotRInt64()
        EqInt64 = LibBinaryen.BinaryenEqInt64()
        NeInt64 = LibBinaryen.BinaryenNeInt64()
        LtSInt64 = LibBinaryen.BinaryenLtSInt64()
        LtUInt64 = LibBinaryen.BinaryenLtUInt64()
        LeSInt64 = LibBinaryen.BinaryenLeSInt64()
        LeUInt64 = LibBinaryen.BinaryenLeUInt64()
        GtSInt64 = LibBinaryen.BinaryenGtSInt64()
        GtUInt64 = LibBinaryen.BinaryenGtUInt64()
        GeSInt64 = LibBinaryen.BinaryenGeSInt64()
        GeUInt64 = LibBinaryen.BinaryenGeUInt64()
        AddFloat32 = LibBinaryen.BinaryenAddFloat32()
        SubFloat32 = LibBinaryen.BinaryenSubFloat32()
        MulFloat32 = LibBinaryen.BinaryenMulFloat32()
        DivFloat32 = LibBinaryen.BinaryenDivFloat32()
        CopySignFloat32 = LibBinaryen.BinaryenCopySignFloat32()
        MinFloat32 = LibBinaryen.BinaryenMinFloat32()
        MaxFloat32 = LibBinaryen.BinaryenMaxFloat32()
        EqFloat32 = LibBinaryen.BinaryenEqFloat32()
        NeFloat32 = LibBinaryen.BinaryenNeFloat32()
        LtFloat32 = LibBinaryen.BinaryenLtFloat32()
        LeFloat32 = LibBinaryen.BinaryenLeFloat32()
        GtFloat32 = LibBinaryen.BinaryenGtFloat32()
        GeFloat32 = LibBinaryen.BinaryenGeFloat32()
        AddFloat64 = LibBinaryen.BinaryenAddFloat64()
        SubFloat64 = LibBinaryen.BinaryenSubFloat64()
        MulFloat64 = LibBinaryen.BinaryenMulFloat64()
        DivFloat64 = LibBinaryen.BinaryenDivFloat64()
        CopySignFloat64 = LibBinaryen.BinaryenCopySignFloat64()
        MinFloat64 = LibBinaryen.BinaryenMinFloat64()
        MaxFloat64 = LibBinaryen.BinaryenMaxFloat64()
        EqFloat64 = LibBinaryen.BinaryenEqFloat64()
        NeFloat64 = LibBinaryen.BinaryenNeFloat64()
        LtFloat64 = LibBinaryen.BinaryenLtFloat64()
        LeFloat64 = LibBinaryen.BinaryenLeFloat64()
        GtFloat64 = LibBinaryen.BinaryenGtFloat64()
        GeFloat64 = LibBinaryen.BinaryenGeFloat64()
        PageSize = LibBinaryen.BinaryenPageSize()
        CurrentMemory = LibBinaryen.BinaryenCurrentMemory()
        GrowMemory = LibBinaryen.BinaryenGrowMemory()
        HasFeature = LibBinaryen.BinaryenHasFeature()
        module Atomic
            Add = LibBinaryen.BinaryenAtomicRMWAdd()
            Sub = LibBinaryen.BinaryenAtomicRMWSub()
            And = LibBinaryen.BinaryenAtomicRMWAnd()
            Or = LibBinaryen.BinaryenAtomicRMWOr()
            Xor = LibBinaryen.BinaryenAtomicRMWXor()
            Xchg = LibBinaryen.BinaryenAtomicRMWXchg()
            Ops = [Add, Sub, And, Or, Xor, Xchg]
        end
        def self.is_atomic?(id : Operation) : Bool
            Atomic::Ops.include? id
        end
    end
    struct Expression
        NULL = Expression.new Pointer(Void).null
        alias Id = LibBinaryen::ExpressionId
        alias Ids = ExpressionIds
        def initialize(ref : LibBinaryen::BinaryenExpressionRef)
            @ref = ref
        end
        getter ref : LibBinaryen::BinaryenExpressionRef
        def id : Id
            LibBinaryen::BinaryenExpressionGetId(@ref)
        end
        def print
            LibBinaryen::BinaryenExpressionPrint(@ref)
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
        def name
            case id
            when Ids::Block
                String.new LibBinaryen::BinaryenBlockGetName(@ref).as(Pointer(UInt8))
            when Ids::Loop
                String.new LibBinaryen::BinaryenLoopGetName(@ref).as(Pointer(UInt8))
            when Ids::Break
                String.new LibBinaryen::BinaryenBreakGetName(@ref).as(Pointer(UInt8))
            else
                nil
            end
        end
        def type : Type
            LibBinaryen::BinaryenExpressionGetType(@ref)
        end
        struct SwitchNames
            getter exp : Expression
            include Enumerable(String)
            def initialize(exp : Expression)
                @exp = exp
            end
            def size : Int
                LibBinaryen::BinaryenSwitchGetNumNames(exp.ref)
            end
            def [](i : Int) : String
                String.new LibBinaryen::BinaryenSwitchGetName(exp.ref, i.to_u32)
            end
            def each(&block : String -> _)
                size.times do |i|
                    yield self[i]
                end
            end
            def default : String
                String.new LibBinaryen::BinaryenSwitchGetDefaultName(exp.ref)
            end
        end
        def switch_names
            raise "expression must be Switch, but #{id}" unless id == Ids::Switch

        end
        struct Children
            getter exp : Expression
            include Enumerable(Expression)
            def initialize(exp : Expression)
                @exp = exp
            end
            def size : Int
                case exp.id
                when Ids::Block
                    LibBinaryen::BinaryenBlockGetNumChildren(exp.ref)
                when Ids::CallImport
                    LibBinaryen::BinaryenCallImportGetNumOperands(exp.ref)
                when Ids::Call
                    LibBinaryen::BinaryenCallGetNumOperands(exp.ref)
                when Ids::CallIndirect
                    LibBinaryen::BinaryenCallIndirectGetNumOperands(exp.ref)
                when Ids::Host
                    LibBinaryen::BinaryenHostGetNumOperands(exp.ref)
                else
                    raise "bad expression type"
                end
            end
            def [](i : Int) : Expression
                Expression.new case exp.id
                when Ids::Block
                    LibBinaryen::BinaryenBlockGetChild(exp.ref, i.to_u32)
                when Ids::Call
                    LibBinaryen::BinaryenCallGetOperand(exp.ref, i.to_u32)
                when Ids::CallImport
                    LibBinaryen::BinaryenCallImportGetOperand(exp.ref, i.to_u32)
                when Ids::CallIndirect
                    LibBinaryen::BinaryenCallIndirectGetOperand(exp.ref, i.to_u32)
                when Ids::Host
                    LibBinaryen::BinaryenHostGetOperand(exp.ref, i.to_u32)
                else
                    raise "bad expression type"
                end
            end
            def each(&block : Expression -> _)
                size.times do |i|
                    yield self[i]
                end
            end
        end
        def children
            raise "expression must be Block, but #{id}" unless is_block?
            Children.new self
        end
        def operands
            raise "expression must be Call, CallImport, CallIndirect or Host, but #{id}" unless 
                id == Ids::Call || id == Ids::CallImport || id == Ids::CallIndirect ||
                    id == Ids::Host
            Children.new self
        end
        def condition : Expression
            Expression.new case id
            when Ids::If
                LibBinaryen::BinaryenIfGetCondition(@ref)
            when Ids::Break
                LibBinaryen::BinaryenBreakGetCondition(@ref)
            when Ids::Switch
                LibBinaryen::BinaryenSwitchGetCondition(@ref)
            when Ids::Select
                LibBinaryen::BinaryenSelectGetCondition(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def value : Expression | Int | String
            Expression.new case id
            when Ids::Break
                LibBinaryen::BinaryenBreakGetValue(@ref)
            when Ids::Switch
                LibBinaryen::BinaryenSwitchGetValue(@ref)
            when Ids::SetLocal
                LibBinaryen::BinaryenSetLocalGetValue(@ref)
            when Ids::SetGlobal
                LibBinaryen::BinaryenSetGlobalGetValue(@ref)
            when Ids::Store
                LibBinaryen::BinaryenStoreGetValue(@ref)
            when Ids::Return
                LibBinaryen::BinaryenReturnGetValue(@ref)
            when Ids::AtomicRMW
                LibBinaryen::BinaryenAtomicRMWGetValue(@ref)
            when Ids::Const
                return case type
                when Types::Int32
                    BinaryenConstGetValueI32(@ref)
                when Types::Int64
                    BinaryenConstGetValueI64(@ref)
                when Types::Float32
                    BinaryenConstGetValueF32(@ref)
                when Types::Float64
                    BinaryenConstGetValueF64(@ref)
                else
                    raise "bad Const value type"
                end
            else
                raise "bad Expression Id"
            end
        end
        def value_unary : Expression
            LibBinaryen::BinaryenUnaryGetValue(@ref)
        end
        def value_left : Expression
            LibBinaryen::BinaryenBinaryGetLeft(@ref)
        end
        def value_right : Expression
            LibBinaryen::BinaryenBinaryGetRight(@ref)
        end
        def if_true : Expression
            Expression.new case id
            when Ids::If
                LibBinaryen::BinaryenIfGetIfTrue(@ref)
            when Ids::Select
                LibBinaryen::BinaryenSelectGetIfTrue(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def if_false : Expression
            Expression.new case id
            when Ids::If
                LibBinaryen::BinaryenSelectGetIfTrue(@ref)
            when Ids::Select
                LibBinaryen::BinaryenSelectGetIfFalse(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def body : Expression
            raise "expression must be Loop, but #{id}" unless is_loop?
            Expression.new LibBinaryen::BinaryenLoopGetBody(@ref)
        end
        def target : Expression
            raise "expression must be Call, but #{id}" unless id == Ids::Call
            Expression.new LibBinaryen::BinaryenCallGetTarget(@ref)
        end
        # local_ind or global_ind
        def var_ind : Int | String
            case id
            when Ids::GetLocal
                LibBinaryen::BinaryenGetLocalGetIndex(@ref)
            when Ids::SetLocal
                LibBinaryen::BinaryenSetLocalGetIndex(@ref)
            when Ids::GetGlobal
                String.new LibBinaryen::BinaryenGetGlobalGetName(@ref)
            when Ids::SetGlobal
                String.new LibBinaryen::BinaryenSetGlobalGetName(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def op_unary : Operation
            LibBinaryen::BinaryenUnaryGetOp(@ref)
        end
        def op_binary : Operation
            LibBinaryen::BinaryenBinaryGetOp(@ref)
        end
        def op : Operation
            case id
            when Ids::Host
                LibBinaryen::BinaryenHostGetOp(@ref)
            when Ids::AtomicRMW
                LibBinaryen::BinaryenAtomicRMWGetOp(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def is_atomic? : Bool
            case id
            when Ids::Load
                LibBinaryen::BinaryenLoadIsAtomic(@ref).to_i32 != 0_u32
            when Ids::Store
                LibBinaryen::BinaryenStoreIsAtomic(@ref).to_i32 != 0_u32
            else
                raise "bad Expression Id"
            end
        end
        def bytes : Int
            case id
            when Ids::Load
                LibBinaryen::BinaryenLoadGetBytes(@ref)
            when Ids::Store
                LibBinaryen::BinaryenStoreGetBytes(@ref)
            when Ids::AtomicRMW
                LibBinaryen::BinaryenAtomicRMWGetBytes(@ref)
            when Ids::AtomicCmpxchg
                LibBinaryen::BinaryenAtomicCmpxchgGetBytes(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def offset : Int
            case id
            when Ids::Load
                LibBinaryen::BinaryenLoadGetOffset(@ref)
            when Ids::Store
                LibBinaryen::BinaryenStoreGetOffset(@ref)
            when Ids::AtomicRMW
                LibBinaryen::BinaryenAtomicRMWGetOffset(@ref)
            when Ids::AtomicCmpxchg
                LibBinaryen::BinaryenAtomicCmpxchgGetOffset(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def align : Int
            case id
            when Ids::Load
                LibBinaryen::BinaryenLoadGetAlign(@ref)
            when Ids::Store
                LibBinaryen::BinaryenStoreGetAlign(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def ptr : Expression
            Expression.new case id
            when Ids::Load
                LibBinaryen::BinaryenLoadGetPtr(@ref)
            when Ids::Store
                LibBinaryen::BinaryenStoreGetPtr(@ref)
            when Ids::AtomicRMW
                LibBinaryen::BinaryenAtomicRMWGetPtr(@ref)
            when Ids::AtomicCmpxchg
                LibBinaryen::BinaryenAtomicCmpxchgGetPtr(@ref)
            when Ids::AtomicWait
                LibBinaryen::BinaryenAtomicWaitGetPtr(@ref)
            when Ids::AtomicWake
                LibBinaryen::BinaryenAtomicWakeGetPtr(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def expected : Expression
            Expression.new case id
            when Ids::AtomicCmpxchg
                LibBinaryen::BinaryenAtomicCmpxchgGetExpected(@ref)
            when Ids::AtomicWait
                LibBinaryen::BinaryenAtomicWaitGetExpected(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def replacement : Expression
            Expression.new case id
            when Ids::AtomicCmpxchg
                LibBinaryen::BinaryenAtomicCmpxchgGetReplacement(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def expected_type : Type
            case id
            when Ids::AtomicWait
                LibBinaryen::BinaryenAtomicWaitGetExpectedType(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def wake_count : Expression
            Expression.new case id
            when Ids::AtomicWait
                LibBinaryen::BinaryenAtomicWakeGetWakeCount(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def timeout : Expression
            Expression.new case id
            when Ids::AtomicWait
                LibBinaryen::BinaryenAtomicWaitGetTimeout(@ref)
            else
                raise "bad Expression Id"
            end
        end
    end
    struct FunctionType
        def initialize(name : String, ftref : LibBinaryen::BinaryenFunctionTypeRef,
                      modl : Module)
            @ftref, @name, @modl = ftref, name, modl
        end
        property ftref : LibBinaryen::BinaryenFunctionTypeRef, 
            name : String, modl : Module
        def remove
            @modl.remove_function_type @name
        end
    end
    struct LocalVariable
        @index = 0_u32
        @type = Types::None
        property index : UInt32, type : Type
    end
    module Tools
        def self.to_literial(val : Int | Float)
            case val
            when Int64
                LibBinaryen.BinaryenLiteralInt64(val.to_i64)
            when Int
                LibBinaryen.BinaryenLiteralInt32(val.to_i32)
            when Float64
                LibBinaryen.BinaryenLiteralFloat32(val.ref)
            when Float32
                LibBinaryen.BinaryenLiteralFloat64(val.ref)
            else
                raise "Bad type : #{typeof(val.ref)}"
            end
        end
    end
    class Module
        def initialize()
            @modl = LibBinaryen.BinaryenModuleCreate()
            @id = Atomic(UInt32).new(1)
        end
        getter modl : LibBinaryen::BinaryenModuleRef
        def finalize()
            LibBinaryen.BinaryenModuleDispose(@modl)
        end
        def add_function_type(name : String | Nil, result : Type, 
                params : Array(Type)) : FunctionType
            name = name.nil? ? name.to_unsafe : "_xrystal_ftype_#{@id.add 1}"
            f = LibBinaryen.BinaryenAddFunctionType(@modl, name,
                        result, params.to_unsafe, params.size.to_u32)
            FunctionType.new name, f, self
        end
        def remove_function_type(name : String)
            Expression.new LibBinaryen.BinaryenRemoveFunctionType(@modl, name,to_unsafe)
        end
        def exp_block(name : String | Nil, children : Array(Expression), 
                  type : Type | Nil = Types::Undefine) : Expression
            name = name.nil? ? name.to_unsafe : Pointer(UInt8).null
            Expression.new LibBinaryen.BinaryenBlock(@modl, name, children.to_unsafe, 
                                       children.size.to_u32, type)
        end
        def exp_if(condition : Expression, on_true : Expression, 
               on_fail : Expression | Nil = nil) : Expression
            on_fail = Expression::NULL unless on_fail
            Expression.new LibBinaryen.BinaryenIf(@modl, condition.ref, on_true, on_fail)
        end
        def exp_loop(name : String | Nil, body : Expression) : Expression
            name = name.nil? ? name.to_unsafe : Pointer(UInt8).null
            Expression.new LibBinaryen.BinaryenLoop(@modl, name, body)
        end
        def exp_break(name : String | Nil = nil, condition : Expression | Nil = nil,
                  value : Expression | Nil = nil) : Expression
            name = name.nil? ? name.to_unsafe : Pointer(UInt8).null
            condition ||= Expression::NULL
            value ||= Expression::NULL
            Expression.new LibBinaryen.BinaryenBreak(@modl, name, condition.ref, value)
        end
        def exp_switch(names : Array(String), default_name : String, 
                  condition : Expression | Nil = nil, value : Expression | Nil = nil) : Expression
            ps = names.map &.to_unsafe
            default_name = default_name.to_unsafe
            condition ||= Expression::NULL
            value ||= Expression::NULL
            Expression.new LibBinaryen.BinaryenSwitch(@modl, ps, names.size.to_u32, 
                        default_name.to_unsafe, condition.ref, value)
        end
        def exp_call(target : String, operands : Array(Expression), 
                     return_type : Type) : Expression
            Expression.new LibBinaryen.BinaryenCall(@modl, target.to_unsafe, operands.to_unsafe,
                        operands.size.to_u32, returnType)
        end
        def exp_call_import(target : String, operands : Array(Expression), 
                     return_type : Type) : Expression
            Expression.new LibBinaryen.BinaryenCallImport(@modl, target.to_unsafe, operands.to_unsafe,
                        operands.size.to_u32, returnType)
        end
        def exp_call_indirect(target : Expression, operands : Array(Expression), 
                             type : FunctionType) : Expression
            Expression.new LibBinaryen.BinaryenCallIndirect(@modl, target, operands.to_unsafe,
                                              operands.size.to_u32, type.name)
        end
        # TODO : function builder
        def exp_get_local(index : Int, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenGetLocal(@modl, index, type)
        end
        def exp_set_local(index : Int, val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenSetLocal(@modl, index, val.ref)
        end
        def exp_tee_local(index : Int, val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenTeeLocal(@modl, index, val.ref)
        end
        def exp_get_gloval(name : String, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenGetGlobal(@modl, name.to_unsafe, type)
        end
        def exp_set_global(name : String, val : Expression) : Expression
            LExpression.new ibBinaryen.BinaryenSetGlobal(@modl, name.to_unsafe, val.ref)
        end
        def exp_load(bytes : Int, signed : Bool, offest : Int, align : Int,
                    type : Type, ptr : Expression) : Expression
            signed = (signed ? 1_u32 : 0_u32)
            LExpression.new ibBinaryen.BinaryenLoad(@modl, bytes.to_u32, signed,
                    offest.to_u32, align.to_u32, type, ptr)
        end
        def exp_store(bytes : Int, offset : Int, align : Int, ptr : Expression, 
                     val : Expression, type : Type) : Expression
            LExpression.new ibBinaryen.BinaryenStore(@modl, bytes.to_u32, align.to_u32,
                        ptr, val.ref, type)
        end
        def exp_const(val : Int | Float) : Expression
            LExpression.new ibBinaryen.BinaryenConst(@modl, Tools.to_literial(val.ref))
        end
        def exp(op : Operation, val : Expression) : Expression
            LExpression.new ibBinaryen.BinaryenUnary(@modl, op, val.ref)
        end
        def exp(op : Operation, v1 : Expression, v2 : Expression) : Expression
            Expression.new LibBinaryen.BinaryenBinary(@modl, op, v1, v2)
        end
        def exp_select(condition : Expression, if_true : Expression,
                      if_false : Expression) : Expression
            Expression.new LibBinaryen.BinaryenSelect(@modl, condition.ref, if_true.ref, if_false.ref)
        end
        def exp_drop(val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenDrop(@modl, val.ref)
        end
        def exp_return(val : Expression | Nil) : Expression
            val = Expression::NULL unless val
            Expression.new LibBinaryen.BinaryenReturn(val.ref)
        end
        def exp_host(op : Operation, name : String, 
                     operands : Array(Expression)) : Expression
            Expression.new LibBinaryen.BinaryenHost(@modl, op, name.to_unsafe, operands.to_unsafe,
                                      operands.size.to_u32)
        end
        def exp_nop() : Expression
            Expression.new LibBinaryen.BinaryenNop(@modl)
        end
        def exp_unreachable() : Expression
            Expression.new LibBinaryen.BinaryenUnreachable(@modl)
        end
        def exp_atomic_load(bytes : Int, offset : Int, type : Type, ptr : Expression) : Expression
            Expression.new LibBinaryen.BinaryenAtomicLoad(bytes.to_u32, offest.to_u32, type, ptr)
        end
        def exp_atomic_store(bytes : Int, offset : Int, ptr : Expression, 
                             val : Expression, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenAtomicStore(bytes.to_u32, offset.to_u32, ptr, val.ref, type)
        end
        def exp_atomic(op : Operation, offset : Int, ptr : Expression, val : Expression, 
                       type : BinaryenType) : Expression
            bytes = Types.size_of type
            Expression.new LibBinaryen.BinaryenAtomicRMW(@modl, op, bytes, offset.to_u32, ptr, val.ref, type)
        end
        def exp_atomic_cmpxchg(bytes : Int, offset : Int, ptr : Expression, expected : Expression,
                              replacement : Expression, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenAtomicCmpxchg(@modl, bytes.to_u32, offset.to_u32, 
                                               ptr, expected, replacement, type)
        end
        def exp_atomic_wait(ptr : Expression, expected : Expression, timeout : Expression | Nil,
                            type : Type) : Expression
            timeout = todo
            Expression.new LibBinaryen.BinaryenAtomicWait(@modl, ptr, expected, timeout, type)
        end
        def exp_atomic_wake(ptr : Expression, wake_cnt : Expression) : Expression
            Expression.new LibBinaryen.BinaryenAtomicWake(@modl, ptr, wake_cnt)
        end
    end
end

