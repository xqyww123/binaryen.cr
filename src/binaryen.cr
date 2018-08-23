require "atomic"
require "./binaryen/*"

module Binaryen
    alias Type = LibBinaryen::BinaryenType
    module Types
        None = LibBinaryen.BinaryenTypeNone()
        Int32= LibBinaryen.BinaryenTypeInt32()
        Int64= LibBinaryen.BinaryenTypeInt64()
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
        ClassInWASM = {::Int32 => Int32, ::Int64 => Int64, ::Float32 => Float32, ::Float64 => Float64, Nil => None, Void => None, ::UInt32 => Int32, ::UInt64 => Int64 }
        ClassInCrystal = {Int32 => ::Int32, Int64 => ::Int64, Float32 => ::Float32, Float64 => ::Float64, None => Nil}
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
    alias ExternalKind = LibBinaryen::BinaryenExternalKind
    module ExternalKinds
        Function = LibBinaryen.BinaryenExternalFunction()
        Table = LibBinaryen.BinaryenExternalTable()
        Memory = LibBinaryen.BinaryenExternalMemory()
        Global = LibBinaryen.BinaryenExternalGlobal()
    end
    alias Operation = LibBinaryen::BinaryenOp
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
        alias Id = LibBinaryen::BinaryenExpressionId
        alias Ids = ExpressionIds
        def initialize(@ref : LibBinaryen::BinaryenExpressionRef)
        end
        def to_unsafe
            @ref
        end
        def id : Id
            return Ids::Invalid if @ref.null?
            LibBinaryen.BinaryenExpressionGetId(@ref)
        end
        def print
            p @ref if @ref.null?
            LibBinaryen.BinaryenExpressionPrint(@ref)
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
        def name : String | Nil
            String.new case id
            when Ids::Block
                LibBinaryen.BinaryenBlockGetName(@ref).as(Pointer(UInt8))
            when Ids::Loop
                LibBinaryen.BinaryenLoopGetName(@ref).as(Pointer(UInt8))
            when Ids::Break
                LibBinaryen.BinaryenBreakGetName(@ref).as(Pointer(UInt8))
            else
                return nil
            end
        end
        def type : Type
            LibBinaryen.BinaryenExpressionGetType(@ref)
        end
        struct SwitchNames
            getter exp : Expression
            include Indexable(String)
            def initialize(@exp : Expression)
            end
            def size : Int
                LibBinaryen.BinaryenSwitchGetNumNames(exp)
            end
            def unsafe_at(i : Int) : String
                String.new LibBinaryen.BinaryenSwitchGetName(exp, i.to_u32)
            end
            def default : String
                String.new LibBinaryen.BinaryenSwitchGetDefaultName(exp)
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
                    LibBinaryen.BinaryenBlockGetNumChildren(exp)
                when Ids::CallImport
                    LibBinaryen.BinaryenCallImportGetNumOperands(exp)
                when Ids::Call
                    LibBinaryen.BinaryenCallGetNumOperands(exp)
                when Ids::CallIndirect
                    LibBinaryen.BinaryenCallIndirectGetNumOperands(exp)
                when Ids::Host
                    LibBinaryen.BinaryenHostGetNumOperands(exp)
                else
                    raise "bad expression type"
                end
            end
            def unsafe_at(i : Int) : Expression
                Expression.new case exp.id
                when Ids::Block
                    LibBinaryen.BinaryenBlockGetChild(exp, i.to_u32)
                when Ids::Call
                    LibBinaryen.BinaryenCallGetOperand(exp, i.to_u32)
                when Ids::CallImport
                    LibBinaryen.BinaryenCallImportGetOperand(exp, i.to_u32)
                when Ids::CallIndirect
                    LibBinaryen.BinaryenCallIndirectGetOperand(exp, i.to_u32)
                when Ids::Host
                    LibBinaryen.BinaryenHostGetOperand(exp, i.to_u32)
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
            raise "expression must be Call, CallImport, CallIndirect or Host, but #{id}" unless 
                id == Ids::Call || id == Ids::CallImport || id == Ids::CallIndirect ||
                    id == Ids::Host
            Children.new self
        end
        def condition : Expression
            Expression.new case id
            when Ids::If
                LibBinaryen.BinaryenIfGetCondition(@ref)
            when Ids::Break
                LibBinaryen.BinaryenBreakGetCondition(@ref)
            when Ids::Switch
                LibBinaryen.BinaryenSwitchGetCondition(@ref)
            when Ids::Select
                LibBinaryen.BinaryenSelectGetCondition(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def value : Expression | Int | String
            Expression.new case id
            when Ids::Break
                LibBinaryen.BinaryenBreakGetValue(@ref)
            when Ids::Switch
                LibBinaryen.BinaryenSwitchGetValue(@ref)
            when Ids::SetLocal
                LibBinaryen.BinaryenSetLocalGetValue(@ref)
            when Ids::SetGlobal
                LibBinaryen.BinaryenSetGlobalGetValue(@ref)
            when Ids::Store
                LibBinaryen.BinaryenStoreGetValue(@ref)
            when Ids::Return
                LibBinaryen.BinaryenReturnGetValue(@ref)
            when Ids::AtomicRMW
                LibBinaryen.BinaryenAtomicRMWGetValue(@ref)
            when Ids::Const
                return case type
                when Types::Int32
                    LibBinaryen.BinaryenConstGetValueI32(@ref)
                when Types::Int64
                    LibBinaryen.BinaryenConstGetValueI64(@ref)
                when Types::Float32
                    LibBinaryen.BinaryenConstGetValueF32(@ref)
                when Types::Float64
                    LibBinaryen.BinaryenConstGetValueF64(@ref)
                else
                    raise "bad Const value type"
                end
            else
                raise "bad Expression Id"
            end
        end
        def value_unary : Expression
            LibBinaryen.BinaryenUnaryGetValue(@ref)
        end
        def value_left : Expression
            LibBinaryen.BinaryenBinaryGetLeft(@ref)
        end
        def value_right : Expression
            LibBinaryen.BinaryenBinaryGetRight(@ref)
        end
        def if_true : Expression
            Expression.new case id
            when Ids::If
                LibBinaryen.BinaryenIfGetIfTrue(@ref)
            when Ids::Select
                LibBinaryen.BinaryenSelectGetIfTrue(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def if_false : Expression
            Expression.new case id
            when Ids::If
                LibBinaryen.BinaryenSelectGetIfTrue(@ref)
            when Ids::Select
                LibBinaryen.BinaryenSelectGetIfFalse(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def body : Expression
            raise "expression must be Loop, but #{id}" unless is_loop?
            Expression.new LibBinaryen.BinaryenLoopGetBody(@ref)
        end
        def target : Expression
            raise "expression must be Call, but #{id}" unless id == Ids::Call
            Expression.new LibBinaryen.BinaryenCallGetTarget(@ref)
        end
        # local_ind or global_ind
        def var_ind : Int | String
            case id
            when Ids::GetLocal
                LibBinaryen.BinaryenGetLocalGetIndex(@ref)
            when Ids::SetLocal
                LibBinaryen.BinaryenSetLocalGetIndex(@ref)
            when Ids::GetGlobal
                String.new LibBinaryen.BinaryenGetGlobalGetName(@ref)
            when Ids::SetGlobal
                String.new LibBinaryen.BinaryenSetGlobalGetName(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def op_unary : Operation
            LibBinaryen.BinaryenUnaryGetOp(@ref)
        end
        def op_binary : Operation
            LibBinaryen.BinaryenBinaryGetOp(@ref)
        end
        def op : Operation
            case id
            when Ids::Host
                LibBinaryen.BinaryenHostGetOp(@ref)
            when Ids::AtomicRMW
                LibBinaryen.BinaryenAtomicRMWGetOp(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def is_atomic? : Bool
            case id
            when Ids::Load
                LibBinaryen.BinaryenLoadIsAtomic(@ref).to_i32 != 0_u32
            when Ids::Store
                LibBinaryen.BinaryenStoreIsAtomic(@ref).to_i32 != 0_u32
            else
                raise "bad Expression Id"
            end
        end
        def bytes : Int
            case id
            when Ids::Load
                LibBinaryen.BinaryenLoadGetBytes(@ref)
            when Ids::Store
                LibBinaryen.BinaryenStoreGetBytes(@ref)
            when Ids::AtomicRMW
                LibBinaryen.BinaryenAtomicRMWGetBytes(@ref)
            when Ids::AtomicCmpxchg
                LibBinaryen.BinaryenAtomicCmpxchgGetBytes(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def offset : Int
            case id
            when Ids::Load
                LibBinaryen.BinaryenLoadGetOffset(@ref)
            when Ids::Store
                LibBinaryen.BinaryenStoreGetOffset(@ref)
            when Ids::AtomicRMW
                LibBinaryen.BinaryenAtomicRMWGetOffset(@ref)
            when Ids::AtomicCmpxchg
                LibBinaryen.BinaryenAtomicCmpxchgGetOffset(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def align : Int
            case id
            when Ids::Load
                LibBinaryen.BinaryenLoadGetAlign(@ref)
            when Ids::Store
                LibBinaryen.BinaryenStoreGetAlign(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def ptr : Expression
            Expression.new case id
            when Ids::Load
                LibBinaryen.BinaryenLoadGetPtr(@ref)
            when Ids::Store
                LibBinaryen.BinaryenStoreGetPtr(@ref)
            when Ids::AtomicRMW
                LibBinaryen.BinaryenAtomicRMWGetPtr(@ref)
            when Ids::AtomicCmpxchg
                LibBinaryen.BinaryenAtomicCmpxchgGetPtr(@ref)
            when Ids::AtomicWait
                LibBinaryen.BinaryenAtomicWaitGetPtr(@ref)
            when Ids::AtomicWake
                LibBinaryen.BinaryenAtomicWakeGetPtr(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def expected : Expression
            Expression.new case id
            when Ids::AtomicCmpxchg
                LibBinaryen.BinaryenAtomicCmpxchgGetExpected(@ref)
            when Ids::AtomicWait
                LibBinaryen.BinaryenAtomicWaitGetExpected(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def replacement : Expression
            Expression.new case id
            when Ids::AtomicCmpxchg
                LibBinaryen.BinaryenAtomicCmpxchgGetReplacement(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def expected_type : Type
            case id
            when Ids::AtomicWait
                LibBinaryen.BinaryenAtomicWaitGetExpectedType(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def wake_count : Expression
            Expression.new case id
            when Ids::AtomicWait
                LibBinaryen.BinaryenAtomicWakeGetWakeCount(@ref)
            else
                raise "bad Expression Id"
            end
        end
        def timeout : Expression
            Expression.new case id
            when Ids::AtomicWait
                LibBinaryen.BinaryenAtomicWaitGetTimeout(@ref)
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
#        def ref : LibBinaryen::BinaryenExportRef
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
        def initialize(@ftref : LibBinaryen::BinaryenFunctionTypeRef,
                      @modl : Module)
        end
        def to_unsafe
            @ftref
        end
        getter ftref : LibBinaryen::BinaryenFunctionTypeRef, modl : Module
        def remove
            @modl.remove_function_type name
        end
        def name
            String.new LibBinaryen.BinaryenFunctionTypeGetName @ftref
        end
        struct Params
            include Indexable(String)
            def initialize(@function_type : FunctionType)
            end
            getter function_type
            def size
                LibBinaryen.BinaryenFunctionTypeGetNumParams @function_type
            end
            def unsafe_at(i : Int)
                String.new LibBinaryen.BinaryenFunctionTypeGetParam @functionType, i.to_u32
            end
        end
        def params
            Params.new self
        end
        def result : Type
            LibBinaryen.BinaryenFunctionTypeGetResult @ftref
        end
    end
    struct Function
        def initialize(@ref : LibBinaryen::BinaryenFunctionRef)
        end
        getter ref : LibBinaryen::BinaryenFunctionRef
        def to_unsafe
            @ref
        end
        struct Params
            def initialize(@f : Function)
            end
            getter f : Function
            include Indexable(Type)
            def size : Int
                LibBinaryen.BinaryenFunctionGetNumParams(f)
            end
            def unsafe_at(i : Int) : Type
                LibBinaryen.BinaryenFunctionGetParam(f, i)
            end
        end
        struct Vars
            def initialize(@f : Function)
            end
            getter f : Function
            include Indexable(Type)
            def size : Int
                LibBinaryen.BinaryenFunctionGetNumVars(f)
            end
            def unsafe_at(i : Int) : Type
                LibBinaryen.BinaryenFunctionGetVar(f, i)
            end
        end
        def params : Params
            Params.new self
        end
        def vars : Vars
            Vars.new self
        end
        def result_type : Type
            LibBinaryen.BinaryenFunctionGetResult(@ref)
        end
        def body : Expression
            Expression.new LibBinaryen.BinaryenFunctionGetBody(@ref)
        end
        def name : String
            String.new LibBinaryen.BinaryenFunctionGetName(@ref)
        end
        def type : String
            String.new LibBinaryen.BinaryenFunctionGetType(@ref)
        end
    end
    struct Import
        def initialize(@ref : LibBinaryen::BinaryenImportRef, @modl : Module)
        end
        getter ref : LibBinaryen::BinaryenImportRef, modl : Module
        def to_unsafe : LibBinaryen::BinaryenImportRef
            @ref
        end
        def kind : ExternalKind
            LibBinaryen.BinaryenImportGetKind(@ref)
        end
        def module : String
            String.new LibBinaryen.BinaryenImportGetModule(@ref)
        end
        def name : String
            String.new LibBinaryen.BinaryenImportGetName(@ref)
        end
        def internal_name : String
            name
        end
        def function_type : FunctionType
            @modl.get_function_type String.new LibBinaryen.BinaryenImportGetFunctionType(@ref)
        end
        def result_type : Type
            function_type.result
        end
        def base : String
            String.new LibBinaryen.BinaryenImportGetBase(@ref)
        end
    end
    struct Export
        def initialize(@ref : LibBinaryen::BinaryenExportRef)
        end
        getter ref : LibBinaryen::BinaryenExportRef
        def to_unsafe : LibBinaryen::BinaryenExportRef
            @ref
        end
        def internal_name : String
            String.new LibBinaryen.BinaryenExportGetName @ref
        end
        def export_name : String
            String.new LibBinaryen.BinaryenExportGetValue @ref
        end
        def kind : ExternalKind
            LibBinaryen.BinaryenExportGetKind @ref
        end
    end
    struct FunctionTable
        def initialize(@name : String = "_default")
        end
        getter name : String, functions : Array(Function) = [] of Function
    end
    module Tools
        def self.to_literial(val : Int | Float)
            case val
            when Int64, UInt64
                LibBinaryen.BinaryenLiteralInt64(val.to_i64)
            when Int32, UInt32
                LibBinaryen.BinaryenLiteralInt32(val.to_i32)
            when Float64
                LibBinaryen.BinaryenLiteralFloat32(val)
            when Float32
                LibBinaryen.BinaryenLiteralFloat64(val)
            else
                raise "Bad type : #{typeof(val)}"
            end
        end
        def self.to_literial(val : Int | Float, type : Type)
            case type
            when Types::Int32
                LibBinaryen.BinaryenLiteralInt32 val.to_i32
            when Types::Int64
                LibBinaryen.BinaryenLiteralInt64 val.to_i64
            when Types::Float32
                LibBinaryen.BinaryenLiteralFloat32 val.to_f32
            when Types::Float64
                LibBinaryen.BinaryenLiteralFloat64 val.to_f64
            else
                raise ArgumentError.new "bad type, must indicate a entity type"
            end
        end
    end
    struct Relooper
        def initialize
            @ref = LibBinaryen::RelooperCreate
        end
        getter ref : LibBinaryen::RelooperRef
        def to_unsafe
            @ref
        end
        struct Block
            def initialize(@ref)
            end
            getter ref : LibBinaryen::RelooperBlockRef
            def branch(to : Block, condition : Expression, code : Expression) : Void
                LibBinaryen.RelooperAddBranch @ref, to, condition, code
            end
            def branch_for_switch(to : Block, 
                                  indexes : Array(UInt32), code : Expression) : Void
                LibBinaryen.RelooperAddBranchForSwitch @ref, to, indexes, indexes.size.to_u32, code
            end
        end
        def add_block(code : Expression) : Block
            Block.new LibBinaryen.RelooperAddBlock @ref, code
        end
        def add_block_with_switch(code : Expression, condition : Expression) : Block
            Block.new LibBinaryen.RelooperAddBlockWithSwitch @ref, code, condition
        end
    end

    class Codes
        def initialize(@data)
        end
        getter data : Bytes
        def finalize
            LibC.free @data
        end
        forward_missing_to @data
    end
    class Module
        def initialize(@modl : LibBinaryen::BinaryenModuleRef = Pointer(Void).null)
            @modl ||= LibBinaryen.BinaryenModuleCreate
            @id = Atomic(UInt32).new(1_u32)
        end
        getter modl : LibBinaryen::BinaryenModuleRef
        # the default table
        getter table : FunctionTable = FunctionTable.new
        @function_types : Hash(String, FunctionType) = {} of String => FunctionType
        def finalize()
            LibBinaryen.BinaryenModuleDispose(@modl)
        end
        def to_unsafe
            @modl
        end

        def add_function_type(name : String | Nil, result : Type, 
                params : Array(Type)) : FunctionType
            @function_types[name] ||= begin
                name ||= "_xrystal_ftype_#{@id.add 1_u32}"
                f = LibBinaryen.BinaryenAddFunctionType(@modl, name,
                            result, params, params.size.to_u32)
                FunctionType.new f, self
                                      end
        end
        def remove_function_type(name : String) : Void
            return unless @function_types.delete name
            LibBinaryen.BinaryenRemoveFunctionType(@modl, name)
        end
        def remove(ft : FunctionType) : Void
            remove_function_type ft.name
        end
        def get_function_type(name : String)
            @function_types[name]
        end
        def get_function_type(result : Type, params : Array(Type))
            FunctionType.new LibBinaryen.BinaryenGetFunctionTypeBySignature(@modl, result, params, params.size.to_u32), self
        end
        def add_function(name : String, type : FunctionType, var_types : Array(Type), 
                        body : Expression)
            Function.new LibBinaryen.BinaryenAddFunction(@modl, name,
                        type, var_types, var_types.size.to_u32, body)
        end
        def get_function(name : String)
            Function.new LibBinaryen.BinaryenGetFunction(name)
        end
        def remove_function(name : String) : Void
            LibBinaryen.BinaryenRemoveFunction @modl, name
        end
        def remove(func : Function)
            remove_function func.name
        end
        def add_function_import(internal_name : String, external_module : String, 
                                external_base : String, type : FunctionType) : Import
            Import.new LibBinaryen.BinaryenAddFunctionImport(@modl, internal_name, external_module,
                        external_base, type), self
        end
        ## not support in MVP currently.
        #def add_tabel_import(internal_name : String, external_module : String,
        #                external_base : String) : Import
        #    Import.new LibBinaryen.BinaryenAddTableImport(@modl, internal_name, external_module,
        #                                                  external_base), @modl
        #end
        ## not support in MVP currently.
        #def add_memory_import(internal_name : String, external_module : String,
        #                external_base : String) : Import
        #    Import.new LibBinaryen.BinaryenAddMemoryImport(@modl, internal_name, external_module,
        #                                                  external_base), @modl
        #end
        def add_global_import(internal_name : String, external_module : String,
                        external_base : String, type : Type) : Import
            Import.new LibBinaryen.BinaryenAddGlobalImport(@modl, internal_name, external_module,
                                                          external_base, type), @modl
        end
        def remove_import(name : String)
            LibBinaryen.BinaryenRemoveImport(@modl, name);
        end
        def remove(import : Import)
            remove_import(import.name)
        end
        def add_function_export(internal_name : String, external_name : String) : Export
            Export.new LibBinaryen.BinaryenAddFunctionExport @modl, internal_name, external_name
        end
        def add_memory_export(internal_name : String, external_name : String) : Export
            Export.new LibBinaryen.BinaryenAddMemoryExport @modl, internal_name, external_name
        end
        def add_table_export(internal_name : String, external_name : String) : Export
            Export.new LibBinaryen.BinaryenAddTableExport @modl, internal_name, external_name
        end
        def add_global_export(internal_name : String, external_name : String) : Export
            Export.new LibBinaryen.BinaryenAddGlobalExport @modl, internal_name, external_name
        end
        def add_export(func : Function, extern_name : String) : Export
            Export.new LibBinaryen.BinaryenAddFunctionExport @modl, func.name, extern_name
        end
        def remove_export(external_name : String)
            LibBinaryen.BinaryenRemoveExport external_name
        end
        def remove(export : Export)
            remove_export export.external_name
        end
        def add_global(name : String, type : Type, mutable : Bool, init : Expression)
            LibBinaryen.BinaryenAddGlobal @modl, name, type, (mutable ? 0_u8 : 1_u8), init
            name
        end

        class MemorySettingSegment 
            property data : Bytes, offset : Expression
            def initialize(@data, @offset)
            end
        end
        record MemorySetting, initial : Int32 = 1, maximum : Int32 = 1, 
            segments : Array(MemorySettingSegment) = [] of MemorySettingSegment, 
            export_name : String = "default"
        getter memory_setting : MemorySetting = MemorySetting.new
        property start_point : Function?

        def compiled?
            @code
        end
        def code : Codes?
            @code || compile
        end
        getter source_map : Codes?
        def compile(*, src_map_url : String? = nil, optimize = true)
            raise "The module has already been compiled" if @code
            if sp = start_point
                LibBinaryen.BinaryenSetStart @modl, sp
            end
            LibBinaryen.BinaryenSetFunctionTable @modl, @table.functions.map(&.to_unsafe), 
                @table.functions.size.to_u32
            LibBinaryen.BinaryenSetMemory @modl, memory_setting.initial, memory_setting.maximum,
                memory_setting.export_name, memory_setting.segments.map(&.data.to_unsafe),
                memory_setting.segments.map(&.offset.to_unsafe),
                memory_setting.segments.map(&.data.bytesize.to_u32),
                memory_setting.segments.size.to_u32
            auto_drop
            optimize if optimize
            ret = LibBinaryen.BinaryenModuleAllocateAndWrite @modl, (src_map_url || Pointer(UInt8).null)
            @sourceMap = Codes.new(Bytes.new ret.sourceMap, LibC.strlen(ret.sourceMap), read_only: true) if ret.sourceMap
            @code = Codes.new(Bytes.new ret.binary.as(Pointer(UInt8)), ret.binaryBytes, read_only: true)
        end
        def write(src_map_url : String? = nil)
            compile src_map_url
        end
        def self.decompile(code : Bytes)
            Module.new LibBinaryen.BinaryenModuleRead code, code.bytesize
        end
        def self.read(code : Bytes)
            decompile
        end
        def interpret
            LibBinaryen.BinaryenModuleInterpret @modl
        end

        def render_and_delete(relooper : Relooper, entry : Relooper::Block, labelHelper : Int) : Expression
            Expression.new LibBinaryen.RelooperRenderAndDispose relooper, entry, labelHelper, @modl
        end

        struct DebugInfo
            def initialize(@modl : Module)
            end
            getter modl
            struct Files
                def initialize(@modl : Module)
                end
                getter modl
                alias ID = LibBinaryen::BinaryenIndex
                def add(file : String) : ID
                    LibBinaryen.BinaryenModuleAddDebugInfoFileName @modl, file
                end
                def <<(file : String)
                    push file
                end
                def add(*files : String)
                    files.each {|a| push a }
                end
                def [](i : Int)
                    String.new LibBinaryen.BinaryenModuleGetDebugInfoFileName @modl, i.to_u32
                end
            end
            def files
                Files.new @modl
            end
            def set(f : Function, expr : Expression, file : Files::ID, line : Int, column : Int)
                LibBinaryen.BinaryenFunctionSetDebugLocation f, expr, file, line. column
            end
        end
        def debug
            DebugInfo.new self
        end

        def exp_block(name : String?, children : Array(Expression), 
                  type : Type = Types::Undefined) : Expression
            name = name.nil? ? name : Pointer(UInt8).null
            Expression.new LibBinaryen.BinaryenBlock(@modl, name, children.map(&.to_unsafe), 
                                       children.size.to_u32, type)
        end
        struct BlockBuilder
            getter block = [] of Expression
            getter modl : Module
            def initialize(@modl)
            end
            macro method_missing(call)
              @block << @modl.{{call}}
            end
            def <<(o)
                @block << o
            end
        end
        def exp_block(name : String? = nil, type = Types::Undefined, &block) : Expression
            builder = BlockBuilder.new self
            yield builder
            exp_block name, builder.block, type
        end
        def exp_if(condition : Expression, on_true : Expression? = nil, 
               on_fail : Expression? = nil) : Expression
            on_fail ||= Expression::NULL
            on_true ||= exp_nop
            Expression.new LibBinaryen.BinaryenIf(@modl, condition, on_true, on_fail)
        end
        def exp_loop(name : String | Nil, body : Expression) : Expression
            name = name.nil? ? name : Pointer(UInt8).null
            Expression.new LibBinaryen.BinaryenLoop(@modl, name, body)
        end
        def exp_break(name : String | Nil = nil, condition : Expression | Nil = nil,
                  value : Expression | Nil = nil) : Expression
            name = name.nil? ? name : Pointer(UInt8).null
            condition ||= Expression::NULL
            value ||= Expression::NULL
            Expression.new LibBinaryen.BinaryenBreak(@modl, name, condition, value)
        end
        def exp_switch(names : Array(String), default_name : String, 
                  condition : Expression | Nil = nil, value : Expression | Nil = nil) : Expression
            ps = names.map &.to_unsafe
            condition ||= Expression::NULL
            value ||= Expression::NULL
            Expression.new LibBinaryen.BinaryenSwitch(@modl, ps, names.size.to_u32, 
                        default_name, condition, value)
        end
        def exp_call(target : String, operands : Array(Expression), 
                     return_type : Type) : Expression
            Expression.new LibBinaryen.BinaryenCall(@modl, target, 
                        operands.map(&.to_unsafe), operands.size.to_u32, return_type)
        end
        def exp_call_import(target : String, operands : Array(Expression), 
                     return_type : Type) : Expression
            Expression.new LibBinaryen.BinaryenCallImport(@modl, target, 
                        operands.map(&.to_unsafe), operands.size.to_u32, return_type)
        end
        def exp_call_indirect(target : Expression, operands : Array(Expression), 
                             type : FunctionType) : Expression
            Expression.new LibBinaryen.BinaryenCallIndirect(@modl, target, operands,
                                              operands.size.to_u32, type.name)
        end
        def exp_call(func : Function | Import, operands : Array(Expression), ret : Type) : Expression
            if func.is_a? Function
                exp_call func.name, operands, ret
            else
                exp_call_import func.name, operands, ret
            end
        end
        def exp_call(func : Function | Import, operands : Array(Expression)) : Expression
            if func.is_a? Function
                exp_call func.name, operands, func.result_type
            else
                exp_call_import func.name, operands, func.result_type
            end
        end
        # TODO : function builder
        def exp_get_local(index : Int, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenGetLocal(@modl, index, type)
        end
        def exp_set_local(index : Int, val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenSetLocal(@modl, index, val)
        end
        def exp_tee_local(index : Int, val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenTeeLocal(@modl, index, val)
        end
        def exp_get_global(name : String, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenGetGlobal(@modl, name, type)
        end
        def exp_set_global(name : String, val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenSetGlobal(@modl, name, val)
        end
        def exp_load(bytes : Int, signed : Bool, offset : Int, align : Int,
                    type : Type, ptr : Expression) : Expression
            signed = (signed ? 1_u32 : 0_u32)
            Expression.new LibBinaryen.BinaryenLoad(@modl, bytes.to_u32, signed,
                    offset.to_u32, align.to_u32, type, ptr)
        end
        def exp_load(offset : Int, type : ::Class, ptr : Expression, align : Int = 0)
            signed = ( type < Int::Signed ? 1_u32 : 0_u32 )
            Expression.new LibBinaryen.BinaryenLoad(@modl, type.size.to_u32, signed,
                    offset.to_u32, align.to_u32, Types::ClassInWASM[type], ptr)
        end
        def exp_store(bytes : Int, offset : Int, align : Int, ptr : Expression, 
                     val : Expression, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenStore(@modl, bytes.to_u32, offset.to_u32, align.to_u32,
                        ptr, val, type)
        end
        def exp_const(val : Int | Float) : Expression
            Expression.new LibBinaryen.BinaryenConst(@modl, Tools.to_literial(val))
        end
        def exp_const(val : Int | Float, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenConst @modl, Tools.to_literial val, type
        end
        def exp(op : Operation, val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenUnary(@modl, op, val)
        end
        def exp(op : Operation, v1 : Expression, v2 : Expression) : Expression
            Expression.new LibBinaryen.BinaryenBinary(@modl, op, v1, v2)
        end
        def exp_select(condition : Expression, if_true : Expression,
                      if_false : Expression) : Expression
            Expression.new LibBinaryen.BinaryenSelect(@modl, condition, if_true, if_false)
        end
        def exp_drop(val : Expression) : Expression
            Expression.new LibBinaryen.BinaryenDrop(@modl, val)
        end
        def exp_return(val : Expression | Nil = nil) : Expression
            val = Expression::NULL unless val
            Expression.new LibBinaryen.BinaryenReturn(@modl, val)
        end
        def exp_host(op : Operation, name : String, 
                     operands : Array(Expression)) : Expression
            Expression.new LibBinaryen.BinaryenHost(@modl, op, name, operands,
                                      operands.size.to_u32)
        end
        def exp_nop() : Expression
            Expression.new LibBinaryen.BinaryenNop(@modl)
        end
        def exp_unreachable() : Expression
            Expression.new LibBinaryen.BinaryenUnreachable(@modl)
        end
        def exp_atomic_load(bytes : Int, offset : Int, type : Type, ptr : Expression) : Expression
            Expression.new LibBinaryen.BinaryenAtomicLoad(bytes.to_u32, offset.to_u32, type, ptr)
        end
        def exp_atomic_store(bytes : Int, offset : Int, ptr : Expression, 
                             val : Expression, type : Type) : Expression
            Expression.new LibBinaryen.BinaryenAtomicStore(bytes.to_u32, offset.to_u32, ptr, val, type)
        end
        def exp_atomic(op : Operation, offset : Int, ptr : Expression, val : Expression, 
                       type : BinaryenType) : Expression
            bytes = Types.size_of type
            Expression.new LibBinaryen.BinaryenAtomicRMW(@modl, op, bytes, offset.to_u32, ptr, val, type)
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

        def self.parse(s_exp : String) : Module
            Module.new LibBinaryen.BinaryenModuleParse s_exp
        end
        def print
            LibBinaryen.BinaryenModulePrint @modl
        end
        def print_asmjs
            LibBinaryen.BinaryenModulePrintAsmjs @modl
        end
        def valid : Bool
            LibBinaryen.BinaryenModuleValidate(@modl) != 0
        end
        def optimize
            LibBinaryen.BinaryenModuleOptimize @modl
        end
        def optimize(f : Function)
            LibBinaryen.BinaryenFunctionOptimize f, @modl
        end
        def run_passes(passes : Array(String))
            LibBinaryen.BinaryenModuleRunPasses @modl, passes.map(&.to_unsafe), passes.size
        end
        def run_passes(passes : Array(String), f : Function)
            LibBinaryen.BinaryenFunctionRunPasses f, @modl, passes(&.to_unsafe), passes.size
        end
        def auto_drop
            LibBinaryen.BinaryenModuleAutoDrop @modl
        end
    end
    def self.optimize_level : Int
        LibBinaryen.BinaryenGetOptimizeLevel
    end
    def self.optimize_level=(v : Int) : Int
        LibBinaryen.BinaryenSetOptimizeLevel v
        v
    end
    def self.shrink_level : Int
        LibBinaryen.BinaryenGetShrinkLevel
    end
    def self.shrink_level=(v : Int) : Int
        LibBinaryen.BinaryenSetShrinkLevel v
        v
    end
    def self.debug_info : Bool
        LibBinaryen.BinaryenGetDebugInfo
    end
    def self.debug_info=(v : Bool) : Bool
        LibBinaryen.BinaryenSetDebugInfo v
        v
    end
    def self.api_tracing=(on : Bool)
        LibBinaryen.BinaryenSetAPITracing (on ? 1 : 0)
    end
end

