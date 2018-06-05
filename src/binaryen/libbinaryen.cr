@[Link("binaryen")]
lib LibBinaryen
    alias BinaryenType = UInt32
    alias BinaryenIndex = UInt32
    alias BinaryenExpressionId = UInt32
    alias BinaryenExternalKind = UInt32
    alias BinaryenOp = Int32
    alias BinaryenFunctionTypeRef = Pointer(Void)
    alias BinaryenExpressionRef = Pointer(Void)
    alias BinaryenFunctionRef = Pointer(Void)
    alias BinaryenImportRef = Pointer(Void)
    alias BinaryenModuleRef = Pointer(Void)
    union BinaryenLiteralUnion
        i32 : Int32
        i64 : Int64
        f32 : Float32
        f64 : Float64
    end
    struct BinaryenLiteral
      type : Int32
      v : BinaryenLiteralUnion
    end

    fun BinaryenTypeNone() : BinaryenType
    fun BinaryenTypeInt32() : BinaryenType
    fun BinaryenTypeInt64() : BinaryenType
    fun BinaryenTypeFloat32() : BinaryenType
    fun BinaryenTypeFloat64() : BinaryenType
    fun BinaryenTypeUnreachable() : BinaryenType
    fun BinaryenTypeAuto() : BinaryenType
    fun BinaryenRemoveFunctionType(modl : BinaryenModuleRef, name : Pointer(UInt8))
    fun BinaryenInvalidId() : BinaryenExpressionId
    fun BinaryenBlockId() : BinaryenExpressionId
    fun BinaryenIfId() : BinaryenExpressionId
    fun BinaryenLoopId() : BinaryenExpressionId
    fun BinaryenBreakId() : BinaryenExpressionId
    fun BinaryenSwitchId() : BinaryenExpressionId
    fun BinaryenCallId() : BinaryenExpressionId
    fun BinaryenCallImportId() : BinaryenExpressionId
    fun BinaryenCallIndirectId() : BinaryenExpressionId
    fun BinaryenGetLocalId() : BinaryenExpressionId
    fun BinaryenSetLocalId() : BinaryenExpressionId
    fun BinaryenGetGlobalId() : BinaryenExpressionId
    fun BinaryenSetGlobalId() : BinaryenExpressionId
    fun BinaryenLoadId() : BinaryenExpressionId
    fun BinaryenStoreId() : BinaryenExpressionId
    fun BinaryenConstId() : BinaryenExpressionId
    fun BinaryenUnaryId() : BinaryenExpressionId
    fun BinaryenBinaryId() : BinaryenExpressionId
    fun BinaryenSelectId() : BinaryenExpressionId
    fun BinaryenDropId() : BinaryenExpressionId
    fun BinaryenReturnId() : BinaryenExpressionId
    fun BinaryenHostId() : BinaryenExpressionId
    fun BinaryenNopId() : BinaryenExpressionId
    fun BinaryenUnreachableId() : BinaryenExpressionId
    fun BinaryenAtomicCmpxchgId() : BinaryenExpressionId
    fun BinaryenAtomicRMWId() : BinaryenExpressionId
    fun BinaryenAtomicWaitId() : BinaryenExpressionId
    fun BinaryenAtomicWakeId() : BinaryenExpressionId
    fun BinaryenExternalFunction() : BinaryenExternalKind
    fun BinaryenExternalTable() : BinaryenExternalKind
    fun BinaryenExternalMemory() : BinaryenExternalKind
    fun BinaryenExternalGlobal() : BinaryenExternalKind
    fun BinaryenModuleCreate() : BinaryenModuleRef
    fun BinaryenModuleDispose(modl : BinaryenModuleRef)
    fun BinaryenAddFunctionType(modl : BinaryenModuleRef, name : Pointer(UInt8), result : BinaryenType, paramTypes : Pointer(BinaryenType), numParams : BinaryenIndex) : BinaryenFunctionTypeRef
    fun BinaryenLiteralInt32(x : Int32) : BinaryenLiteral
    fun BinaryenLiteralInt64(x : Int64) : BinaryenLiteral
    fun BinaryenLiteralFloat32(x : Float32) : BinaryenLiteral
    fun BinaryenLiteralFloat64(x : Float64) : BinaryenLiteral
    fun BinaryenLiteralFloat32Bits(x : Int32) : BinaryenLiteral
    fun BinaryenLiteralFloat64Bits(x : Int64) : BinaryenLiteral
    fun BinaryenClzInt32() : BinaryenOp
    fun BinaryenCtzInt32() : BinaryenOp
    fun BinaryenPopcntInt32() : BinaryenOp
    fun BinaryenNegFloat32() : BinaryenOp
    fun BinaryenAbsFloat32() : BinaryenOp
    fun BinaryenCeilFloat32() : BinaryenOp
    fun BinaryenFloorFloat32() : BinaryenOp
    fun BinaryenTruncFloat32() : BinaryenOp
    fun BinaryenNearestFloat32() : BinaryenOp
    fun BinaryenSqrtFloat32() : BinaryenOp
    fun BinaryenEqZInt32() : BinaryenOp
    fun BinaryenClzInt64() : BinaryenOp
    fun BinaryenCtzInt64() : BinaryenOp
    fun BinaryenPopcntInt64() : BinaryenOp
    fun BinaryenNegFloat64() : BinaryenOp
    fun BinaryenAbsFloat64() : BinaryenOp
    fun BinaryenCeilFloat64() : BinaryenOp
    fun BinaryenFloorFloat64() : BinaryenOp
    fun BinaryenTruncFloat64() : BinaryenOp
    fun BinaryenNearestFloat64() : BinaryenOp
    fun BinaryenSqrtFloat64() : BinaryenOp
    fun BinaryenEqZInt64() : BinaryenOp
    fun BinaryenExtendSInt32() : BinaryenOp
    fun BinaryenExtendUInt32() : BinaryenOp
    fun BinaryenWrapInt64() : BinaryenOp
    fun BinaryenTruncSFloat32ToInt32() : BinaryenOp
    fun BinaryenTruncSFloat32ToInt64() : BinaryenOp
    fun BinaryenTruncUFloat32ToInt32() : BinaryenOp
    fun BinaryenTruncUFloat32ToInt64() : BinaryenOp
    fun BinaryenTruncSFloat64ToInt32() : BinaryenOp
    fun BinaryenTruncSFloat64ToInt64() : BinaryenOp
    fun BinaryenTruncUFloat64ToInt32() : BinaryenOp
    fun BinaryenTruncUFloat64ToInt64() : BinaryenOp
    fun BinaryenReinterpretFloat32() : BinaryenOp
    fun BinaryenReinterpretFloat64() : BinaryenOp
    fun BinaryenConvertSInt32ToFloat32() : BinaryenOp
    fun BinaryenConvertSInt32ToFloat64() : BinaryenOp
    fun BinaryenConvertUInt32ToFloat32() : BinaryenOp
    fun BinaryenConvertUInt32ToFloat64() : BinaryenOp
    fun BinaryenConvertSInt64ToFloat32() : BinaryenOp
    fun BinaryenConvertSInt64ToFloat64() : BinaryenOp
    fun BinaryenConvertUInt64ToFloat32() : BinaryenOp
    fun BinaryenConvertUInt64ToFloat64() : BinaryenOp
    fun BinaryenPromoteFloat32() : BinaryenOp
    fun BinaryenDemoteFloat64() : BinaryenOp
    fun BinaryenReinterpretInt32() : BinaryenOp
    fun BinaryenReinterpretInt64() : BinaryenOp
    fun BinaryenExtendS8Int32() : BinaryenOp
    fun BinaryenExtendS16Int32() : BinaryenOp
    fun BinaryenExtendS8Int64() : BinaryenOp
    fun BinaryenExtendS16Int64() : BinaryenOp
    fun BinaryenExtendS32Int64() : BinaryenOp
    fun BinaryenAddInt32() : BinaryenOp
    fun BinaryenSubInt32() : BinaryenOp
    fun BinaryenMulInt32() : BinaryenOp
    fun BinaryenDivSInt32() : BinaryenOp
    fun BinaryenDivUInt32() : BinaryenOp
    fun BinaryenRemSInt32() : BinaryenOp
    fun BinaryenRemUInt32() : BinaryenOp
    fun BinaryenAndInt32() : BinaryenOp
    fun BinaryenOrInt32() : BinaryenOp
    fun BinaryenXorInt32() : BinaryenOp
    fun BinaryenShlInt32() : BinaryenOp
    fun BinaryenShrUInt32() : BinaryenOp
    fun BinaryenShrSInt32() : BinaryenOp
    fun BinaryenRotLInt32() : BinaryenOp
    fun BinaryenRotRInt32() : BinaryenOp
    fun BinaryenEqInt32() : BinaryenOp
    fun BinaryenNeInt32() : BinaryenOp
    fun BinaryenLtSInt32() : BinaryenOp
    fun BinaryenLtUInt32() : BinaryenOp
    fun BinaryenLeSInt32() : BinaryenOp
    fun BinaryenLeUInt32() : BinaryenOp
    fun BinaryenGtSInt32() : BinaryenOp
    fun BinaryenGtUInt32() : BinaryenOp
    fun BinaryenGeSInt32() : BinaryenOp
    fun BinaryenGeUInt32() : BinaryenOp
    fun BinaryenAddInt64() : BinaryenOp
    fun BinaryenSubInt64() : BinaryenOp
    fun BinaryenMulInt64() : BinaryenOp
    fun BinaryenDivSInt64() : BinaryenOp
    fun BinaryenDivUInt64() : BinaryenOp
    fun BinaryenRemSInt64() : BinaryenOp
    fun BinaryenRemUInt64() : BinaryenOp
    fun BinaryenAndInt64() : BinaryenOp
    fun BinaryenOrInt64() : BinaryenOp
    fun BinaryenXorInt64() : BinaryenOp
    fun BinaryenShlInt64() : BinaryenOp
    fun BinaryenShrUInt64() : BinaryenOp
    fun BinaryenShrSInt64() : BinaryenOp
    fun BinaryenRotLInt64() : BinaryenOp
    fun BinaryenRotRInt64() : BinaryenOp
    fun BinaryenEqInt64() : BinaryenOp
    fun BinaryenNeInt64() : BinaryenOp
    fun BinaryenLtSInt64() : BinaryenOp
    fun BinaryenLtUInt64() : BinaryenOp
    fun BinaryenLeSInt64() : BinaryenOp
    fun BinaryenLeUInt64() : BinaryenOp
    fun BinaryenGtSInt64() : BinaryenOp
    fun BinaryenGtUInt64() : BinaryenOp
    fun BinaryenGeSInt64() : BinaryenOp
    fun BinaryenGeUInt64() : BinaryenOp
    fun BinaryenAddFloat32() : BinaryenOp
    fun BinaryenSubFloat32() : BinaryenOp
    fun BinaryenMulFloat32() : BinaryenOp
    fun BinaryenDivFloat32() : BinaryenOp
    fun BinaryenCopySignFloat32() : BinaryenOp
    fun BinaryenMinFloat32() : BinaryenOp
    fun BinaryenMaxFloat32() : BinaryenOp
    fun BinaryenEqFloat32() : BinaryenOp
    fun BinaryenNeFloat32() : BinaryenOp
    fun BinaryenLtFloat32() : BinaryenOp
    fun BinaryenLeFloat32() : BinaryenOp
    fun BinaryenGtFloat32() : BinaryenOp
    fun BinaryenGeFloat32() : BinaryenOp
    fun BinaryenAddFloat64() : BinaryenOp
    fun BinaryenSubFloat64() : BinaryenOp
    fun BinaryenMulFloat64() : BinaryenOp
    fun BinaryenDivFloat64() : BinaryenOp
    fun BinaryenCopySignFloat64() : BinaryenOp
    fun BinaryenMinFloat64() : BinaryenOp
    fun BinaryenMaxFloat64() : BinaryenOp
    fun BinaryenEqFloat64() : BinaryenOp
    fun BinaryenNeFloat64() : BinaryenOp
    fun BinaryenLtFloat64() : BinaryenOp
    fun BinaryenLeFloat64() : BinaryenOp
    fun BinaryenGtFloat64() : BinaryenOp
    fun BinaryenGeFloat64() : BinaryenOp
    fun BinaryenPageSize() : BinaryenOp
    fun BinaryenCurrentMemory() : BinaryenOp
    fun BinaryenGrowMemory() : BinaryenOp
    fun BinaryenHasFeature() : BinaryenOp
    fun BinaryenAtomicRMWAdd() : BinaryenOp
    fun BinaryenAtomicRMWSub() : BinaryenOp
    fun BinaryenAtomicRMWAnd() : BinaryenOp
    fun BinaryenAtomicRMWOr() : BinaryenOp
    fun BinaryenAtomicRMWXor() : BinaryenOp
    fun BinaryenAtomicRMWXchg() : BinaryenOp
    fun BinaryenBlock(modl : BinaryenModuleRef, name : Pointer(UInt8), children : Pointer(BinaryenExpressionRef), numChildren : BinaryenIndex, type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenIf(modl : BinaryenModuleRef, condition : BinaryenExpressionRef, ifTrue : BinaryenExpressionRef, ifFalse : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenLoop(modl : BinaryenModuleRef, in : Pointer(UInt8), body : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenBreak(modl : BinaryenModuleRef, name : Pointer(UInt8), condition : BinaryenExpressionRef, value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenSwitch(modl : BinaryenModuleRef, names : Pointer(UInt8), numNames : BinaryenIndex, defaultName : Pointer(UInt8), condition : BinaryenExpressionRef, value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenCall(modl : BinaryenModuleRef, target : Pointer(UInt8), operands : Pointer(BinaryenExpressionRef), numOperands : BinaryenIndex, returnType : BinaryenType) : BinaryenExpressionRef
    fun BinaryenCallImport(modl : BinaryenModuleRef, target : Pointer(UInt8), operands : Pointer(BinaryenExpressionRef), numOperands : BinaryenIndex, returnType : BinaryenType) : BinaryenExpressionRef
    fun BinaryenCallIndirect(modl : BinaryenModuleRef, target : BinaryenExpressionRef, operands : Pointer(BinaryenExpressionRef), numOperands : BinaryenIndex, type : Pointer(UInt8)) : BinaryenExpressionRef
    fun BinaryenGetLocal(modl : BinaryenModuleRef, index : BinaryenIndex, type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenSetLocal(modl : BinaryenModuleRef, index : BinaryenIndex, value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenTeeLocal(modl : BinaryenModuleRef, index : BinaryenIndex, value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenGetGlobal(modl : BinaryenModuleRef, name : Pointer(UInt8), type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenSetGlobal(modl : BinaryenModuleRef, name : Pointer(UInt8), value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenLoad(modl : BinaryenModuleRef, bytes : UInt32, signed : Int8, offest : UInt32, align : UInt32, type : BinaryenType, ptr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenStore(modl : BinaryenModuleRef, bytes : UInt32, offset : UInt32, align : UInt32, ptr : BinaryenExpressionRef, value : BinaryenExpressionRef, type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenConst(modl : BinaryenModuleRef, value : BinaryenLiteral) : BinaryenExpressionRef
    fun BinaryenUnary(modl : BinaryenModuleRef, op : BinaryenOp, value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenBinary(modl : BinaryenModuleRef, op : BinaryenOp, left : BinaryenExpressionRef, right : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenSelect(modl : BinaryenModuleRef, condition : BinaryenExpressionRef, ifTrue : BinaryenExpressionRef, ifFalse : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenDrop(modl : BinaryenModuleRef, value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenReturn(modl : BinaryenModuleRef, value : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenHost(modl : BinaryenModuleRef, op : BinaryenOp, name : Pointer(UInt8), operands : Pointer(BinaryenExpressionRef), numOperands : BinaryenIndex) : BinaryenExpressionRef
    fun BinaryenNop(modl : BinaryenModuleRef) : BinaryenExpressionRef
    fun BinaryenUnreachable(modl : BinaryenModuleRef) : BinaryenExpressionRef
    fun BinaryenAtomicLoad(modl : BinaryenModuleRef, bytes : UInt32, offset : UInt32, type : BinaryenType, ptr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenAtomicStore(modl : BinaryenModuleRef, bytes : UInt32, offset : UInt32, ptr : BinaryenExpressionRef, value : BinaryenExpressionRef, type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenAtomicRMW(modl : BinaryenModuleRef, op : BinaryenOp, bytes : BinaryenIndex, offset : BinaryenIndex, ptr : BinaryenExpressionRef, value : BinaryenExpressionRef, type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenAtomicCmpxchg(modl : BinaryenModuleRef, bytes : BinaryenIndex, offset : BinaryenIndex, ptr : BinaryenExpressionRef, expected : BinaryenExpressionRef, replacement : BinaryenExpressionRef, type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenAtomicWait(modl : BinaryenModuleRef, ptr : BinaryenExpressionRef, expected : BinaryenExpressionRef, timeout : BinaryenExpressionRef, type : BinaryenType) : BinaryenExpressionRef
    fun BinaryenAtomicWake(modl : BinaryenModuleRef, ptr : BinaryenExpressionRef, wakeCount : BinaryenExpressionRef) : BinaryenExpressionRef
    
    fun BinaryenExpressionGetId(expr : BinaryenExpressionRef) : BinaryenExpressionId
    fun BinaryenExpressionGetType(expr : BinaryenExpressionRef) : BinaryenType
    fun BinaryenExpressionPrint(expr : BinaryenExpressionRef)
    
    fun BinaryenBlockGetName(expr : BinaryenExpressionRef) : Pointer(UInt8)
    fun BinaryenBlockGetNumChildren(expr : BinaryenExpressionRef) : BinaryenIndex
    fun BinaryenBlockGetChild(expr : BinaryenExpressionRef, index : BinaryenIndex) : BinaryenExpressionRef
    
    fun BinaryenIfGetCondition(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenIfGetIfTrue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenIfGetIfFalse(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    fun BinaryenLoopGetName(expr : BinaryenExpressionRef) : Pointer(UInt8)
    fun BinaryenLoopGetBody(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    fun BinaryenBreakGetName(expr : BinaryenExpressionRef) : Pointer(UInt8)
    fun BinaryenBreakGetCondition(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenBreakGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    fun BinaryenSwitchGetNumNames(expr : BinaryenExpressionRef) : BinaryenIndex
    fun BinaryenSwitchGetName(expr : BinaryenExpressionRef, index : BinaryenIndex) : Pointer(UInt8)
    fun BinaryenSwitchGetDefaultName(expr : BinaryenExpressionRef) : Pointer(UInt8)
    fun BinaryenSwitchGetCondition(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenSwitchGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    fun BinaryenCallGetTarget(expr : BinaryenExpressionRef) : Pointer(UInt8)
    fun BinaryenCallGetNumOperands(expr : BinaryenExpressionRef) : BinaryenIndex
    fun BinaryenCallGetOperand(expr : BinaryenExpressionRef, index : BinaryenIndex) : BinaryenExpressionRef
    
    fun BinaryenCallImportGetTarget(expr : BinaryenExpressionRef) : Pointer(UInt8)
    fun BinaryenCallImportGetNumOperands(expr : BinaryenExpressionRef) : BinaryenIndex
    fun BinaryenCallImportGetOperand(expr : BinaryenExpressionRef, index : BinaryenIndex) : BinaryenExpressionRef
    
    fun BinaryenCallIndirectGetTarget(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenCallIndirectGetNumOperands(expr : BinaryenExpressionRef) : BinaryenIndex
    fun BinaryenCallIndirectGetOperand(expr : BinaryenExpressionRef, index : BinaryenIndex) : BinaryenExpressionRef
    
    fun BinaryenGetLocalGetIndex(expr : BinaryenExpressionRef) : BinaryenIndex
    
    fun BinaryenSetLocalIsTee(expr : BinaryenExpressionRef) : LibC::Int
    fun BinaryenSetLocalGetIndex(expr : BinaryenExpressionRef) : BinaryenIndex
    fun BinaryenSetLocalGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    fun BinaryenGetGlobalGetName(expr : BinaryenExpressionRef) : Pointer(UInt8)
    
    # Gets the name of the specified `SetGlobal` expression.
    fun BinaryenSetGlobalGetName(expr : BinaryenExpressionRef) : Pointer(UInt8)
    # Gets the nested value expression within the specified `SetLocal` expression.
    fun BinaryenSetGlobalGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the operator of the specified `Host` expression.
    fun BinaryenHostGetOp(expr : BinaryenExpressionRef) : BinaryenOp
    # Gets the name operand of the specified `Host` expression. May be `NULL`.
    fun BinaryenHostGetNameOperand(expr : BinaryenExpressionRef) : Pointer(UInt8)
    # Gets the number of nested operand expressions within the specified `Host` expression.
    fun BinaryenHostGetNumOperands(expr : BinaryenExpressionRef) : BinaryenIndex
    # Gets the nested operand expression at the specified index within the specified `Host` expression.
    fun BinaryenHostGetOperand(expr : BinaryenExpressionRef, index : BinaryenIndex) : BinaryenExpressionRef
    
    # Tests if the specified `Load` expression is atomic.
    fun BinaryenLoadIsAtomic(expr : BinaryenExpressionRef) : LibC::Int
    # Tests if the specified `Load` expression is signed.
    fun BinaryenLoadIsSigned(expr : BinaryenExpressionRef) : LibC::Int
    # Gets the offset of the specified `Load` expression.
    fun BinaryenLoadGetOffset(expr : BinaryenExpressionRef) : UInt32
    # Gets the byte size of the specified `Load` expression.
    fun BinaryenLoadGetBytes(expr : BinaryenExpressionRef) : UInt32
    # Gets the alignment of the specified `Load` expression.
    fun BinaryenLoadGetAlign(expr : BinaryenExpressionRef) : UInt32
    # Gets the nested pointer expression within the specified `Load` expression.
    fun BinaryenLoadGetPtr(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Tests if the specified `Store` expression is atomic.
    fun BinaryenStoreIsAtomic(expr : BinaryenExpressionRef) : LibC::Int
    # Gets the byte size of the specified `Store` expression.
    fun BinaryenStoreGetBytes(expr : BinaryenExpressionRef) : UInt32
    # Gets the offset of the specified store expression.
    fun BinaryenStoreGetOffset(expr : BinaryenExpressionRef) : UInt32
    # Gets the alignment of the specified `Store` expression.
    fun BinaryenStoreGetAlign(expr : BinaryenExpressionRef) : UInt32
    # Gets the nested pointer expression within the specified `Store` expression.
    fun BinaryenStoreGetPtr(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested value expression within the specified `Store` expression.
    fun BinaryenStoreGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the 32-bit integer value of the specified `Const` expression.
    fun BinaryenConstGetValueI32(expr : BinaryenExpressionRef) : Int32
    # Gets the 64-bit integer value of the specified `Const` expression.
    fun BinaryenConstGetValueI64(expr : BinaryenExpressionRef) : Int64
    # Gets the low 32-bits of a 64-bit integer value of the specified `Const` expression. Useful where I64 returning exports are illegal, i.e. binaryen.js.
    fun BinaryenConstGetValueI64Low(expr : BinaryenExpressionRef) : Int32
    # Gets the high 32-bits of a 64-bit integer value of the specified `Const` expression. Useful where I64 returning exports are illegal, i.e. binaryen.js.
    fun BinaryenConstGetValueI64High(expr : BinaryenExpressionRef) : Int32
    # Gets the 32-bit float value of the specified `Const` expression.
    fun BinaryenConstGetValueF32(expr : BinaryenExpressionRef) : Float32
    # Gets the 64-bit float value of the specified `Const` expression.
    fun BinaryenConstGetValueF64(expr : BinaryenExpressionRef) : Float64
    
    # Gets the operator of the specified `Unary` expression.
    fun BinaryenUnaryGetOp(expr : BinaryenExpressionRef) : BinaryenOp
    # Gets the nested value expression within the specified `Unary` expression.
    fun BinaryenUnaryGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the operator of the specified `Binary` expression.
    fun BinaryenBinaryGetOp(expr : BinaryenExpressionRef) : BinaryenOp
    # Gets the nested left expression within the specified `Binary` expression.
    fun BinaryenBinaryGetLeft(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested right expression within the specified `Binary` expression.
    fun BinaryenBinaryGetRight(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the nested ifTrue expression within the specified `Select` expression.
    fun BinaryenSelectGetIfTrue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested ifFalse expression within the specified `Select` expression.
    fun BinaryenSelectGetIfFalse(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested condition expression within the specified `Select` expression.
    fun BinaryenSelectGetCondition(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the nested value expression within the specified `Drop` expression.
    fun BinaryenDropGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the nested value expression within the specified `Return` expression.
    fun BinaryenReturnGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the operator of the specified `AtomicRMW` expression.
    fun BinaryenAtomicRMWGetOp(expr : BinaryenExpressionRef) : BinaryenOp
    # Gets the byte size of the specified `AtomicRMW` expression.
    fun BinaryenAtomicRMWGetBytes(expr : BinaryenExpressionRef) : UInt32
    # Gets the offset of the specified `AtomicRMW` expression.
    fun BinaryenAtomicRMWGetOffset(expr : BinaryenExpressionRef) : UInt32
    # Gets the nested pointer expression within the specified `AtomicRMW` expression.
    fun BinaryenAtomicRMWGetPtr(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested value expression within the specified `AtomicRMW` expression.
    fun BinaryenAtomicRMWGetValue(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the byte size of the specified `AtomicCmpxchg` expression.
    fun BinaryenAtomicCmpxchgGetBytes(expr : BinaryenExpressionRef) : UInt32
    # Gets the offset of the specified `AtomicCmpxchg` expression.
    fun BinaryenAtomicCmpxchgGetOffset(expr : BinaryenExpressionRef) : UInt32
    # Gets the nested pointer expression within the specified `AtomicCmpxchg` expression.
    fun BinaryenAtomicCmpxchgGetPtr(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested expected value expression within the specified `AtomicCmpxchg` expression.
    fun BinaryenAtomicCmpxchgGetExpected(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested replacement value expression within the specified `AtomicCmpxchg` expression.
    fun BinaryenAtomicCmpxchgGetReplacement(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    
    # Gets the nested pointer expression within the specified `AtomicWait` expression.
    fun BinaryenAtomicWaitGetPtr(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested expected value expression within the specified `AtomicWait` expression.
    fun BinaryenAtomicWaitGetExpected(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested timeout expression within the specified `AtomicWait` expression.
    fun BinaryenAtomicWaitGetTimeout(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the expected type of the specified `AtomicWait` expression.
    fun BinaryenAtomicWaitGetExpectedType(expr : BinaryenExpressionRef) : BinaryenType
    
    # Gets the nested pointer expression within the specified `AtomicWake` expression.
    fun BinaryenAtomicWakeGetPtr(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    # Gets the nested wake count expression within the specified `AtomicWake` expression.
    fun BinaryenAtomicWakeGetWakeCount(expr : BinaryenExpressionRef) : BinaryenExpressionRef
    fun BinaryenAddFunction(modl : BinaryenModuleRef, name : Pointer(UInt8), type : BinaryenFunctionTypeRef, varTypes : Pointer(BinaryenType), numVarTypes : BinaryenIndex, body : BinaryenExpressionRef) : BinaryenFunctionRef
    
    # Gets a function reference by name.
    fun BinaryenGetFunction(modl : BinaryenModuleRef, name : Pointer(UInt8)) : BinaryenFunctionRef
    
    # Removes a function by name.
    fun BinaryenRemoveFunction(modl : BinaryenModuleRef, name : Pointer(UInt8))
    
    fun BinaryenAddFunctionImport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalModuleName : Pointer(UInt8), externalBaseName : Pointer(UInt8), functionType : BinaryenFunctionTypeRef) : BinaryenImportRef
    fun BinaryenAddTableImport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalModuleName : Pointer(UInt8), externalBaseName : Pointer(UInt8)) : BinaryenImportRef
    fun BinaryenAddMemoryImport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalModuleName : Pointer(UInt8), externalBaseName : Pointer(UInt8)) : BinaryenImportRef
    fun BinaryenAddGlobalImport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalModuleName : Pointer(UInt8), externalBaseName : Pointer(UInt8), globalType : BinaryenType) : BinaryenImportRef
    fun BinaryenRemoveImport(modl : BinaryenModuleRef, internalName : Pointer(UInt8))
    
    # Exports
    
    alias BinaryenExportRef = Pointer(Void)
    
    fun BinaryenAddFunctionExport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalName : Pointer(UInt8)) : BinaryenExportRef
    fun BinaryenAddTableExport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalName : Pointer(UInt8)) : BinaryenExportRef
    fun BinaryenAddMemoryExport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalName : Pointer(UInt8)) : BinaryenExportRef
    fun BinaryenAddGlobalExport(modl : BinaryenModuleRef, internalName : Pointer(UInt8), externalName : Pointer(UInt8)) : BinaryenExportRef
    fun BinaryenRemoveExport(modl : BinaryenModuleRef, externalName : Pointer(UInt8))
    
    # Globals
    
    alias BinaryenGlobalRef = Pointer(Void)
    
    fun BinaryenAddGlobal(modl : BinaryenModuleRef, name : Pointer(UInt8), type : BinaryenType, mutable : Int8, init : BinaryenExpressionRef) : BinaryenGlobalRef
    
    # Function table. One per module
    
    fun BinaryenSetFunctionTable(modl : BinaryenModuleRef, funcs : Pointer(BinaryenFunctionRef), numFuncs : BinaryenIndex)
    
    # Memory. One per module
    
    # Each segment has data in segments, a start offset in segmentOffsets, and a size in segmentSizes.
    # exportName can be NULL
    fun BinaryenSetMemory(modl : BinaryenModuleRef, initial : BinaryenIndex, maximum : BinaryenIndex, exportName : Pointer(UInt8), segments : Pointer(Pointer(UInt8)), segmentOffsets : Pointer(BinaryenExpressionRef), segmentSizes : Pointer(BinaryenIndex), numSegments : BinaryenIndex)
    
    # Start function. One per module
    
    fun BinaryenSetStart(modl : BinaryenModuleRef, start : BinaryenFunctionRef)
    
    #
    # ========== Module Operations ==========
    #
    
    # Parse a module in s-expression text format
    fun BinaryenModuleParse(text : Pointer(UInt8)) : BinaryenModuleRef
    
    # Print a module to stdout in s-expression text format. Useful for debugging.
    fun BinaryenModulePrint(modl : BinaryenModuleRef)
    
    # Print a module to stdout in asm.js syntax.
    fun BinaryenModulePrintAsmjs(modl : BinaryenModuleRef)
    
    # Validate a module, showing errors on problems.
    #  @return 0 if an error occurred, 1 if validated succesfully
    fun BinaryenModuleValidate(modl : BinaryenModuleRef) : LibC::Int
    
    # Runs the standard optimization passes on the module. Uses the currently set
    # global optimize and shrink level.
    fun BinaryenModuleOptimize(modl : BinaryenModuleRef)
    
    # Gets the currently set optimize level. Applies to all modules, globally.
    # 0, 1, 2 correspond to -O0, -O1, -O2 (default), etc.
    fun BinaryenGetOptimizeLevel() : LibC::Int
    
    # Sets the optimization level to use. Applies to all modules, globally.
    # 0, 1, 2 correspond to -O0, -O1, -O2 (default), etc.
    fun BinaryenSetOptimizeLevel(level : LibC::Int)
    
    # Gets the currently set shrink level. Applies to all modules, globally.
    # 0, 1, 2 correspond to -O0, -Os (default), -Oz.
    fun BinaryenGetShrinkLevel() : LibC::Int
    
    # Sets the shrink level to use. Applies to all modules, globally.
    # 0, 1, 2 correspond to -O0, -Os (default), -Oz.
    fun BinaryenSetShrinkLevel(level : LibC::Int)
    
    # Gets whether generating debug information is currently enabled or not.
    # Applies to all modules, globally.
    fun BinaryenGetDebugInfo() : LibC::Int
    
    # Enables or disables debug information in emitted binaries.
    # Applies to all modules, globally.
    fun BinaryenSetDebugInfo(on : LibC::Int)
    
    # Runs the specified passes on the module. Uses the currently set global
    # optimize and shrink level.
    fun BinaryenModuleRunPasses(modl : BinaryenModuleRef, passes : Pointer(UInt8), numPasses : BinaryenIndex)
    
    # Auto-generate drop() operations where needed. This lets you generate code without
    # worrying about where they are needed. (It is more efficient to do it yourself,
    # but simpler to use autodrop).
    fun BinaryenModuleAutoDrop(modl : BinaryenModuleRef)
    
    # Serialize a module into binary form. Uses the currently set global debugInfo option.
    # @return how many bytes were written. This will be less than or equal to outputSize
    fun BinaryenModuleWrite(modl : BinaryenModuleRef, output : Pointer(UInt8), outputSize : LibC::SizeT) : LibC::SizeT
    
    struct BinaryenBufferSizes
      outputBytes : LibC::SizeT
      sourceMapBytes : LibC::SizeT
    end
    
    # Serialize a module into binary form including its source map. Uses the currently set
    # global debugInfo option.
    # @returns how many bytes were written. This will be less than or equal to outputSize
    fun BinaryenModuleWriteWithSourceMap(modl : BinaryenModuleRef, url : Pointer(UInt8), output : Pointer(UInt8), outputSize : LibC::SizeT, sourceMap : Pointer(UInt8), sourceMapSize : LibC::SizeT) : BinaryenBufferSizes
    
    # Result structure of BinaryenModuleAllocateAndWrite. Contained buffers have been allocated
    # using malloc() and the user is expected to free() them manually once not needed anymore.
    struct BinaryenModuleAllocateAndWriteResult
      binary : Pointer(Void)
      binaryBytes : LibC::SizeT
      sourceMap : Pointer(UInt8)
    end
    
    # Serializes a module into binary form, optionally including its source map if
    # sourceMapUrl has been specified. Uses the currently set global debugInfo option.
    # Differs from BinaryenModuleWrite in that it implicitly allocates appropriate buffers
    # using malloc(), and expects the user to free() them manually once not needed anymore.
    fun BinaryenModuleAllocateAndWrite(modl : BinaryenModuleRef, sourceMapUrl : Pointer(UInt8)) : BinaryenModuleAllocateAndWriteResult
    
    # Deserialize a module from binary form.
    fun BinaryenModuleRead(input : Pointer(UInt8), inputSize : LibC::SizeT) : BinaryenModuleRef
    
    # Execute a module in the Binaryen interpreter. This will create an instance of
    # the module, run it in the interpreter - which means running the start method -
    # and then destroying the instance.
    fun BinaryenModuleInterpret(modl : BinaryenModuleRef)
    
    # Adds a debug info file name to the module and returns its index.
    fun BinaryenModuleAddDebugInfoFileName(modl : BinaryenModuleRef, filename : Pointer(UInt8)) : BinaryenIndex
    
    # Gets the name of the debug info file at the specified index. Returns `NULL` if it
    # does not exist.
    fun BinaryenModuleGetDebugInfoFileName(modl : BinaryenModuleRef, index : BinaryenIndex) : Pointer(UInt8)
    
    #
    # ======== FunctionType Operations ========
    #
    
    # Gets the name of the specified `FunctionType`.
    fun BinaryenFunctionTypeGetName(ftype : BinaryenFunctionTypeRef) : Pointer(UInt8)
    # Gets the number of parameters of the specified `FunctionType`.
    fun BinaryenFunctionTypeGetNumParams(ftype : BinaryenFunctionTypeRef) : BinaryenIndex
    # Gets the type of the parameter at the specified index of the specified `FunctionType`.
    fun BinaryenFunctionTypeGetParam(ftype : BinaryenFunctionTypeRef, index : BinaryenIndex) : BinaryenType
    # Gets the result type of the specified `FunctionType`.
    fun BinaryenFunctionTypeGetResult(ftype : BinaryenFunctionTypeRef) : BinaryenType
    
    #
    # ========== Function Operations ==========
    #
    
    # Gets the name of the specified `Function`.
    fun BinaryenFunctionGetName(func : BinaryenFunctionRef) : Pointer(UInt8)
    # Gets the name of the `FunctionType` associated with the specified `Function`. May be `NULL` if the signature is implicit.
    fun BinaryenFunctionGetType(func : BinaryenFunctionRef) : Pointer(UInt8)
    # Gets the number of parameters of the specified `Function`.
    fun BinaryenFunctionGetNumParams(func : BinaryenFunctionRef) : BinaryenIndex
    # Gets the type of the parameter at the specified index of the specified `Function`.
    fun BinaryenFunctionGetParam(func : BinaryenFunctionRef, index : BinaryenIndex) : BinaryenType
    # Gets the result type of the specified `Function`.
    fun BinaryenFunctionGetResult(func : BinaryenFunctionRef) : BinaryenType
    # Gets the number of additional locals within the specified `Function`.
    fun BinaryenFunctionGetNumVars(func : BinaryenFunctionRef) : BinaryenIndex
    # Gets the type of the additional local at the specified index within the specified `Function`.
    fun BinaryenFunctionGetVar(func : BinaryenFunctionRef, index : BinaryenIndex) : BinaryenType
    # Gets the body of the specified `Function`.
    fun BinaryenFunctionGetBody(func : BinaryenFunctionRef) : BinaryenExpressionRef
    
    # Runs the standard optimization passes on the function. Uses the currently set
    # global optimize and shrink level.
    fun BinaryenFunctionOptimize(func : BinaryenFunctionRef, modl : BinaryenModuleRef)
    
    # Runs the specified passes on the function. Uses the currently set global
    # optimize and shrink level.
    fun BinaryenFunctionRunPasses(func : BinaryenFunctionRef, modl : BinaryenModuleRef, passes : Pointer(UInt8), numPasses : BinaryenIndex)
    
    # Sets the debug location of the specified `Expression` within the specified `Function`.
    fun BinaryenFunctionSetDebugLocation(func : BinaryenFunctionRef, expr : BinaryenExpressionRef, fileIndex : BinaryenIndex, lineNumber : BinaryenIndex, columnNumber : BinaryenIndex)
    
    #
    # ========== Import Operations ==========
    #
    
    # Gets the external kind of the specified import.
    fun BinaryenImportGetKind(import : BinaryenImportRef) : BinaryenExternalKind
    # Gets the external module name of the specified import.
    fun BinaryenImportGetModule(import : BinaryenImportRef) : Pointer(UInt8)
    # Gets the external base name of the specified import.
    fun BinaryenImportGetBase(import : BinaryenImportRef) : Pointer(UInt8)
    # Gets the internal name of the specified import.
    fun BinaryenImportGetName(import : BinaryenImportRef) : Pointer(UInt8)
    # Gets the type of the imported global, if referencing a `Global`.
    fun BinaryenImportGetGlobalType(import : BinaryenImportRef) : BinaryenType
    # Gets the name of the function type of the imported function, if referencing a `Function`.
    fun BinaryenImportGetFunctionType(import : BinaryenImportRef) : Pointer(UInt8)
    
    #
    # ========== Export Operations ==========
    #
    
    # Gets the external kind of the specified export.
    fun BinaryenExportGetKind(export : BinaryenExportRef) : BinaryenExternalKind
    # Gets the external name of the specified export.
    fun BinaryenExportGetName(export : BinaryenExportRef) : Pointer(UInt8)
    # Gets the internal name of the specified export.
    fun BinaryenExportGetValue(export : BinaryenExportRef) : Pointer(UInt8)
    
    #
    # ========== CFG / Relooper ==========
    #
    # General usage is (1) create a relooper, (2) create blocks, (3) add
    # branches between them, (4) render the output.
    #
    # See Relooper.h for more details
    
    alias RelooperRef = Pointer(Void)
    alias RelooperBlockRef = Pointer(Void)
    
    # Create a relooper instance
    fun RelooperCreate() : RelooperRef
    
    # Create a basic block that ends with nothing, or with some simple branching
    fun RelooperAddBlock(relooper : RelooperRef, code : BinaryenExpressionRef) : RelooperBlockRef
    
    # Create a branch to another basic block
    # The branch can have code on it, that is executed as the branch happens. this is useful for phis. otherwise, code can be NULL
    fun RelooperAddBranch(from : RelooperBlockRef, to : RelooperBlockRef, condition : BinaryenExpressionRef, code : BinaryenExpressionRef)
    
    # Create a basic block that ends a switch on a condition
    fun RelooperAddBlockWithSwitch(relooper : RelooperRef, code : BinaryenExpressionRef, condition : BinaryenExpressionRef) : RelooperBlockRef
    
    # Create a switch-style branch to another basic block. The block's switch table will have these indexes going to that target
    fun RelooperAddBranchForSwitch(from : RelooperBlockRef, to : RelooperBlockRef, indexes : Pointer(BinaryenIndex), numIndexes : BinaryenIndex, code : BinaryenExpressionRef)
    
    # Generate structed wasm control flow from the CFG of blocks and branches that were created
    # on this relooper instance. This returns the rendered output, and also disposes of the
    # relooper and its blocks and branches, as they are no longer needed.
    #   @param labelHelper To render irreducible control flow, we may need a helper variable to
    #                      guide us to the right target label. This value should be an index of
    #                      an i32 local variable that is free for us to use.
    fun RelooperRenderAndDispose(relooper : RelooperRef, entry : RelooperBlockRef, labelHelper : BinaryenIndex, modl : BinaryenModuleRef) : BinaryenExpressionRef
    
    #
    # ========= Other APIs =========
    #
    
    # Sets whether API tracing is on or off. It is off by default. When on, each call
    # to an API method will print out C code equivalent to it, which is useful for
    # auto-generating standalone testcases from projects using the API.
    # When calling this to turn on tracing, the prelude of the full program is printed,
    # and when calling it to turn it off, the ending of the program is printed, giving
    # you the full compilable testcase.
    # TODO: compile-time option to enable/disable this feature entirely at build time?
    fun BinaryenSetAPITracing(on : LibC::Int)
    
    #
    # ========= Utilities =========
    #
    
    # Note that this function has been added because there is no better alternative
    # currently and is scheduled for removal once there is one. It takes the same set
    # of parameters as BinaryenAddFunctionType but instead of adding a new function
    # signature, it returns a pointer to the existing signature or NULL if there is no
    # such signature yet.
    fun BinaryenGetFunctionTypeBySignature(modl : BinaryenModuleRef, result : BinaryenType, paramTypes : Pointer(BinaryenType), numParams : BinaryenIndex) : BinaryenFunctionTypeRef
end
