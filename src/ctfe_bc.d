module ddmd.ctfe_bc;

import ddmd.expression;
import ddmd.declaration : FuncDeclaration, VarDeclaration, Declaration;
import ddmd.dsymbol;
import ddmd.dstruct;
import ddmd.mtype;
import ddmd.statement;
import ddmd.visitor;
import ddmd.arraytypes : Expressions;

/**
 * Written By Stefan Koch in 2016
 * All Rights Reserved.
 */

/**
        Something about the instruction set :
        The instruction set is designed to be fast to interpret.
        Therefore it is subject to uncommen restrictions and features uncommen ideoms.

        It is one of the few VM instruction sets that do have SIMD-Instructions.

        Example include The multiple increment which can increment 2 diffrent stack locations at once.
        (By either 1,4,8, or 16)

        The operation-combine marker which allows to execute the same chain of operations on an abitary number of StackLocations.


*/

import std.conv : to;

__gshared SharedCtfeState _sharedCtfeState;
__gshared SharedCtfeState* sharedCtfeState = &_sharedCtfeState;
static private
{
    ScopeStatement reduceNestedScopeAndCompoundStatements(ScopeStatement _ss) pure
    {
        if (_ss is null)
            return null;

        for (;;)
        {
            auto _cs = reduceNestedCompundAndScopeStatements(_ss.statement.isCompoundStatement);
            auto __ss = ((_cs) ? _cs.last.isScopeStatement : _ss.statement.isScopeStatement);
            if (__ss)
                _ss = __ss;
            else
                return _ss;
        }
    }

    CompoundStatement reduceNestedCompundAndScopeStatements(CompoundStatement cs) pure
    {
        if (cs is null)
            return null;

        while (cs.statements.dim == 1)
        {
            auto _ss = reduceNestedScopeAndCompoundStatements((*cs.statements)[0].isScopeStatement);
            auto _cs = (
                _ss ? _ss.statement.isCompoundStatement : ((*cs.statements)[0].isCompoundStatement));
            if (_cs)
                cs = _cs;
            else
                return cs;
        }

        return cs;
    }
}
/*struct SelfCall
{
    BCAddr callPoint;
    BCValue[16] arguments;
    ubyte argumentCount;
}*/

struct SwitchFixupEntry
{
    BCAddr atIp;
    alias atIp this;
    /// 0 means jump after the swich
    /// -1 means jump to the defaultStmt
    /// positve numbers denote which case to jump to
    int fixupFor;
}

struct BoolExprFixupEntry
{
    BCAddr atIp;
    BCValue cond;
    alias atIp this;
    bool ifTrue;
}

struct SwitchState
{
    SwitchFixupEntry[128] switchFixupTable;
    uint switchFixupTableCount;

    BCLabel[128] beginCaseStatements;
    uint beginCaseStatementsCount;
}

Expression evaluateFunction(FuncDeclaration fd, Expressions* args, Expression thisExp)
{
    Expression[] _args;
    //TODO check if the functions returnType is a uint;

    if (args)
        foreach (a; *args)
        {
            _args ~= a;
        }

    return evaluateFunction(fd, _args ? _args : [], thisExp);
}

import ddmd.ctfe.bc;

Expression evaluateFunction(FuncDeclaration fd, Expression[] args, Expression _this = null)
{
    scope BCV bcv = new BCV(fd);
    //  bcv.setThis(bcv);
    /*  if (fd.ctfeCode) {
                return executeFun(fd.ctfeCode, args);
        } else {
                fd.ctfeCode = compile(fd);
                return executeFun(fd.ctfeCode, args);
        } */
    import std.datetime : StopWatch;

    StopWatch csw;

    if (auto fbody = fd.fbody.isCompoundStatement)
    {
        csw.start();
        bcv.beginParameters();
        if (fd.parameters)
            foreach (i, p; *(fd.parameters))
            {
                debug(ctfe)
                {
                    import std.stdio;

                    writeln("parameter [", i, "] : ", p.toString);
                }
                p.accept(bcv);
            }
        bcv.endParameters();
        debug(ctfe)
        {
            import std.stdio;

            writeln("ParameterType : ", bcv.parameterTypes);
        }
        import std.stdio;

        bcv.visit(fbody);
        csw.stop();

        debug(ctfe)
        {
            import std.stdio;
            import std.algorithm;

            //writeln("EXECPTION!!! :", th.msg);

            bcv.printInstructions.writeln;
            bcv.vars.keys.each!(k => (cast(VarDeclaration) k).print);
            bcv.vars.writeln;
            writeln("Generting bc took " ~ csw.peek.usecs.to!string ~ "usecs");

            writeln(" stackUsage = ", (bcv.sp - 4).to!string ~ " byte");
            writeln(" TemporaryCount = ", (bcv.temporaryCount).to!string);
        }

    }

    if (!bcv.IGaveUp)
    {
        import std.algorithm;
        import std.range;
        import std.datetime : StopWatch;
        import std.stdio;

        StopWatch sw;
        sw.start();
        foreach (a; args)
        {
            a.toString();
            //a.accept(bcv);
        }

        auto argumentsBeginIp = bcv.beginArguments();
        auto bc_args = args.map!(a => bcv.genExpr(a)).array;
        bcv.endArguments(argumentsBeginIp);

        bcv.printInstructions.writeln;

        auto retval = interpret(bcv.byteCodeArray[0 .. bcv.ip], bc_args);
        sw.stop();
        import std.stdio;

        writeln("Executing bc took " ~ sw.peek.msecs.to!string ~ "msecs");
        if (retval != -1)
        {
            return new IntegerExp(retval);
        }
        else
        {
            assert(0, "Interpreter Errored"); //return null;
        }
    }
    else
    {
        assert(0, "CTFE Errored");
        //return null;

    }

}

string toString(T)(T value) if (is(T : Statement) || is(T : Declaration)
        || is(T : Expression) || is(T : Dsymbol) || is(T : Type))
{
    import core.stdc.string : strlen;

    const(char)* cPtr = value.toChars();
    return cast(string) cPtr[0 .. strlen(cPtr)];
}

struct BCArray
{
    BCType elementType;
    uint elementTypeIndex;

    uint length;

    const(uint) arraySize() const
    {
        return length * basicTypeSize(elementType);
    }

    const(uint) arraySize(const SharedCtfeState* sharedState) const
    {
        return sharedState.size(elementType, elementTypeIndex) * length;
    }
}

struct BCStruct
{
    BCType[ubyte.max] memberTypes;
    uint[ubyte.max] memberTypeIndexs;

    uint memeberTypesCount;
    //    uint[] methodByteCode;

    void addField(BCType bct)
    {
        memberTypes[memeberTypesCount++] = bct;
    }
}

struct SharedCtfeState
{
    uint _threadLock;
    //Type 0 beeing the terminator for chainedTypes
    void*[ubyte.max] structDeclPointers;

    BCStruct[ubyte.max] structs;
    uint structCount;
    BCArray[ubyte.max] arrays;
    uint arrayCount;
    //BCPointer[ubyte.max] pointers;
    //uint pointerCount;

    bool addStructInProgress;

    BCStruct* beginStruct(void* structDeclPointer)
    {
        assert(!addStructInProgress);
        addStructInProgress = true;
        structDeclPointers[structCount] = structDeclPointer;
        return &structs[structCount];
    }

    const(BCType) endStruct(BCStruct* s)
    {
        assert(addStructInProgress);
        assert(s == &structs[structCount]);
        addStructInProgress = false;

        return BCType(BCTypeEnum.Struct, structCount++);
    }

    const(uint) size(const BCType type, const uint elementTypeIndex) const
    {

        switch (type)
        {
        case BCTypeEnum.Struct:
            {
                uint _size;
                assert(elementTypeIndex <= structCount);
                BCStruct _struct = structs[elementTypeIndex];

                //import std.algorithm : sum;
                foreach (i, memberType; _struct.memberTypes[0 .. _struct.memeberTypesCount])
                {
                    _size += isBasicBCType(memberType) ? basicTypeSize(memberType) : this.size(memberType,
                        _struct.memberTypeIndexs[i]);
                }

                return _size;

            }

        case BCType.Array:
            {
                assert(elementTypeIndex <= arrayCount);
                BCArray _array = arrays[elementTypeIndex];

                return (
                    isBasicBCType(_array.elementType) ? _array.arraySize() : _array.arraySize(
                    &this));
            }

        default:
            {
                return 0;
            }

        }
    }
}

extern (C++) final class BCV : Visitor
{

    BCGen gen;
    alias gen this;

    BCAddr headJmp;

    // for now!
    BCValue[] arguments;
    BCType[] parameterTypes;

    bool processingArguments;
    bool processingParameters;

    bool IGaveUp;
    /// just used in switch handling to share local state between visit functions
    SwitchState switchState;
    SwitchFixupEntry* switchFixup;

    FuncDeclaration me;
    bool inReturnStatement;

    const(BCType) toBCType(Type t) /*pure*/
    {
        TypeBasic bt = t.isTypeBasic;
        if (bt)
        {
            switch (bt.ty)
            {
            case ENUMTY.Tbool:
                return BCType(BCTypeEnum.i1);
            case ENUMTY.Tint8:
            case ENUMTY.Tuns8:
                return BCType(BCTypeEnum.i8);
            case ENUMTY.Tchar:
                return BCType(BCTypeEnum.Char);
            case ENUMTY.Tint16:
            case ENUMTY.Tuns16:
                return BCType(BCTypeEnum.i16);
            case ENUMTY.Tint32:
            case ENUMTY.Tuns32:
                return BCType(BCTypeEnum.i32);
            case ENUMTY.Tint64:
            case ENUMTY.Tuns64:
                return BCType(BCTypeEnum.i64);
            default:
                IGaveUp = true;
                debug(ctfe) assert(0, "Type unsupported " ~ (cast(Type)(t)).toString());
                return BCType.init;
            }
        }
        else
        {
            if (t.isString)
            {
                return BCType(BCTypeEnum.String);
            }
            else if (t.ty == Tstruct)
            {
                StructDeclaration sd = (cast(TypeStruct) t).sym;
                auto st = sharedCtfeState.beginStruct(cast(void*) sd);

                foreach (sMember; sd.fields)
                {
                    st.addField(toBCType(sMember.type));
                }

                return sharedCtfeState.endStruct(st);

            }

            IGaveUp = true;

            debug(ctfe) assert(0, "NBT Type unsupported " ~ (cast(Type)(t)).toString());
            return BCType.init;
        }
    }

    alias visit = super.visit;

    import ddmd.tokens;

    BCBlock[void*] labeledBlocks;

    BCValue[void*] vars;
    BoolExprFixupEntry[ubyte.max] fixupTable;
    uint fixupTableCount;

    BCBlock* currentBlock;
    BCAddr[ubyte.max] breakFixups;
    uint breakFixupsCount;

    BCValue retval;
    BCValue assignTo;

    bool discardValue = false;

    string printInstructions()
    {
        return ddmd.ctfe.bc.printInstructions(cast(int*) byteCodeArray.ptr, ip.addr);
    }

    enum VisitationType
    {
        asExpression,
        asDeclatation,
    }

public:

    this(FuncDeclaration fd)
    {
        me = fd;
        headJmp = beginJmp();
    }

    ~this()
    {
        //We are about to finish codegen
        //time to fixup unresolved labels
        //and time to fixup self calls

        scope (exit)
        {
            import std.stdio;

            if (!__ctfe)
                writeln(printInstructions( /*gen.byteCodeArray[0 .. ip])*/ ));
        }

    }

    void beginParameters()
    {
        processingParameters = true;
    }

    void endParameters()
    {
        sp += (parameterTypes.length * 4);
        processingParameters = false;
    }

    BCAddr beginArguments()
    {
        processingArguments = true;
        return ip;
    }

    void endArguments(BCAddr argIp)
    {
        endJmp(headJmp, BCLabel(argIp));
        processingArguments = false;
        genJump(BCLabel(BCAddr(6)));
    }

    BCValue genExpr(Expression expr)
    {

        debug(ctfe)
        {
            import std.stdio;
        }
        auto oldRetval = retval;

        if (processingArguments)
        {
            assignTo = BCValue(StackAddr(cast(short)(4 + (arguments.length * 4))),
                BCType(BCTypeEnum.i32));
            assert(arguments.length <= parameterTypes.length, "passed to many arguments");
        }

        expr.accept(this);

        debug(ctfe)
        {
            writeln("expr: ", expr.toString, " == ", retval);
        }
//        assert(!discardValue || retval.vType != BCValueType.Unknown);
        BCValue ret = retval;
        retval = oldRetval;

        //        if (processingArguments) {
        //            arguments ~= retval;
        //            assert(arguments.length <= parameterTypes.length, "passed to many arguments");
        //        }

        return ret;
    }

    BCValue genLt(Expression lhs, Expression rhs)
    {
        auto _lhs = genExpr(lhs);
        auto _rhs = genExpr(rhs);

        BCValue result;
        Lt3(result, _lhs, _rhs);
        return result;
    }

    BCValue genEq(Expression lhs, Expression rhs)
    {
        auto _lhs = genExpr(lhs);
        auto _rhs = genExpr(rhs);
        BCValue result;
        Eq3(result, _lhs, _rhs);

        retval = result;
        return result;
    }

/*    static bool chainedBooleanExp(BinExp e) {
        if (e.op == TOKandand || e.op == TOKoror) {
            if (e.e1.op == TOKandand || e.e1.op == TOKoror) {

            }
        }

    }
*/
    override void visit(BinExp e)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("Called visit(BinExp) %s ... \n\tdiscardReturnValue %d",
                e.toString, discardValue);
            writefln("(BinExp).Op: %s", e.op.to!string);

        }

        retval = assignTo ? assignTo : genTemporary(toBCType(e.type)).value;

        switch (e.op)
        {
        case TOK.TOKequal:
            {
                Eq3(assignTo, genExpr(e.e1), genExpr(e.e2));
            }
            break;
        case TOK.TOKplusplus:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto expr = genExpr(e.e1);
                assert(expr.vType != BCValueType.Immidiate,
                    "++ does not make sense as on an Immidiate Value");

                discardValue = oldDiscardValue;
                if (assignTo || !discardValue)
                    emitSet(retval, expr);

                Add3(expr, expr, BCValue(Imm32(1)));

            }
            break;
        case TOK.TOKminusminus:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto expr = genExpr(e.e1);
                assert(expr.vType != BCValueType.Immidiate,
                    "-- does not make sense as on an Immidiate Value");

                discardValue = oldDiscardValue;
                if (assignTo || !discardValue)
                    emitSet(retval, expr);

                Sub3(expr, expr, BCValue(Imm32(1)));

            }
            break;
        case TOK.TOKadd:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto lhs = genExpr(e.e1);
                auto rhs = genExpr(e.e2);
                assert(!oldDiscardValue, "A lone add discarding the value is strange");
                Add3(retval, lhs, rhs);
                // TOOD use sizeof(retval);
                discardValue = oldDiscardValue;
            }
            break;
        case TOK.TOKmin:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto lhs = genExpr(e.e1);
                auto rhs = genExpr(e.e2);
                assert(!oldDiscardValue, "A lone sub discarding the value is strange");
                Sub3(retval, lhs, rhs);
                // TOOD use sizeof(retval);
                discardValue = oldDiscardValue;
            }
            break;
        case TOK.TOKmul:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto lhs = genExpr(e.e1);
                auto rhs = genExpr(e.e2);
                assert(!oldDiscardValue, "A lone mul discarding the value is strange");
                Mul3(retval, lhs, rhs);
                // TOOD use sizeof(retval);
                discardValue = oldDiscardValue;
            }
            break;
        case TOK.TOKdiv:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto lhs = genExpr(e.e1);
                auto rhs = genExpr(e.e2);
                assert(!oldDiscardValue, "A lone mul discarding the value is strange");
                Div3(retval, lhs, rhs);
                // TOOD use sizeof(retval);
                discardValue = oldDiscardValue;
            }
            break;
        case TOK.TOKoror:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto lhs = genExpr(e.e1);
                if (e.e1.op != TOKoror) fixupTable[fixupTableCount++] = BoolExprFixupEntry(beginCndJmp(), lhs, true);
                auto rhs = genExpr(e.e2);
                if (e.e2.op != TOKoror) fixupTable[fixupTableCount++] = BoolExprFixupEntry(beginCndJmp(), rhs, true);

                assert(!oldDiscardValue, "A lone oror discarding the value is strange");
                discardValue = oldDiscardValue;
            }
            break;
        case TOK.TOKandand:
            {
                const oldDiscardValue = discardValue;
                discardValue = false;
                auto lhs = genExpr(e.e1);
                if (e.e1.op != TOKandand) fixupTable[fixupTableCount++] = BoolExprFixupEntry(beginCndJmp(), lhs, false);
                auto rhs = genExpr(e.e2);
                if (e.e2.op != TOKandand) fixupTable[fixupTableCount++] = BoolExprFixupEntry(beginCndJmp(), rhs, false);

                assert(!oldDiscardValue, "A lone andand discarding the value is strange");
                discardValue = oldDiscardValue;
            }
            break;

        default:
            IGaveUp = true;
            debug(ctfe) assert(0, "BinExp.Op " ~ to!string(e.op) ~ " not handeled");
        }

    }

    override void visit(IndexExp ie)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("IndexExp %s ... \n\tdiscardReturnValue %d", ie.toString, discardValue);
            writefln("ie.type : %s ", ie.type.toString);
        }

        assert(ie.e1.type.isString, "For now only indexes into strings a supported");
        auto _string = genExpr(ie.e1);
        assert(_string.type == BCType.String);
        auto idx = genExpr(ie.e2);

        auto ptr = genTemporary(BCType(BCTypeEnum.i32)).value;
        /// we have to add the size of the length to the ptr
        Add3(ptr, _string, idx);
        Add3(ptr, ptr, BCValue(Imm32(basicTypeSize(BCType(BCTypeEnum.i32)))));

        assignTo = assignTo ? assignTo : genTemporary(BCType(BCTypeEnum.i32)).value;

        emitLongInst(LongInst64(LongInst.Lsb, assignTo.stackAddr, ptr.stackAddr));

        assert(idx.type == BCType(BCTypeEnum.i32));
        //emitLongInst(LongInst64(LongInst.Lss, assignTo.stackAddr, ptr.stackAddr));
        // *lhsRef = DS[aligin4(rhs)]
        retval = assignTo;

        //              writeln("ie.e1: ", genExpr(ie.e1).value.toString);
        //              writeln("ie.e2: ", genExpr(ie.e2).value.toString);
        //IGaveUp = true;
        //debug(ctfe) assert(0, "IndexExp unsupported");
    }

    BCBlock genBlock(Statement stmt)
    {
        BCBlock result;
        auto oldBlock = currentBlock;
        const oldBreakFixupsCount = breakFixupsCount;

        debug(ctfe)
        {
            import std.stdio;

            writeln("Calling genBlock on : ", stmt.toString);
        }
        currentBlock = &result;
        result.begin = BCLabel(ip);
        stmt.accept(this);
        result.end = BCLabel(ip);

        // Now let's fixup thoose breaks
        foreach (Jmp; breakFixups[oldBreakFixupsCount .. breakFixupsCount])
        {
            endJmp(Jmp, result.end);
        }
        currentBlock = oldBlock;
        breakFixupsCount = oldBreakFixupsCount;

        return result;
    }

    override void visit(ForStatement fs)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("ForStatement %s", fs.toString);
        }

        if (fs._init)
        {
            (fs._init.accept(this));
        }

        if (fs.condition !is null && fs._body !is null)
        {
            BCAddr condJmp;
            BCLabel condEval = genLabel();
            BCValue condExpr = genExpr(fs.condition);
            if (condExpr.vType == BCValueType.Unknown)
            {
                condJmp = beginCndJmp();
            }
            else
            {
                condJmp = beginJmpZ();
            }
            auto _body = genBlock(fs._body);
            if (fs.increment)
            {
                fs.increment.accept(this);
                _body.end = genLabel();
            }
            genJump(condEval);
            auto afterJmp = genLabel();
            if (condExpr.vType == BCValueType.Unknown)
            {
                endCndJmp(condJmp, afterJmp);
            }
            else
            {
                endJmpZ(condJmp, condExpr.stackAddr, afterJmp);
            }

        }
        else if (fs.condition !is null  /* && fs._body is null*/ )
        {
            BCLabel condEval = genLabel();
            BCValue condExpr = genExpr(fs.condition);
            if (fs.increment)
            {
                fs.increment.accept(this);
            }
            genJump(condEval);
        }
        else
        { // fs.condition is null && fs._body !is null
            auto _body = genBlock(fs._body);
            if (fs.increment)
            {
                fs.increment.accept(this);
            }
            genJump(_body.begin);
        }

    }

    override void visit(Expression e)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("Expression %s", e.toString);
        }

        assert(0, "Cannot handleExpression");
    }

    override void visit(DotVarExp dve)
    {
        if(dve.e1.type.ty == Tstruct) {
        auto structDeclPtr = cast(void*)((cast(TypeStruct) dve.e1.type).sym);
        BCStruct* _struct;
        foreach (i; 0 .. sharedCtfeState.structCount)
        {
            if (sharedCtfeState.structDeclPointers[i] == structDeclPtr)
            {
                _struct = &sharedCtfeState.structs[i];
                break;
            }
        }
        assert(_struct, "We don't know the struct Type");
        debug(ctfe)
        {
            import std.stdio;

            writeln(*_struct);
        }
        retval = (assignTo
            && assignTo.vType == BCValueType.StackValue) ? assignTo : genTemporary(
            BCType(BCTypeEnum.i32Ptr)).value;

        auto lhs = genExpr(dve.e1);

        assert(lhs.type == BCTypeEnum.Struct);
        // temporary hack :)
        lhs.type = BCTypeEnum.i32;
        
        assert(lhs.vType == BCValueType.StackValue);
        auto offset = BCValue(Imm32(dve.var.isVarDeclaration.offset));

        //auto ptr = genTemporary(BCType(BCTypeEnum.i32Ptr)).value;
        /// we have to add the size of the length to the ptr
        //Add3(ptr, lhs, offset);
            //HACK to make pointer arith work
        auto oldType = retval.type.type;
        retval.type.type = BCTypeEnum.i32;
        Add3(retval, lhs, offset);
        retval.type = oldType;

        debug(ctfe)
        {
            import std.stdio;

            writeln("dve.var : ", dve.var.toString);
            writeln(dve.var.isVarDeclaration.offset);
        }
        
        }
         else {
        assert(0, "Can only take members of a struct for now");
        }
    }

    override void visit(StructLiteralExp sle)
    {
        auto structDeclPtr = cast(void*) sle.sd;
        BCStruct* _struct;

        foreach (i; 0 .. sharedCtfeState.structCount)
        {
            if (sharedCtfeState.structDeclPointers[i] == structDeclPtr)
            {
                _struct = &sharedCtfeState.structs[i];
                break;
            }
        }
        assert(_struct, "We don't know the struct Type");
        foreach (ty; _struct.memberTypes[0 .. _struct.memeberTypesCount])
        {
            assert(ty.type == BCTypeEnum.i32, "can only deal with ints and uints atm.");
        }

        retval = assignTo ? assignTo : genTemporary(BCType(BCTypeEnum.i32)).value;
        /*HACK HACK HACK*/ sp += 4; //HACK
        auto result = BCValue(StackAddr(sp), BCType(BCTypeEnum.i32));

    
        foreach (elem; *sle.elements)
        {
            auto elexpr = genExpr(elem);
            assert(elexpr.type == BCTypeEnum.i32);
            emitSet(BCValue(StackAddr(sp), elexpr.type), elexpr);
            sp += align4(basicTypeSize(elexpr.type));
        }

        emitSet(retval, BCValue(Imm32(result.stackAddr)));
    }

    override void visit(ArrayLengthExp ale)
    {
        auto array = genExpr(ale.e1);
        assert(array.type == BCType.String, "We only handle StringLengths");
        assert(array.vType == BCValueType.StackValue, "We only handle StringLengths");
        retval = assignTo ? assignTo : genTemporary(BCType(BCTypeEnum.i32)).value;
        emitLongInst(LongInst64(LongInst.Lss, retval.stackAddr, array.stackAddr)); // *lhsRef = DS[aligin4(rhs)]
        retval = assignTo;
        //emitSet(, array);
        //emitPrt(retval);
        /*
        uint_32 length 
        uint_32 [length/4+1] chars;
         */
    }

    override void visit(VarExp ve)
    {
        auto vd = cast(void*) ve.var.isVarDeclaration;
        assert(vd, "VarExp " ~ ve.toString ~ "is not a VariableDeclaration !?!");
        auto sv = vd in vars;
        if (sv is null)
        {
            IGaveUp = true;
            return;
        }
        assert(sv, "Variable " ~ ve.toString ~ " not in StackFrame");

        debug(ctfe)
        {
            import std.stdio;

            writefln("VarExp %s discardValue %d", ve.toString, discardValue);
            writeln("ve.var sp : ", (vd in vars).stackAddr);
        }
        retval = (*sv);
    }

    override void visit(DeclarationExp de)
    {
        auto oldRetval = retval;
        auto vd = de.declaration.isVarDeclaration();
        assert(vd, "DeclarationExps are expected to be VariableDeclarations");
        visit(vd);
        auto var = retval;
        debug(ctfe)
        {
            import std.stdio;

            writefln("DeclarationExp %s discardValue %d", de.toString, discardValue);
            writefln("DeclarationExp.declaration: %x", cast(void*) de.declaration.isVarDeclaration);
        }

        if (auto ci = vd.getConstInitializer)
        {

            ci.accept(this);
            if (retval.vType == BCValueType.Immidiate && retval.type == BCType(BCTypeEnum.i32))
            {
                emitSet(var, retval);
            }
            retval = oldRetval;
        }
        assert(sp < ushort.max, "StackOverflow Stack is currently constrained to 64K");

    }

    override void visit(VarDeclaration vd)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("VarDeclaration %s discardValue %d", vd.toString, discardValue);
        }

        auto var = BCValue(StackAddr(sp), toBCType(vd.type));
        vars[cast(void*) vd] = var;
        sp += cast(short) align4(cast(uint) basicTypeSize(var.type));
        retval = var;

        if (processingParameters)
        {
            parameterTypes ~= var.type;
        }

        debug(ctfe)
        {
            import std.stdio;

            writeln("StackPointer after push: ", sp);
        }
    }

    override void visit(BinAssignExp e)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("BinAssignExp %s discardValue %d", e.toString, discardValue);
        }
        const oldDiscardValue = discardValue;
        auto oldAssignTo = assignTo;
        auto oldRetval = retval;
        discardValue = false;
        e.e1.accept(this);
        auto lhs = retval;
        //assert(lhs.vType == BCValueType.StackValue);
        discardValue = false;
        e.e2.accept(this);
        auto rhs = retval;
        //assert(rhs.vType == BCValueType.Immidiate);

        //assert(rhs.type == BCType.i32 && lhs.type == BCType(BCTypeEnum.i32));

        switch (e.op)
        {

        case TOK.TOKaddass:
            {
                retval = Add3(lhs, lhs, rhs);
            }
            break;
        default:
            {
                IGaveUp = true;
                debug(ctfe) assert(0, "Unsupported for now");
            }
        }
        //assert(discardValue);

        retval = oldDiscardValue ? oldRetval : retval;
        discardValue = oldDiscardValue;
        assignTo = oldAssignTo;
    }

    override void visit(IntegerExp ie)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("IntegerExpression %s", ie.toString);
        }

        auto bct = toBCType(ie.type);
        //assert(bct == BCType.i32, "only 32bit is suppoorted for now");
        retval = BCValue(Imm32(cast(uint) ie.getInteger()));
        assert(retval.vType == BCValueType.Immidiate);
    }

    override void visit(StringExp se)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("StringExpression %s", se.toString);
        }

        assert(se.sz == 1, "only char strings are supported for now");
        assert(se.string[se.len] == '\0', "string should be 0-terminated");
        auto result = BCValue(StackAddr(sp), BCType(BCTypeEnum.String));
        emitSet(BCValue(StackAddr(sp), BCType(BCTypeEnum.i32)), BCValue(Imm32(cast(int) se.len)));
        sp += align4(basicTypeSize(BCType(BCTypeEnum.i32)));
        auto rest = se.len % 4;
        foreach (cellIndex; 0 .. (se.len / 4) + (rest != 0))
        {
            emitSet(BCValue(StackAddr(sp), BCType(BCTypeEnum.i32)),
                BCValue(Imm32(*((cast(uint*) se.string) + cellIndex))));
            sp += align4(basicTypeSize(BCType(BCTypeEnum.i32)));
        }

        if (!rest)
        {
            //trailing 0
            sp += align4(basicTypeSize(BCType(BCTypeEnum.i32)));
        }

        if (assignTo)
        {
            emitSet(assignTo, BCValue(Imm32(result.stackAddr)));
        }

        retval = result;
    }

    override void visit(CmpExp ce)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("CmpExp %s discardValue %d", ce.toString, discardValue);
        }

        switch (ce.op)
        {
        case TOK.TOKlt:
            {
                retval = Lt3(assignTo, genExpr(ce.e1), genExpr(ce.e2));
            }
            break;

        case TOK.TOKgt:
            {
                retval = Gt3(assignTo, genExpr(ce.e1), genExpr(ce.e2));
            }
            break;

        default:
            IGaveUp = true;
            debug(ctfe) assert(0, "Unsupported Operation " ~ to!string(ce.op));
        }
    }

    override void visit(AssignExp ae)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("AssignExp %s", ae.toString);
        }
        auto oldRetval = retval;
        auto oldAssignTo = assignTo;
        const oldDiscardValue = discardValue;
        discardValue = false;
        auto lhs = genExpr(ae.e1);
        // another dirty hack.
        //transforming structs into pointers
        if (lhs.type == BCTypeEnum.Struct) {
            lhs.type = BCTypeEnum.i32;
        }
        assignTo = lhs;

        auto rhs = genExpr(ae.e2);
        emitSet(lhs, rhs);

        retval = oldDiscardValue ? oldRetval : retval;
        assignTo = oldAssignTo;
        discardValue = oldDiscardValue;

    }

    override void visit(SwitchStatement ss)
    {
        with (switchState)
        {
            //This Transforms swtich in a series of if else construts.
            debug(ctfe)
            {
                import std.stdio;

                writefln("SwitchStatement %s", ss.toString);
            }

            auto lhs = genExpr(ss.condition);

            assert(ss.cases.dim <= beginCaseStatements.length,
                "We will not have enough array space to store all cases for gotos");

            foreach (i, caseStmt; *(ss.cases))
            {
                caseStmt.index = cast(int) i;
                // apperantly I have to set the index myself;

                auto rhs = genExpr(caseStmt.exp);
                auto jmpCond = Eq3(BCValue.init, lhs, rhs);
                auto jump = beginCndJmp();

                auto cs = reduceNestedCompundAndScopeStatements(
                    caseStmt.statement.isCompoundStatement);
                auto _ss = reduceNestedScopeAndCompoundStatements(
                    caseStmt.statement.isScopeStatement);

                if (!cs && ss)
                {
                    cs = reduceNestedCompundAndScopeStatements(_ss.statement.isCompoundStatement);
                }

                static bool endsSwitchBlock(Statement stmt) pure
                {
                    return stmt.isBreakStatement || stmt.isReturnStatement
                        || stmt.isGotoCaseStatement || stmt.isGotoDefaultStatement;
                }

                bool blockReturns = ((cs && (endsSwitchBlock(cs.last))) || (_ss
                    && (endsSwitchBlock(_ss.statement))) || endsSwitchBlock(caseStmt.statement));

                switchFixup = &switchFixupTable[switchFixupTableCount];
                auto caseBlock = genBlock(caseStmt.statement);
                beginCaseStatements[beginCaseStatementsCount++] = caseBlock.begin;
                //If the block returns regardless there is no need for a fixup
                if (!blockReturns)
                {
                    switchFixupTable[switchFixupTableCount++] = beginJmp();
                    switchFixup = &switchFixupTable[switchFixupTableCount];
                }

                endCndJmp(jump, caseBlock.end);
            }
            if (ss.sdefault)
            {
                auto defaultBlock = genBlock(ss.sdefault.statement);

                foreach (ac_jmp; switchFixupTable[0 .. switchFixupTableCount])
                {
                    if (ac_jmp.fixupFor == 0)
                        endJmp(ac_jmp, defaultBlock.end);
                    else if (ac_jmp.fixupFor == -1)
                        endJmp(ac_jmp, defaultBlock.begin);
                    else
                        endJmp(ac_jmp, beginCaseStatements[ac_jmp.fixupFor - 1]);
                }
            }
            else
            {
                auto afterSwitch = genLabel();

                foreach (ac_jmp; switchFixupTable[0 .. switchFixupTableCount])
                {
                    if (ac_jmp.fixupFor == 0)
                        endJmp(ac_jmp, afterSwitch);
                    else if (ac_jmp.fixupFor != -1)
                        endJmp(ac_jmp, beginCaseStatements[ac_jmp.fixupFor - 1]);
                    else
                        assert(0, "Without a default Statement there cannot be a jump to default");
                }

            }

            switchFixupTableCount = 0;
            switchFixup = null;
            //after we are done let's set thoose indexes back to zero
            //who knowns what will happen if we don't ?
            foreach (cs; *(ss.cases))
            {
                cs.index = 0;
            }
        }
    }

    override void visit(GotoCaseStatement gcs)
    {
        with (switchState)
        {
            *switchFixup = SwitchFixupEntry(beginJmp(), gcs.cs.index + 1);
            switchFixupTableCount++;
        }
    }

    override void visit(GotoDefaultStatement gd)
    {
        with (switchState)
        {
            *switchFixup = SwitchFixupEntry(beginJmp(), -1);
            switchFixupTableCount++;
        }
    }

    override void visit(GotoStatement gs)
    {
        assert(cast(void*) gs.ident in labeledBlocks,
            "We have not encounterd the label you want to jump to");
        genJump(labeledBlocks[cast(void*) gs.ident].begin);
    }

    override void visit(LabelStatement ls)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("LabelStatement %s", ls.toString);
        }

        assert(cast(void*) ls.ident !in labeledBlocks,
            "We already enounterd a LabelStatement with this identifier");
        labeledBlocks[cast(void*) ls.ident] = genBlock(ls.statement);
    }

    override void visit(ContinueStatement cs)
    {
        if (cs.ident)
        {
            assert(cast(void*) cs.ident in labeledBlocks,
                "We have not encounterd the label you want to jump to");
            genJump(labeledBlocks[cast(void*) cs.ident].begin);
        }
        else
        {
            genJump(currentBlock.begin);
        }
    }

    override void visit(BreakStatement bs)
    {
        if (bs.ident)
        {
            assert(cast(void*) bs.ident in labeledBlocks,
                "We have not encounterd the label you want to jump to");
            genJump(labeledBlocks[cast(void*) bs.ident].end);
        }
        else if (switchFixup)
        {
            with (switchState)
            {
                *switchFixup = SwitchFixupEntry(beginJmp(), 0);
                switchFixupTableCount++;
            }
        }
        else
        {
            breakFixups[breakFixupsCount++] = beginJmp();
        }

    }

    override void visit(CallExp ce)
    {
        assert(inReturnStatement && ce.f == me,
            "only direct tail recursive calls are supported for now");

        /+
                //This is experimental do exepect hiccups;
                //first reset the stack
                sp = StackAddr(4);
                //then push the arguments on
                foreach(arg;ce.arguments.opSlice()) {
                        arg.accept(this);
                }
                // and jump to the start of the function;
                endJmp(beginJmp(), BCLabel(BCAddr(4)));
       +/
    }

    override void visit(ReturnStatement rs)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("ReturnStatement %s", rs.toString);
        }
        assert(!inReturnStatement);
        inReturnStatement = true;
        auto retval = genExpr(rs.exp);
        if (retval.vType == BCValueType.Immidiate)
        {
            retval = pushOntoStack(retval);
        }
        emitReturn(retval);
        inReturnStatement = false;
    }

    override void visit(CastExp ce)
    {
        // just go over the cast as if it were not there :)
        //FIXME make this handle casts properly
        //e.g. calling opCast do truncation and so on
        ce.e1.accept(this);
    }

    override void visit(ExpStatement es)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("ExpStatement %s", es.toString);
        }
        immutable oldDiscardValue = discardValue;
        discardValue = true;
        genExpr(es.exp);
        discardValue = oldDiscardValue;
    }
/*
    override void visit(WhileStatement ws)
    {
        auto evalBlockBegin = genLabel();
        BCValue condExpr = genExpr(ws.condition);
        auto tjmp = beginCndJmp();
        auto _body = genBlock(ws._body);
        genJump(evalBlockBegin);
        auto afterJmp = genLabel();
        endCndJmp(tjmp, afterJmp);
    }
*/
    override void visit(Statement s)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("Statement %s", s.toString);
        }
        IGaveUp = true;
        debug(ctfe) assert(0, "Statement unsupported");
        //s.accept(this);
    }

    override void visit(IfStatement fs)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("IfStatement %s", fs.toString);
        }

        uint oldFixupTableCount = fixupTableCount;
        auto cond = genExpr(fs.condition);
        auto branch = fs.condition.op == TOKandand ? typeof(beginCndJmp()).init : beginCndJmp();
        BCBlock ifbody;
        BCBlock elsebody;
        if (fs.ifbody)
        {
            ifbody = genBlock(fs.ifbody);
        
            if (fs.elsebody)
            {
                auto afterBodyJmp = beginJmp();
                elsebody = genBlock(fs.elsebody);
                endJmp(afterBodyJmp, genLabel());
            }
        }

        foreach(fixup;fixupTable[oldFixupTableCount .. fixupTableCount]) {
            if (fixup.ifTrue) {
                endCndJmp(fixup.atIp, ifbody ? ifbody.begin : genLabel(), true, fixup.cond);
            } else {
                endCndJmp(fixup.atIp, elsebody ? elsebody.begin : genLabel(), false, fixup.cond);
            }

            --fixupTableCount;
        }

        assert(oldFixupTableCount == fixupTableCount);
        /// NOTE THIS IS A HACK!
        /// It seems to work fine though
        switch(fs.condition.op) {
            case TOKoror :
            {
                endJmp(branch, elsebody ? elsebody.begin : genLabel());
            } break;
            case TOKandand :
            {

            } break;
            default :
            {
                endCndJmp(branch, elsebody ? elsebody.begin : genLabel());
            }
        }
    }

    override void visit(ScopeStatement ss)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("ScopeStatement %s", ss.toString);
        }
        ss.statement.accept(this);
    }

    override void visit(CompoundStatement cs)
    {
        debug(ctfe)
        {
            import std.stdio;

            writefln("CompundStatement %s", cs.toString);
        }

        foreach (stmt; *(cs.statements))
        {
            stmt.accept(this);
        }
    }
}
