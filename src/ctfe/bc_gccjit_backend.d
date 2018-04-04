module ddmd.ctfe.bc_gccjit_backend;
static if (!is(typeof ({ import gccjit.c;  })))
{
    pragma(msg, "gccjit header not there ... not compiling bc_gccjit backend");
}
else
{
    import gccjit.c;

    alias jctx = gcc_jit_context*;
    alias jfunc = gcc_jit_function*;
    alias jtype = gcc_jit_type*;
    alias jresult = gcc_jit_result*;
    alias jparam = gcc_jit_param*;
    alias jblock = gcc_jit_block*;
    alias jlvalue = gcc_jit_lvalue*;
    alias jrvalue = gcc_jit_rvalue*;
    alias jfield = gcc_jit_field*;
    alias jstruct = gcc_jit_struct*;

    struct BCFunction
    {
        void* funcDecl;

        jfunc func;
        char* fname;
        int nArgs;
    }
}

//version (Have_libgccjit)
    struct GCCJIT_BCGen
{
    enum max_params = 64;
    import gccjit.c;
    import ddmd.ctfe.bc_common;
    import std.stdio;
    import std.conv;
    import std.string;

    static struct FunctionState 
    {
        jlvalue[64] parameters;
        ubyte parameterCount;
        jblock[512] blocks;
        uint blockCount;
        jlvalue[1024] locals;
        ushort localCount;
        jlvalue[1024] temporaries;
        ushort temporaryCount;
        jlvalue[temporaries.length + locals.length] stackValues;
        ushort stackValueCount;
    }

    pragma(msg, FunctionState.sizeof);

    FunctionState[] functionStates;

    jfunc func()
    {
        return functions[functionCount].func;
    }

    FunctionState* currentFunctionState()
    {
        return &functionStates[functionCount];
    }

    alias currentFunctionState this;


    static void bc_jit_main()
    {

        BCHeap heap;
        heap.heapSize = 100;
        writeln(heap.heapSize);
        enum hwString = "Hello World.\nI've been missing you.";
        auto hello_world = heap.pushString(&hwString[0], hwString.length);

        GCCJIT_BCGen *gen = new GCCJIT_BCGen();
        with (gen)
        {
            Initialize();
            gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING, 0);
            {

                beginFunction(0);

                auto t32_lo = genLocal(BCType(BCTypeEnum.i64), "t32_lo");
                auto t32_hi = genLocal(BCType(BCTypeEnum.i64), "t32_hi");
                auto t64_hi = genLocal(BCType(BCTypeEnum.i64), "t64_hi");
                auto t64 = genLocal(BCType(BCTypeEnum.i64), "t64");
                auto mem = genLocal(BCType(BCTypeEnum.i32), "mem");
                auto mem4 = genLocal(BCType(BCTypeEnum.i32), "mem4");

                Alloc(mem, imm32(8));
                Add3(mem4, mem, imm32(4));
                Store32(mem4, imm32(0x13371337));
                Store32(mem, imm32(0xDEADBEEF));
                print_ptr(rvalue(imm32(0xDEADBEEF)), "0xDEADBEEF");
                Load32(t32_lo,mem);
                print_ptr(rvalue(t32_lo), "t32_lo(DEADBEEF)");
                Load32(t32_hi,mem4);
                Lsh3(t64_hi, t32_hi, imm32(32));
                print_ptr(rvalue(t64_hi), "t64_hi");
                Or3(t64_hi, t64_hi, t32_lo); 
                print_ptr(rvalue(t64_hi), "t64_hi");
                
                //Or3(t64, t64_hi, t32_lo);
                Load64(t64, mem);
                print_ptr(rvalue(t64), "t64");
                Ret(t64);

                endFunction();
                Finalize();
            }
        }

        auto rv = gen.run(0, [imm32(64),imm32(32),imm32(32)], &heap);

        writeln("heapSize: ", heap.heapSize, " rv: ", rv);
    }

    BCValue run(uint fnId, BCValue[] args, BCHeap *heapPtr)
    {
        extern (C) struct ReturnType
        {
            long imm64;
            uint type;
        }

        assert(result, "No result. Did you try to run before calling Finalize?");

        alias fType = 
            extern (C) ReturnType function(uint fnId, long[max_params] args,
                uint* heapSize, uint* heap, ReturnType* returnValue);
                

        auto func = cast(fType)
            gcc_jit_result_get_code(result, "__Runner__");

        long[max_params] fnArgs;

        foreach(i, arg;args)
        {
            fnArgs[i] = arg.imm64.imm64; 
        }

        ReturnType returnValue;

        auto rv = func(fnId, fnArgs, &heapPtr.heapSize, &heapPtr._heap[0], &returnValue);
        printf("rv: %p ret: %p", rv, returnValue.imm64);
        return imm64(returnValue.imm64);


    }


    jctx ctx;
    jresult result;
    // static globals
    jlvalue flag;
    jlvalue heapSize;
    jlvalue _heap;
    jlvalue returnVal;
    jfield returnValueImm64Field;
    jfield returnValueTypeField;
    jparam[4] dispParams;

    void* heapSizePtrPtr;
    void* heapArrayPtrPtr;

    BCFunction[64] functions;

    gcc_jit_location* currentLoc(int line = __LINE__)
    {
         return gcc_jit_context_new_location(ctx,
         "src/ctfe/bc_gccjit_backend.d", line, 0
        );
    }

    uint functionCount;

    bool insideFunction = false;

    jtype i32type;
    jtype i64type;
    jtype u32type;
    jtype u64type;

    private void print_int(BCValue v)
    {
        print_int(rvalue(v));
    }

    private void print_int(jrvalue val, char* name = null)
    {
        jrvalue[3] args;
        args[0] = gcc_jit_context_new_rvalue_from_ptr(ctx, gcc_jit_context_get_type(ctx, GCC_JIT_TYPE_CONST_CHAR_PTR),  cast(void*)"%s:%d\n".ptr);
        args[1] = gcc_jit_context_new_string_literal(ctx, name);
        args[2] = val;
        auto call = gcc_jit_context_new_call(ctx, currentLoc, printf_fn, 2, &args[0]);
        gcc_jit_block_add_eval(block, currentLoc, call);
    }

    private void print_ptr(jrvalue val, string name = null)
    {
        jrvalue[2] args;
        string formatString = name ? name~": %p\n\0" : "%p\n";
        args[0] = gcc_jit_context_new_rvalue_from_ptr(ctx, gcc_jit_context_get_type(ctx, GCC_JIT_TYPE_CONST_CHAR_PTR),  cast(void*)formatString.ptr);
        args[1] = val;
        auto call = gcc_jit_context_new_call(ctx, currentLoc, printf_fn, 2, &args[0]);
        gcc_jit_block_add_eval(block, currentLoc, call);
    }

    private void print_string(jrvalue base, jrvalue length)
    {
        jrvalue[4] args;
        auto c_char_p = gcc_jit_context_get_type(ctx, GCC_JIT_TYPE_CONST_CHAR_PTR);
        auto void_p = gcc_jit_context_get_type(ctx, GCC_JIT_TYPE_VOID_PTR);
        args[0] = gcc_jit_context_new_rvalue_from_ptr(ctx, c_char_p,  cast(void*)"string: \"%.*s\" :: length %d\n".ptr);

        jrvalue length_times_four = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_MULT, i32type,
            length,
            rvalue_int(4)
        );

        args[1] = length_times_four;

        jlvalue ptr = gcc_jit_function_new_local(func, currentLoc, void_p, "ptr");
        gcc_jit_block_add_assignment(block, currentLoc, ptr, gcc_jit_context_null(ctx, void_p));
        print_ptr(rvalue(_heap), "heap_ptr");

        auto addr = gcc_jit_lvalue_get_address(gcc_jit_context_new_array_access(ctx, currentLoc, rvalue(_heap), base), null);
        gcc_jit_block_add_assignment(block, null,
            ptr, addr);


        args[2] = rvalue(ptr);
        args[3] = length;
        print_ptr(rvalue(ptr));
        auto call = gcc_jit_context_new_call(ctx, currentLoc, printf_fn, 4, &args[0]);
        gcc_jit_block_add_eval(block, currentLoc, call);
    }


    private void call_puts(jrvalue arg)
    {
        auto puts = gcc_jit_context_get_builtin_function(ctx, "puts");
        auto call = gcc_jit_context_new_call(ctx, currentLoc, puts, 1, &arg);
        gcc_jit_block_add_eval(block, currentLoc, call);
    }

    private void printHeapString(uint addr)
    {
        jlvalue base = gcc_jit_context_new_array_access(ctx, null,
            rvalue(_heap), rvalue(addr)
        );

        jlvalue length = gcc_jit_context_new_array_access(ctx, null,
            rvalue(_heap), rvalue(addr - 1)
        );

        auto heapBase = gcc_jit_lvalue_get_address(_heap, null);



        print_int(rvalue(base));

        print_string(rvalue(base), rvalue(length));
    }

    private jblock block()
    {
        assert(blockCount, "blockCount is zero");
        return blocks[blockCount - 1];
    }

    private void newBlock(const char* name = null)
    {
        blocks[blockCount++] = gcc_jit_function_new_block(func, name);
    }

    private jlvalue param(ubyte paramIndex)
    {
        return parameters[paramIndex];
        //return gcc_jit_function_get_param(func, paramIndex);
    }

    private jrvalue zero(jtype type = null)
    {
        type = type ? type : i64type;
        return gcc_jit_context_zero(ctx, type);
    }

    private jlvalue lvalue(BCValue val)
    {
        assert(val.vType != BCValueType.Immediate);
        if (val.vType == BCValueType.Parameter)
        {
            return param(val.paramIndex);
        }
        else if (val.vType == BCValueType.StackValue || val.vType == BCValueType.Local)
        {
            return stackValues[val.stackAddr];
        }
        else
            assert(0, "vType: " ~ enumToString(val.vType) ~ " is currently not supported");

    }

    private jrvalue rvalue(BCValue val)
    {
        //assert(val.isStackValueOrParameter);
        jrvalue rv;
        if (val.vType == BCValueType.Parameter)
        {
            rv = rvalue(param(val.paramIndex));
        }
        else if (val.vType == BCValueType.Immediate)
        {
             
            if (val.type == BCTypeEnum.i64)
                rv = gcc_jit_context_new_rvalue_from_long(ctx, i64type, val.imm64.imm64);
            else
                rv = gcc_jit_context_new_cast(ctx, currentLoc, gcc_jit_context_new_rvalue_from_int(ctx, u32type, val.imm32.imm32), i64type);
        }
        else if (val.vType == BCValueType.StackValue || val.vType == BCValueType.Local)
        {
            rv = rvalue(stackValues[val.stackAddr]);
        }
        else if (val.vType == BCValueType.Error)
        {
            rv = rvalue(val.imm32);
        }
        else
            assert(0, "vType: " ~ enumToString(val.vType) ~ " is currently not supported");

        return rv;
    }

    private jrvalue rvalue(jlvalue val)
    {
            return gcc_jit_lvalue_as_rvalue(val);
    }

    private jrvalue rvalue(jrvalue val)
    {
            return val;
    }


    private jrvalue rvalue(long v, bool unsigned = false)
    {
        return gcc_jit_context_new_rvalue_from_long(ctx, unsigned ? u64type : i64type, v);
    }

    private jrvalue rvalue_int(int v)
    {
        return gcc_jit_context_new_rvalue_from_int(ctx, i32type, v);
    }

    private StackAddr addStackValue(jlvalue val)
    {
        stackValues[stackValueCount] = val;
        return StackAddr(stackValueCount++);
    }

    jtype heapType;
    jtype returnType;
    jfunc dispatcherFn;
    jstruct returnType_struct; 
    jtype paramArrayType;

    jfunc memcpy;
    jfunc printf_fn;

    void Initialize()
    {
        import core.stdc.stdlib : malloc;
        functionStates = (cast(FunctionState*)malloc(265*FunctionState.sizeof))[0 .. 265];
        ctx = gcc_jit_context_acquire();
        u32type = gcc_jit_context_get_int_type(ctx, 4, 0);
        u64type = gcc_jit_context_get_int_type(ctx, 8, 0);
        i32type =  gcc_jit_context_get_int_type(ctx, 4, 1);//gcc_jit_context_get_int_type(ctx, 32, 1);
        i64type = gcc_jit_context_get_int_type(ctx, 8, 1);

        paramArrayType = gcc_jit_context_new_array_type(ctx, currentLoc, i64type, max_params);

        jfield[2] fields;
        fields[0]  = returnValueImm64Field = gcc_jit_context_new_field(ctx, null, i64type, "imm64");
        fields[1] = returnValueTypeField = gcc_jit_context_new_field(ctx, null, u32type, "type");
        returnType_struct = gcc_jit_context_new_struct_type(ctx, null, "returnType", 2, &fields[0]);
        returnType = gcc_jit_struct_as_type(returnType_struct);

        printf_fn = gcc_jit_context_get_builtin_function(ctx, "printf");

        // memcpy = gcc_jit_context_get_builtin_function(ctx, "memcpy");

        heapType =
            gcc_jit_type_get_pointer(u32type);

        // debug stuff
        ctx.gcc_jit_context_set_bool_option(
            GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE, 1
        );

        gcc_jit_context_set_bool_option(ctx,
            GCC_JIT_BOOL_OPTION_DEBUGINFO, 1
        );

        gcc_jit_context_set_int_option(ctx,
            GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 3
        );

        gcc_jit_context_set_bool_allow_unreachable_blocks(ctx,
            1
        );

        ctx.gcc_jit_context_set_bool_option(
            GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES, 1
        );

        jparam[5] runParams;

        runParams[0] = gcc_jit_context_new_param(ctx, currentLoc, i32type, "fnIdx");
        runParams[1] = gcc_jit_context_new_param(ctx, currentLoc, paramArrayType, "paramArray0");// long[64] args;
        runParams[2] = gcc_jit_context_new_param(ctx, currentLoc, gcc_jit_type_get_pointer(i32type), "heapSize"); //uint* heapSize
        runParams[3] = gcc_jit_context_new_param(ctx, currentLoc, heapType, "heap"); //uint[2^^26] heap
        runParams[4] = gcc_jit_context_new_param(ctx, currentLoc, gcc_jit_type_get_pointer(returnType), "returnValue");

        dispParams[0] = gcc_jit_context_new_param(ctx, currentLoc, i32type, "fnIdx");
        dispParams[1] = gcc_jit_context_new_param(ctx, currentLoc, paramArrayType, "paramArray0");// long[64] args;
        dispParams[2] = gcc_jit_context_new_param(ctx, currentLoc, gcc_jit_type_get_pointer(i32type), "heapSize"); //uint* heapSize
        dispParams[3] = gcc_jit_context_new_param(ctx, currentLoc, heapType, "heap"); //uint[2^^26] heap


        returnVal = gcc_jit_context_new_global(ctx, null, GCC_JIT_GLOBAL_INTERNAL, returnType, "__g_return");
        flag = gcc_jit_context_new_global(ctx, null, GCC_JIT_GLOBAL_INTERNAL, gcc_jit_context_get_type(ctx, GCC_JIT_TYPE_BOOL), "__g_flag");


        auto runnerFn = gcc_jit_context_new_function(ctx,
            null, GCC_JIT_FUNCTION_EXPORTED, i64type,
            "__Runner__",
            cast(int)runParams.length, &runParams[0], 0
        );    

        dispatcherFn = gcc_jit_context_new_function(ctx,
            null, GCC_JIT_FUNCTION_INTERNAL, i64type,
            "__Dispatcher__",
            cast(int)dispParams.length, &dispParams[0], 0
        );

        jrvalue[4] dispArgs;

        dispArgs[0] = gcc_jit_param_as_rvalue(runParams[0]);
        dispArgs[1] = gcc_jit_param_as_rvalue(runParams[1]);
        dispArgs[2] = gcc_jit_param_as_rvalue(runParams[2]);
        dispArgs[3] = gcc_jit_param_as_rvalue(runParams[3]);

        auto runnerBlock = gcc_jit_function_new_block(runnerFn, "Runner");
        auto dispCall = 
            gcc_jit_context_new_call(ctx, currentLoc, dispatcherFn, 4, &dispArgs[0]);
        gcc_jit_block_add_eval(runnerBlock, currentLoc, dispCall);
        gcc_jit_block_add_assignment(runnerBlock, currentLoc, 
            gcc_jit_rvalue_dereference(gcc_jit_param_as_rvalue(runParams[4]), currentLoc),
            rvalue(returnVal)
        );
        gcc_jit_block_end_with_return(runnerBlock, currentLoc, rvalue(0));

    }

    void Finalize()
    {
        // build the dispatcher function here
        jblock[] blocks;
        blocks.length = functionCount*2 + 2;
        jrvalue fnIdx = gcc_jit_param_as_rvalue(dispParams[0]);
        foreach(int i, f; functions[0 .. functionCount])
        {
            auto i2 = i * 2;
            blocks[i2] = gcc_jit_function_new_block(dispatcherFn, null);

            jrvalue args[3];
            args[0] = gcc_jit_param_as_rvalue(dispParams[1]);
            args[1] = gcc_jit_param_as_rvalue(dispParams[2]);
            args[2] = gcc_jit_param_as_rvalue(dispParams[3]);

            blocks[i2 + 1] = gcc_jit_function_new_block(dispatcherFn, null);
            gcc_jit_block_add_eval(blocks[i2 + 1], currentLoc,
                gcc_jit_context_new_call(ctx, currentLoc, f.func, 3, &args[0])
            );

        }


        blocks[functionCount*2] = gcc_jit_function_new_block(dispatcherFn, "wrongFunctionPtrs");
        gcc_jit_block_end_with_return(blocks[functionCount*2], currentLoc, rvalue(imm32(-1)));

        blocks[functionCount*2 + 1] = gcc_jit_function_new_block(dispatcherFn, "setReturnValue");

        gcc_jit_block_end_with_return(blocks[functionCount*2 + 1], currentLoc, rvalue(imm32(0)));

        foreach(int i; 0 .. functionCount)
        {
            auto i2 = i * 2;
            if (i2 == 0)
            {
                jrvalue[2] printfArgs;
                printfArgs[0] = gcc_jit_context_new_string_literal(ctx, "Calling fnIdx: %d\n");
                printfArgs[1] = rvalue(fnIdx);
                gcc_jit_block_add_eval(blocks[i2], currentLoc,
                    gcc_jit_context_new_call(ctx, currentLoc, printf_fn, 2, &printfArgs[0])
                );
            }

            gcc_jit_block_end_with_conditional(blocks[i2], currentLoc,
                gcc_jit_context_new_comparison(ctx, currentLoc, GCC_JIT_COMPARISON_EQ, rvalue(fnIdx), gcc_jit_context_new_rvalue_from_int(ctx, i32type, i)),
                blocks[i2 + 1], blocks[i2 + 2]
            );
            gcc_jit_block_end_with_jump(blocks[i2 + 1], currentLoc, blocks[functionCount*2 + 1]);
        }

        gcc_jit_context_dump_to_file(ctx, "ctx.c", 1);
        result = gcc_jit_context_compile(ctx);
    }

    void beginFunction(uint fnId, void* fd = null)
    {
        insideFunction = true;
        writeln("parameterCount: ", parameterCount);
        writeln ("functionIndex: ", (&functionStates[0])  - currentFunctionState);
        static if (is(typeof(() {import ddmd.func : FuncDeclaration; })))
        {
            import ddmd.func : FuncDeclaration;
        }
        else
        {
            struct FuncDeclaration_
            {
                char* toChars;
            }
            alias FuncDeclaration = FuncDeclaration_*;
        }
        auto name = fd ? (cast(FuncDeclaration) fd).toChars : null;

        jparam[3] p;
        p[0] = gcc_jit_context_new_param(ctx, currentLoc, paramArrayType, "paramArray");// long[64] args;
        p[1] = gcc_jit_context_new_param(ctx, currentLoc, gcc_jit_type_get_pointer(i32type), "heapSize"); //uint* heapSize
        p[2] = gcc_jit_context_new_param(ctx, currentLoc, heapType, "heap"); //uint[2^^26] heap

        assert(!functions[functionCount].funcDecl);
        functions[functionCount].funcDecl = fd;

        functions[functionCount].fname = cast(char*) (name ? name : ("f" ~ to!string(fnId)).toStringz);
        functions[functionCount].func =  gcc_jit_context_new_function(ctx,
            null, GCC_JIT_FUNCTION_INTERNAL, i64type,
            cast(const) functions[functionCount].fname,
            cast(int)p.length, cast(jparam*)&p, 0
        );

        newBlock("prologue");

        foreach(uint _p;0 .. parameterCount)
        {
            parameters[_p] = gcc_jit_context_new_array_access(ctx, null,
                gcc_jit_param_as_rvalue(p[0]), rvalue(_p)
            );
        }

        heapSize = gcc_jit_param_as_lvalue(p[1]);
        _heap = gcc_jit_param_as_lvalue(p[2]);

        newBlock("body");

        gcc_jit_block_end_with_jump(blocks[blockCount - 2], currentLoc, block);
    }

    BCFunction endFunction()
    {
        insideFunction = false;
        return functions[functionCount++];
    }

    BCValue genTemporary(BCType bct)
    {
        //TODO replace i64Type maybe depding on bct
        auto type = i64type;
        char[20] name = "tmp";
        sprintf(&name[3], "%d", temporaryCount);
        temporaries[temporaryCount++] = gcc_jit_function_new_local(func, currentLoc, type, &name[0]);
        auto addr = addStackValue(temporaries[temporaryCount - 1]);
        return BCValue(addr, bct, temporaryCount);
    }

    BCValue genLocal(BCType bct, string name)
    {
        assert(name, "locals have to have a name");
        //TODO replace i64Type maybe depding on bct
        auto type = i64type;
        locals[localCount++] = gcc_jit_function_new_local(func, currentLoc, type, &name[0]);
        auto addr = addStackValue(locals[localCount - 1]);
        auto bcLocal = BCValue(addr, bct, localCount, name);
        Set(bcLocal, imm32(0));
        return bcLocal;
    }

    BCValue genParameter(BCType bct, string name = null)
    {
        import std.string;
        //TODO we might want to keep track of the parameter type ?
        if (bct.type == BCTypeEnum.Struct || BCTypeEnum.Class)
            bct = i32Type;

        if (bct != i32Type && bct != BCType(BCTypeEnum.i64))
            assert(0, "can currently only create params of i32Type not: " ~ to!string(bct.type));
        //parameters[parameterCount] =
        auto r = BCValue(BCParameter(parameterCount++, bct, StackAddr(0)));
        return r;

    }

    BCAddr beginJmp(int line = __LINE__)
    {
        newBlock(("beginJmp_" ~ to!string(line) ~ "\0").ptr);
        return BCAddr(blockCount - 2);
    }

    void endJmp(BCAddr atIp, BCLabel target)
    {
        gcc_jit_block_end_with_jump(blocks[atIp], currentLoc, blocks[target.addr]);
    }

    BCLabel genLabel(int line = __LINE__)
    {
        newBlock(("genLabel_" ~ to!string(line) ~ "\0").ptr);
        gcc_jit_block_end_with_jump(blocks[blockCount - 2], currentLoc, block);
        return BCLabel(BCAddr(blockCount-1));
    }

    CndJmpBegin beginCndJmp(BCValue cond = BCValue.init, bool ifTrue = false, int line = __LINE__)
    {
        newBlock(("cndJmp_fallthrough_" ~ to!string(line) ~ "\0").ptr);
        auto cjb = CndJmpBegin(BCAddr(blockCount-2), cond, ifTrue);

        return cjb;
    }

    void endCndJmp(CndJmpBegin jmp, BCLabel target)
    {
        //newBlock("endCndJmp");
        auto targetBlock = blocks[target.addr.addr];
        auto falltroughBlock = blocks[jmp.at.addr + 1];

        jblock true_block;
        jblock false_block;

        if (jmp.ifTrue)
        {
            true_block = targetBlock;
            false_block = falltroughBlock;
        }
        else
        {
            true_block = falltroughBlock;
            false_block = targetBlock;
        }

        auto cond = gcc_jit_context_new_comparison(ctx, null,
            GCC_JIT_COMPARISON_NE, rvalue(jmp.cond), zero
        );

        gcc_jit_block_end_with_conditional(blocks[jmp.at.addr], null,
            cond, true_block, false_block);
    }

    void genJump(BCLabel target)
    {
        gcc_jit_block_end_with_jump(block, currentLoc, blocks[target.addr.addr]);
        newBlock();
    }

    void emitFlg(BCValue lhs)
    {
        gcc_jit_block_add_assignment(block, currentLoc, lvalue(lhs), rvalue(flag));
    }

    void Alloc(BCValue heapPtr, BCValue size)
    {
        auto _size = rvalue(size);
        _size = gcc_jit_context_new_cast(ctx, null,
            _size, i32type
        );

        auto _heapSize = gcc_jit_rvalue_dereference(rvalue(heapSize), null);

        auto rheapSize = gcc_jit_context_new_cast(ctx, null,
            rvalue(_heapSize), i64type
        );

        auto result = lvalue(heapPtr);

        gcc_jit_block_add_assignment(block, null,
            result, rheapSize,
        );

        gcc_jit_block_add_assignment_op(block, null,
            _heapSize, GCC_JIT_BINARY_OP_PLUS, _size
        );
    }

    void Assert(BCValue value, BCValue err)
    {
        auto AssertCJ = beginCndJmp(value);
        gcc_jit_block_add_assignment(block, currentLoc,
            gcc_jit_lvalue_access_field(returnVal, currentLoc, returnValueImm64Field), rvalue(err)
        );
        gcc_jit_block_add_assignment(block, currentLoc,
            gcc_jit_lvalue_access_field(returnVal, currentLoc, returnValueTypeField),
            gcc_jit_context_new_rvalue_from_int(ctx, u32type, BCValueType.Error)
        );
        endCndJmp(AssertCJ, genLabel());
    }

    void Not(BCValue result, BCValue val)
    {
        gcc_jit_block_add_assignment(block, currentLoc, lvalue(result),
            gcc_jit_context_new_unary_op(ctx, currentLoc, GCC_JIT_UNARY_OP_BITWISE_NEGATE, i64type, rvalue(val))
        );
    }

    void Set(BCValue lhs, BCValue rhs)
    {
        gcc_jit_block_add_assignment(block, currentLoc, lvalue(lhs), rvalue(rhs));
    }

    void Lt3(BCValue result, BCValue lhs, BCValue rhs)
    {
        auto cmp = 
            gcc_jit_context_new_comparison(ctx, currentLoc, GCC_JIT_COMPARISON_LT, rvalue(lhs), rvalue(rhs));

        auto _result = result ? lvalue(result) : flag;
        if (result)
            cmp = gcc_jit_context_new_cast(ctx, currentLoc, cmp, i64type);

        gcc_jit_block_add_assignment(block, currentLoc, _result, cmp);
    }

    void Le3(BCValue result, BCValue lhs, BCValue rhs)
    {
        auto cmp = 
            gcc_jit_context_new_comparison(ctx, currentLoc, GCC_JIT_COMPARISON_LE, rvalue(lhs), rvalue(rhs));

        auto _result = result ? lvalue(result) : flag;
        if (result)
            cmp = gcc_jit_context_new_cast(ctx, currentLoc, cmp, i64type);

        gcc_jit_block_add_assignment(block, currentLoc, _result, cmp);
    }

    void Gt3(BCValue result, BCValue lhs, BCValue rhs)
    {
        auto cmp = 
            gcc_jit_context_new_comparison(ctx, currentLoc, GCC_JIT_COMPARISON_GT, rvalue(lhs), rvalue(rhs));

        auto _result = result ? lvalue(result) : flag;
        if (result)
            cmp = gcc_jit_context_new_cast(ctx, currentLoc, cmp, i64type);

        gcc_jit_block_add_assignment(block, currentLoc, _result, cmp);
    }

    void Ge3(BCValue result, BCValue lhs, BCValue rhs)
    {
        auto cmp = 
            gcc_jit_context_new_comparison(ctx, currentLoc, GCC_JIT_COMPARISON_GE, rvalue(lhs), rvalue(rhs));

        auto _result = result ? lvalue(result) : flag;
        if (result)
            cmp = gcc_jit_context_new_cast(ctx, currentLoc, cmp, i64type);

        gcc_jit_block_add_assignment(block, currentLoc, _result, cmp);    
    }

    void Eq3(BCValue result, BCValue lhs, BCValue rhs)
    {
        auto cmp = 
            gcc_jit_context_new_comparison(ctx, currentLoc, GCC_JIT_COMPARISON_EQ, rvalue(lhs), rvalue(rhs));

        auto _result = result ? lvalue(result) : flag;
        if (result)
            cmp = gcc_jit_context_new_cast(ctx, currentLoc, cmp, i64type);

        gcc_jit_block_add_assignment(block, currentLoc, _result, cmp);
    }

    void Neq3(BCValue result, BCValue lhs, BCValue rhs)
    {
        auto _result = result ? lvalue(result) : flag;
        gcc_jit_block_add_assignment(block, currentLoc, _result,
            gcc_jit_context_new_comparison(ctx, currentLoc, GCC_JIT_COMPARISON_NE, rvalue(lhs), rvalue(rhs))
        );
    }

    void Add3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_PLUS, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );
    }

    void Sub3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_MINUS, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );
    }

    void Mul3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_MULT, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );
    }

    void Div3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_DIVIDE, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );
        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );

    }

    void And3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_BITWISE_AND, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );

    }

    void Or3(BCValue result, BCValue lhs, BCValue rhs)
    {
        //assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_BITWISE_OR, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );

    }

    void Xor3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_BITWISE_XOR, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );

    }

    void Lsh3(BCValue result, BCValue lhs, BCValue rhs)
    {
        //assert(lhs.type == i32Type || && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_LSHIFT, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );

    }
    void Rsh3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_LSHIFT, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );

    }

    void Mod3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(lhs.type == i32Type && rhs.type == i32Type);
        assert(lhs.isStackValueOrParameter || lhs.vType == BCValueType.Immediate);
        assert(rhs.isStackValueOrParameter || rhs.vType == BCValueType.Immediate);


        auto _result = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_MODULO, i64type,
            rvalue(lhs),
            rvalue(rhs)
        );

        gcc_jit_block_add_assignment(block, null,
            lvalue(result), _result
        );
    }

    static if (is(typeof(() { import ddmd.globals : Loc; })))
    {
        import ddmd.globals : Loc;
    }
    else
    {
        struct Loc
        {
            int line;
            int col;
            char* file;
        }
    }
    // 



    void Call(BCValue result, BCValue fn, BCValue[] args, Loc l = Loc.init)
    {
        if (l != Loc.init)
        {
            Comment("CallFrom:" ~ to!string(l.tupleof[0]));
        }

        jrvalue[5] fnArgs;

        auto callArgs =
            gcc_jit_function_new_local(func, currentLoc, paramArrayType, "callArgs");
            
        foreach(i, arg; args)
        {
            gcc_jit_block_add_assignment(block, currentLoc,
                gcc_jit_context_new_array_access(ctx, currentLoc, rvalue(callArgs), rvalue(i)),
                rvalue(arg)
            );
        }
        auto fnMinusOne = genTemporary(i32Type);
        Sub3(fnMinusOne, fn, imm32(1));
        fnArgs[0] = gcc_jit_context_new_cast(ctx, currentLoc, rvalue(fnMinusOne), i32type);
        fnArgs[1] = rvalue(callArgs);
        fnArgs[2] = rvalue(heapSize);
        fnArgs[3] = rvalue(_heap);
        if (fn.vType == BCValueType.Immediate && fn.imm32 < functionCount)
        {

            assert(functions[fn.imm32 - 1].func);
            auto call = gcc_jit_context_new_call(
                ctx, currentLoc, 
                functions[fn.imm32 - 1].func, 3, &fnArgs[1]
            );

            gcc_jit_block_add_eval(block, currentLoc, call);
        }
        else
        {
            auto call = gcc_jit_context_new_call(
                ctx, currentLoc, 
                dispatcherFn, 4, &fnArgs[0]
            );

            gcc_jit_block_add_eval(block, currentLoc, call);
        }

    }

    void Load32(BCValue _to, BCValue from)
    {
        gcc_jit_block_add_assignment(block, currentLoc, 
            lvalue(_to),
            gcc_jit_context_new_cast(ctx, currentLoc,
                rvalue(gcc_jit_context_new_array_access(ctx, currentLoc, rvalue(_heap), rvalue(from))),
                i64type
            )
        );
    }

    void Store32(BCValue _to, BCValue value)
    {
        gcc_jit_block_add_assignment(block, currentLoc, 
            gcc_jit_context_new_array_access(ctx, currentLoc, rvalue(_heap), rvalue(_to)),
            gcc_jit_context_new_cast(ctx, currentLoc,
                rvalue(value),
                u32type
            )
        );
    }

    void Load64(BCValue _to, BCValue from)
    {
        auto rValueHeap = rvalue(_heap);
        auto rValueFrom = rvalue(from);
        auto lValueTo = lvalue(_to);
        gcc_jit_block_add_assignment(block, currentLoc, 
            lValueTo,
            gcc_jit_context_new_cast(ctx, currentLoc,
                rvalue(gcc_jit_context_new_array_access(ctx, currentLoc, rValueHeap, rValueFrom)),
                i64type
            )
        );

        auto highAddr = gcc_jit_context_new_binary_op(ctx, currentLoc, 
            GCC_JIT_BINARY_OP_PLUS, i64type, rValueFrom, rvalue(4)
        );

        gcc_jit_block_add_assignment_op(
            block, currentLoc, lValueTo, GCC_JIT_BINARY_OP_BITWISE_OR,
            gcc_jit_context_new_cast(ctx, currentLoc,
                gcc_jit_context_new_binary_op(
                    ctx, currentLoc, GCC_JIT_BINARY_OP_LSHIFT, u64type,
                    gcc_jit_context_new_cast(ctx, currentLoc,
                        rvalue(gcc_jit_context_new_array_access(ctx, currentLoc, rvalue(_heap), highAddr)),
                        u64type
                    ),
                    rvalue(32, true)
                ),
                i64type
            ),
        );
    }

    void Store64(BCValue _to, BCValue value)
    {
        auto rValueHeap = rvalue(_heap);
        gcc_jit_block_add_assignment(block, currentLoc, 
            gcc_jit_context_new_array_access(ctx, currentLoc, rvalue(_heap), rvalue(_to)),
            gcc_jit_context_new_cast(ctx, currentLoc,
                rvalue(value),
                i32type
            )
        );

    }


    void IToF32(BCValue _to, BCValue value){ assert(0, __PRETTY_FUNCTION__ ~ " not implemented"); }
    void IToF64(BCValue _to, BCValue value){ assert(0, __PRETTY_FUNCTION__ ~ " not implemented"); }
    void F64ToI(BCValue _to, BCValue value){ assert(0, __PRETTY_FUNCTION__ ~ " not implemented"); }
    void F32ToI(BCValue _to, BCValue value)
    {

        assert(0, __PRETTY_FUNCTION__ ~ " not implemented"); 
    }


    void Comment(string msg)
    {
        gcc_jit_block_add_comment(block, currentLoc, msg.toStringz);
    }

    void Line(uint line)
    {
        //if (blockCount) 
        //    gcc_jit_block_add_comment(block, currentLoc, ("# Line (" ~ to!string(line) ~ ")").toStringz);
    }

    void Ret(BCValue val)
    {
        gcc_jit_block_add_assignment(block, currentLoc, gcc_jit_lvalue_access_field(returnVal, currentLoc, returnValueImm64Field), rvalue(val));
        gcc_jit_block_end_with_return(block, currentLoc, rvalue(1));
    }

    void MemCpy(BCValue lhs, BCValue rhs, BCValue size)
    {
        jrvalue _lhs = rvalue(lhs.i32);
        jrvalue _rhs = rvalue(rhs.i32);
        jrvalue _size = rvalue(size);


        jrvalue size_times_four = gcc_jit_context_new_binary_op (
            ctx, null,
            GCC_JIT_BINARY_OP_MULT, i64type,
            _size,
            rvalue(uint.sizeof)
        );

        auto rHeap = rvalue(_heap);
        jrvalue[3] args;
        args[0] = gcc_jit_lvalue_get_address(gcc_jit_context_new_array_access(ctx, currentLoc, rHeap, _lhs), currentLoc); // dest
        args[1] = gcc_jit_lvalue_get_address(gcc_jit_context_new_array_access(ctx, currentLoc, rHeap, _rhs), currentLoc); // src
        args[2] = size_times_four;

        auto memcpyCall = gcc_jit_context_new_call(ctx, currentLoc, memcpy, 3, &args[0]);
        gcc_jit_block_add_eval(block, currentLoc, memcpyCall);

    }
}
