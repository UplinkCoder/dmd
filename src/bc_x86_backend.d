module ddmd.ctfe.bc_x86_backend;
import ddmd.ctfe.bc_common;
import std.conv;

struct X86_BCGen
{
    ubyte* codeMemory;
    uint codeMemLeft;

    bool sameLabel = true;
    auto sp = StackAddr(4);
    ushort temporaryCount;
    uint fn;
    uint[128] functions;
    uint functionCount;
    uint beginFunction();
    void endFunction();
    BCValue interpret(BCValue[] args, BCHeap* heapPtr);
    BCValue genTemporary(BCType bct);
    BCValue genParameter(BCType bct);
    BCAddr beginJmp();
    void endJmp(BCAddr atIp, BCLabel target);
    void incSp();
    StackAddr currSp();
    BCLabel genLabel();
    CndJmpBegin beginCndJmp(BCValue cond = BCValue.init, bool ifTrue = false);
    void setb_al()
    {
        emit([ 0x0f, 0x92, 0xc0, ]);
    }

    void endCndJmp(CndJmpBegin jmp, BCLabel target);
    void genJump(BCLabel target);
    void emitFlg(BCValue lhs);
    void AssertError(BCValue val, BCValue msg);
    void Alloc(BCValue heapPtr, BCValue size);
    void Not(BCValue _result, BCValue val)
    {
        //negl(edi)
        //[0xf7, 0x1f]
    }
    void Set(BCValue lhs, BCValue rhs);
    void StoreStack(BCValue value, BCValue addr);
    void Lt3(BCValue _result, BCValue lhs, BCValue rhs);
    void Gt3(BCValue _result, BCValue lhs, BCValue rhs);
    void Eq3(BCValue _result, BCValue lhs, BCValue rhs);
    void Neq3(BCValue _result, BCValue lhs, BCValue rhs);
    void Add3(BCValue _result, BCValue lhs, BCValue rhs)
    {
        //01 0e                   add    %ecx,(%esi)
    }
    void Sub3(BCValue _result, BCValue lhs, BCValue rhs)
    {
        //29 0e                   sub    %ecx,(%esi)
    }
    void Mul3(BCValue _result, BCValue lhs, BCValue rhs)
    {
        //0f af 0e                imul   (%esi),%ecx
        //89 0e                   mov    %ecx,(%esi)
    }
    void Div3(BCValue _result, BCValue lhs, BCValue rhs)
    {
        // 8b 06                   mov    (%esi),%eax
        // 31 d2                   xor    %edx,%edx
        // f7 f1                   div    %ecx
        // 89 06                   mov    %eax,(%esi)
    }
    void And3(BCValue _result, BCValue lhs, BCValue rhs)
    {
        //21 0e                   and    %ecx,(%esi)
    }
    void Or3(BCValue _result, BCValue lhs, BCValue rhs)
    {
        //09 0e                   or     %ecx,(%esi)
    }
    void Xor3(BCValue _result, BCValue lhs, BCValue rhs);
    void Lsh3(BCValue _result, BCValue lhs, BCValue rhs);
    void Rsh3(BCValue _result, BCValue lhs, BCValue rhs);
    void Mod3(BCValue _result, BCValue lhs, BCValue rhs);
    void Byte3(BCValue _result, BCValue word, BCValue idx);
    void Call(BCValue _result, BCValue fn, BCValue[] args);
    void Load32(BCValue _to, BCValue from);
    void Store32(BCValue _to, BCValue value);
    void Ret(BCValue val);
    void Cat3(BCValue _result, const BCValue lhs, const BCValue rhs, const uint size);
    void Halt(BCValue message);

}
