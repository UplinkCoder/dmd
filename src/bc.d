module ddmd.ctfe.bc;
import core.stdc.stdio;
import std.conv;

/**
 * Written By Stefan Koch in 2016
 * All Rights Reserved.
 */

enum Cond
{
    Eq,
    Gt,
    Lt,
}

/+
static immutable uint[4] condFlagArr = [(1 << 0 * 8), (1 << 1 * 8), (1 << 2 * 8), (1 << 3 * 8)];

struct BCCondFlag
{
    enum BCCondFlagEnum
    {
        CF1,
        CF2,
        CF3,
        CF4,
    }

    BCCondFlagEnum flag;
    alias flag this;

    uint toFlagMask32()
    {
        return condFlagArr[flag];
    }
}

static assert(BCCondFlag.CF4 == 3);
+/
enum InstKind
{
    ShortInst,
    CndJumpInst,

    LongInst2Stack,
    LongInstImm32,

    StackInst,
}

InstKind instKind(LongInst i)
{
    final switch (i)
    {
    case LongInst.Prt, LongInst.RelJmp, LongInst.Ret, LongInst.Neg,
            LongInst.Soh, LongInst.Sdh, LongInst.Flg, LongInst.Drb, LongInst.Mod4:
        {
            return InstKind.ShortInst;
        }

    case LongInst.Jmp, LongInst.JmpFalse, LongInst.JmpTrue, LongInst.JmpZ, LongInst.JmpNZ:
        {
            return InstKind.CndJumpInst;
        }

    case LongInst.Add, LongInst.Sub, LongInst.Div, LongInst.Mul, LongInst.Eq,
            LongInst.Lt, LongInst.Gt, LongInst.Set, LongInst.And, LongInst.Or,
            LongInst.Lsh, LongInst.Rsh, LongInst.Mod,// loadOps begin
            LongInst.Lds, LongInst.Lss, LongInst.Lsb:
        {
            return InstKind.LongInst2Stack;
        }

    case LongInst.ImmAdd, LongInst.ImmSub, LongInst.ImmDiv, LongInst.ImmMul,
            LongInst.ImmEq, LongInst.ImmLt, LongInst.ImmGt, LongInst.ImmSet,
            LongInst.ImmAnd, LongInst.ImmOr, LongInst.ImmLsh, LongInst.ImmRsh, LongInst.ImmMod:
        {
            return InstKind.LongInstImm32;
        }

    }
}

enum LongInst : ushort
{
    //Former ShortInst
    Prt,
    RelJmp,
    Ret,
    Neg,
    Soh, /// set hi stack addr
    Sdh, /// set hi dataseg addr
    Flg, // writes the conditionFlag into [lw >> 16]
    Drb, /// sets db = DS[align4(SP[lw >> 16])[SP[lw >> 16] % 4]]
    Mod4, ///SP[lw >> 16] = SP[lw >> 16] & 3
    //End Former ShortInst

    Jmp,
    JmpFalse,
    JmpTrue,
    JmpZ,
    JmpNZ,

    // 2 StackOperands
    Add,
    Sub,
    Div,
    Mul,
    Eq, //sets condflags
    Lt, //sets condflags
    Gt, //sets condflags
    Set,
    And,
    Or,
    Lsh,
    Rsh,
    Mod,

    // Immidate operand
    ImmAdd,
    ImmSub,
    ImmDiv,
    ImmMul,
    ImmEq,
    ImmLt,
    ImmGt,
    ImmSet,
    ImmAnd,
    ImmOr,
    ImmLsh,
    ImmRsh,
    ImmMod,

    Lds, ///SP[hi & 0xFFFF] = DS[align4(SP[hi >> 16])]
    Lss, /// defref pointer on the stack :)
    Lsb, /// load stack byte
}
//Imm-Intructuins and corrospinding 2Operand instructions have to be in the same order
pragma(msg, LongInst.max);
static assert(LongInst.ImmAdd - LongInst.Add == LongInst.ImmRsh - LongInst.Rsh);
static assert(LongInst.ImmAnd - LongInst.And == LongInst.ImmLt - LongInst.Lt);

enum IndirectionFlagMask = ubyte(0x40); // check 7th bit

enum InstMask = ubyte(0x3F); // mask for bit 0-5
//enum CondFlagMask = ~ushort(0x2FF); // mask for 8-10th bit
enum CondFlagMask = 0b11_0000_0000;
/*
 * 64BitInst :
 * [0 .. 6] Instruction
 * [6 .. 9] Flags (* Bit 6 is true for 64BitInst
 *
 *
 */

/* Only for ImmInstructions
* Layaout :
* [0-6] Instruction
* [6-8] Flags
* *****************
* [8-12] CondFlag (or Padding)
* [12-16] Padding
* [16-32] StackOffset (lhs)
* [32-64] Imm32 (rhs) 
*/

struct LongInst64
{
    uint lw;
    uint hi;

    this(const LongInst i, BCAddr addr) pure
    {
        lw = i;
        hi = addr.addr;
    }

    this(const LongInst i, const StackAddr stackAddrLhs, const BCAddr targetAddr) pure
    {
        lw = i;
        hi = stackAddrLhs.addr | targetAddr.addr << 16;
    }

    this(const LongInst i, const StackAddr stackAddrLhs,
        const StackAddr stackAddrRhs, const bool indirect = false) pure
    {
        lw = i | indirect << 6;
        hi = stackAddrLhs.addr | stackAddrRhs.addr << 16;
    }

    this(const LongInst i, const StackAddr stackAddrLhs, const Imm32 rhs, const bool indirect = false) pure
    {
        lw = i | indirect << 6 | stackAddrLhs.addr << 16;
        hi = rhs.imm32;

    }
}

static assert(LongInst.max < 0x3F, "Instruction do not fit in 6 bit anymore");

static short isShortJump(const int offset) pure
{
    assert(offset != 0, "An Jump to the Jump itself is invalid");

    const bool wasNegative = (offset < 0);
    int abs_offset = wasNegative ? offset * -1 : offset;

    if (abs_offset < (1 << 15))
    {
        return (cast(ushort)(wasNegative ? abs_offset *= -1 : abs_offset));
    }
    else
    {
        return 0;
    }
}

static const(uint) align4(const uint val) pure
{
    return ((val + 3) & ~0b11);
}

static assert(align4(1) == 4);
static assert(align4(9) == 12);
static assert(align4(11) == 12);
static assert(align4(12) == 12);
static assert(align4(15) == 16);

uint ShortInst16(const LongInst i, const short imm, const bool indirect = false) pure
{
    return i | indirect << 6 | imm << 16;
}

uint ShortInst16Ex(const LongInst i, ubyte ex, const short imm) pure
{
    return i | ex << 8 | imm << 16;
}

uint ShortInst24(const LongInst i, const uint imm) pure
{
    assert(imm == (imm & 0xFFFFFF));
    assert(i == LongInst.Soh || i == LongInst.Sdh);
    return i | imm << 8;
}

enum BCTypeEnum : ubyte
{
    undef,

    Void,

    Ptr,
    i32Ptr,

    Char,
    i1,

    i8,
    i16,
    i32,
    i64,
    //  everything below here is not used by the bc layer.
    String,
    Array,
    Struct, //Slice,

}

struct BCType
{
    BCTypeEnum type;
    alias type this;
    /// 0 means basic type
    uint typeIndex;
}

const(uint) basicTypeSize(const BCTypeEnum bct) pure
{
    final switch (bct) with (BCTypeEnum)
    {

    case undef, Void:
        {
            assert(0, "We should never encounter undef or Void");
        }
    case i32Ptr, Ptr:
        {
            //TODO add 64bit mode
            return  /* m64 ? 8 :*/ 4;
        }
    case String:
        {
            //TODO add 64bit mode
            return  /* m64 ? 16 :*/ 4;
        }

    case i1:
        {
            return 1;
        }
    case Char, i8:
        {
            return 1;
        }
    case i16:
        {
            return 2;
        }
    case i32:
        {
            return 4;
        }
    case i64:
        {
            return 8;
        }

    case Array, Struct:
        {
            return 0;
        }
    }
}

struct BCSlice
{
    BCTypeEnum elem;
    uint length;
}

enum BCValueType : ubyte
{
    Unknown,
    DataSegValue,
    //  Temporary,
    StackValue,
    Immidiate,
}

struct BCValue
{
    BCValueType vType;
    BCType type;

    ///default value 0 - meaning BuiltinType

    union
    {
        StackAddr stackAddr;
        DataSegAddr dataSegAddr;
        Imm32 imm32;
    }

    union
    {
        void* valAddr;

        //BCSlice* slice;

        ubyte* i8;
        ushort* i16;
        uint* i32;
        ulong* i64;
        //ulong imm64;

    }

    bool opCast(T : bool)()
    {
        return this.vType != vType.Unknown;
    }

    bool opEquals(const BCValue rhs) pure const
    {
        if (this.vType == rhs.vType && this.type.type == rhs.type)
        {
            final switch (this.vType)
            {
            case BCValueType.StackValue:
                return this.stackAddr == rhs.stackAddr;

            case BCValueType.Immidiate:
                switch (this.type)
                {
                case BCTypeEnum.i32:
                    {
                        return imm32.imm32 == rhs.imm32.imm32;
                    }
                    /*case BCTypeEnum.i64:
                    {
                        return imm64 == rhs.imm64;
                    }*/
                default:
                    assert(0, "No comperasion for immidiate");
                }
            case BCValueType.DataSegValue:
                return this.dataSegAddr == rhs.dataSegAddr;

            case BCValueType.Unknown:
                return false;
            }

        }

        return false;
    }

    string toString() const pure
    {
        import std.format : format;

        return format("\nvType: %s\tType: %s\tstackAddr: %s\timm32 %s\t",
            vType, type.type, stackAddr, imm32);
        // return "";
    }

    this(Imm32 imm32) pure
    {
        this.type.type = BCTypeEnum.i32;
        this.vType = BCValueType.Immidiate;
        this.imm32 = imm32;
    }

    /*    this(Imm64 value, BCTypeEnum type) pure
    {
        this.vType = BCValueType.Immidiate;
        this.type.type = type;
        imm64 = value;
    } */

    this(StackAddr sp, BCType type) pure
    {
        this.vType = BCValueType.StackValue;
        this.stackAddr = sp;
        this.type = type;
    }

    this(void* base, short addr, BCType type) pure
    {
        this.vType = BCValueType.StackValue;
        this.stackAddr = StackAddr(addr);
        this.type = type;
        if (!__ctfe)
            this.valAddr = base + addr;
    }
}

pragma(msg, "Sizeof BCValue: ", BCValue.sizeof);
__gshared static bcOne = BCValue(Imm32(1));

struct BCAddr
{
    uint addr;
    alias addr this;
}

struct BCLabel
{
    BCAddr addr;
}

struct BCBlock
{
    bool opCast(T : bool)()
    {
        // since 0 is an invalid address it is enough to check if begin is 0
        return !!begin.addr.addr;
    }

    BCLabel begin;
    BCLabel end;
}

struct DataSegAddr
{
    uint addr;
    alias addr this;
}

struct StackAddr
{
    short addr;
    alias addr this;
}

struct Imm32
{
    uint imm32;
    alias imm32 this;
}

struct Branch
{
    BCValue cond;
    BCLabel ifTrue;
    BCLabel ifFalse;
}

import core.bitop : bsf;

static assert(bsf(0xFF) == 0);
static assert(bsf(0x20) == 5);

bool isBasicBCType(BCTypeEnum bct)
{
    return !(bct == BCTypeEnum.Struct || bct == BCTypeEnum.Array);
}

struct BCGen
{
    //SharedState 
    // The following is shared between ALL BC generator instances in a given run

    /////////////////////////////////////////////

    int[ushort.max] byteCodeArray;
    /// ip starts at 4 because 0 should be an invalid address;
    BCAddr ip = BCAddr(4);
    StackAddr sp = StackAddr(4);

    BCLabel[ubyte.max] unresolvedLabels;
    BCTemporary[ubyte.max] temporarys;

    uint conditionsUsedFlags = 0b11111;

    ubyte unreslovedLabelCount;
    ubyte temporaryCount;

    struct BCTemporary
    {
        BCValue value;
        ubyte tmpIdx;
    }

    void emitLongInst(LongInst64 i)
    {
        byteCodeArray[ip] = i.lw;
        byteCodeArray[ip + 1] = i.hi;
        ip += 2;
    }

    BCTemporary genTemporary(BCType bct)
    {
        auto tmp = temporarys[temporaryCount] = BCTemporary(BCValue(StackAddr(sp),
            bct), temporaryCount);
        //make BCTypeEnum a struct
        //FIXE make the typeIndex a part of BCTypeEnum. 

        sp += align4(isBasicBCType(bct) ? basicTypeSize(bct) : 0); //sharedState.size(bct.type, typeIndex));
        ++temporaryCount;
        return tmp;
    }

    //  pragma(msg, (0xFF ^ 1 << 0x7));
    /+
    BCValue genCondFlag()
    {
        if (conditionsUsedFlags)
        {
            auto firstFreeFlag = bsf(conditionsUsedFlags);
            assert(firstFreeFlag < BCCondFlag.max + 1, "Too many conditions used");
            conditionsUsedFlags ^= (1 << firstFreeFlag); // toggle the first free flag to used;
            return BCValue(BCCondFlag(cast(BCCondFlag.BCCondFlagEnum) firstFreeFlag));
        }
        else
        {
            assert(0, "You seem to have unset more then the maximum nuber of ConditionCountFlags");
        }
    }
+/
    BCAddr beginJmp()
    {
        BCAddr atIp = ip;
        ip += 2;
        return atIp;
    }

    void endJmp(BCAddr atIp, BCLabel target)
    {
        if (auto offset = isShortJump(target.addr - atIp))
        {
            byteCodeArray[atIp] = ShortInst16(LongInst.RelJmp, offset);
        }
        else
        {
            LongInst64 lj = LongInst64(LongInst.Jmp, target.addr);
            byteCodeArray[atIp] = lj.lw;
            byteCodeArray[atIp + 1] = lj.hi;
        }
    }

    void emitPrt(BCValue val)
    {
        assert(val.vType == BCValueType.StackValue);
        byteCodeArray[ip] = ShortInst16(LongInst.Prt, val.stackAddr);
        ip += 2;
    }

    BCLabel genLabel()
    {
        return BCLabel(ip);
    }

    BCAddr beginCndJmp()
    {
        auto at = ip;
        ip += 2;
        return at;
    }

    void endCndJmp(BCAddr atIp, BCLabel target, bool ifTrue = false, BCValue cond = BCValue.init)
    {
        LongInst64 lj;
        if (cond.vType == BCValueType.StackValue && cond.type == BCType.i32)
        {
            lj = (ifTrue ? LongInst64(LongInst.JmpNZ, cond.stackAddr,
                target.addr) : LongInst64(LongInst.JmpZ, cond.stackAddr, target.addr));
        }
        else
        {
            lj = (ifTrue ? LongInst64(LongInst.JmpTrue,
                target.addr) : LongInst64(LongInst.JmpFalse, target.addr));
        }

        byteCodeArray[atIp] = lj.lw;
        byteCodeArray[atIp + 1] = lj.hi;
    }

    BCAddr beginJmpZ()
    {
        auto at = ip;
        ip += 2;
        return at;
    }

    void endJmpZ(BCAddr atIp, StackAddr stackAddr, BCLabel target)
    {
        LongInst64 lj = LongInst64(LongInst.JmpZ, stackAddr, target.addr);
        byteCodeArray[atIp] = lj.lw;
        byteCodeArray[atIp + 1] = lj.hi;
    }

    BCLabel* unresolvedLabel()
    {
        return &unresolvedLabels[unreslovedLabelCount++];
    }

    BCAddr genJump(BCLabel target)
    {
        assert(target.addr);
        auto at = beginJmp();
        endJmp(at, target);
        return at;
    }

    void emitFlg(BCValue lhs)
    {
        assert(lhs.vType == BCValueType.StackValue, "Can only store flags in Stack Values");
        byteCodeArray[ip] = ShortInst16(LongInst.Flg, lhs.stackAddr.addr);
        ip += 2;
    }

    void emitArithInstruction(LongInst inst, BCValue lhs, BCValue rhs)
    {
        assert(inst >= LongInst.Add && inst < LongInst.ImmAdd,
            "Instruction is not in Range for Arith Instructions");
        assert(lhs.vType.StackValue, "only StackValues are supported as lhs");
        assert(lhs.type == BCTypeEnum.i32 || lhs.type == BCTypeEnum.i32Ptr,
            "only i32 or i32Ptr is supported for now not: " ~ to!string(lhs.type.type));
        assert(rhs.type == BCTypeEnum.i32,
            "only i32 is supported for now, not: " ~ to!string(rhs.type.type));

        immutable bool isIndirect = lhs.type == BCTypeEnum.i32Ptr;
        if (rhs.vType == BCValueType.Immidiate)
        {
            //Change the instruction into the corrosponding Imm Instruction;
            inst += (LongInst.ImmAdd - LongInst.Add);
            emitLongInst(LongInst64(inst, lhs.stackAddr, rhs.imm32, isIndirect));
        }
        else if (rhs.vType == BCValueType.StackValue)
        {
            emitLongInst(LongInst64(inst, lhs.stackAddr, rhs.stackAddr, isIndirect));
        }
        else
        {
            assert(0);
        }
    }

    void emitSet(BCValue lhs, BCValue rhs)
    {
        emitArithInstruction(LongInst.Set, lhs, rhs);
    }

    BCValue Lt3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType == BCValueType.Unknown
            || result.vType == BCValueType.StackValue,
            "The result for this must be Empty or a StackValue");
        emitArithInstruction(LongInst.Lt, lhs, rhs);
        if (result.vType == BCValueType.StackValue)
        {
            emitFlg(result);
        }
        return result;
    }

    BCValue Gt3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType == BCValueType.Unknown
            || result.vType == BCValueType.StackValue,
            "The result for this must be Empty or a StackValue");
        emitArithInstruction(LongInst.Gt, lhs, rhs);
        if (result.vType == BCValueType.StackValue)
        {
            emitFlg(result);
        }
        return result;
    }

    BCValue Eq3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType == BCValueType.Unknown
            || result.vType == BCValueType.StackValue,
            "The result for this must be Empty or a StackValue");
        emitArithInstruction(LongInst.Eq, lhs, rhs);
        if (result.vType == BCValueType.StackValue)
        {
            emitFlg(result);
        }
        return result;
    }

    BCValue Add3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot add to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Add, result, rhs);
        return result;
    }

    BCValue Sub3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot sub to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Sub, result, rhs);

        return result;
    }

    BCValue Mul3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot mul to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Mul, result, rhs);
        return result;
    }

    BCValue Div3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot div to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Div, result, rhs);
        return result;
    }

    BCValue And3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot and to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.And, result, rhs);
        return result;
    }

    BCValue Or3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot or to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Or, result, rhs);
        return result;
    }

    BCValue Lsh3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot lsh to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Lsh, result, rhs);
        return result;
    }

    BCValue Rsh3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot rsh to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Rsh, result, rhs);
        return result;
    }

    BCValue Mod3(BCValue result, BCValue lhs, BCValue rhs)
    {
        assert(result.vType != BCValueType.Immidiate, "Cannot and to Immidiate");

        result = (result ? result : lhs);
        if (lhs != result)
        {
            emitSet(result, lhs);
        }
        emitArithInstruction(LongInst.Mod, result, rhs);
        return result;
    }

    BCValue pushOntoStack(BCValue val)
    {
        assert(val.type == BCTypeEnum.i32);
        if (val.vType != BCValueType.StackValue)
        {
            auto stackref = BCValue(null, sp, val.type);
            emitSet(stackref, val);

            sp += align4(basicTypeSize(val.type));
            return stackref;
        }
        else
        {
            return val;
        }
    }

    void emitReturn(BCValue val)
    {
        assert(val.vType == BCValueType.StackValue); // {
        if (!__ctfe)
            debug (bc)
            {
                import std.stdio;

                writeln(val.type == BCTypeEnum.i32Ptr);
            }
        byteCodeArray[ip] = ShortInst16(LongInst.Ret, val.stackAddr, val.type == BCTypeEnum.i32Ptr);
        ip += 2;
        /*} else if (val.vType == BCValueType.Immidiate) {
                        auto sv = pushOntoStack(val);
                        assert(sv.vType == BCValueType.StackValue);
                        byteCodeArray[ip] = ShortInst16(LongInst.Ret, sv.stackAddr);
                        ip += 2;
                }*/
    }

}

string printInstructions(int[] arr)
{
    return printInstructions(arr.ptr, cast(uint) arr.length);
}

string printInstructions(int* startInstructions, uint length)
{

    string result = "StartInstructionDump: \n";
    uint pos = 0;
    import std.conv;

    bool has4ByteOffset;
    if (length > 4 && startInstructions[0 .. 4] == [0, 0, 0, 0])
    {
        has4ByteOffset = true;
        length -= 4;
        startInstructions += 4;
        //pos += 4;
    }

    result ~= "Length : " ~ to!string(length) ~ "\n";
    auto arr = startInstructions[0 .. length];

    while (length--)
    {
        uint lw = arr[pos];
        result ~= pos.to!string ~ ":\t" ~ (!!(lw & IndirectionFlagMask) ? "* " : "");
        ++pos;
        if (lw == 0)
        {
            result ~= "0x0 0x0 0x0 0x0\n";
            continue;
        }

        // We have a long instruction

        --length;
        uint hi = arr[pos++];

        final switch (cast(LongInst)(lw & InstMask))
        {
        case LongInst.ImmSet:
            {
                result ~= "Set SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmAdd:
            {
                result ~= "Add SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmSub:
            {
                result ~= "Sub SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmMul:
            {
                result ~= "Mul SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmDiv:
            {
                result ~= "Div SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;

        case LongInst.ImmAnd:
            {
                result ~= "And SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmOr:
            {
                result ~= "Or SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;

        case LongInst.ImmLsh:
            {
                result ~= "Lsh SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmRsh:
            {
                result ~= "Rsh SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;

        case LongInst.ImmMod:
            {
                result ~= "Mod SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;

        case LongInst.ImmEq:
            {
                result ~= "Eq SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmLt:
            {
                result ~= "Lt SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.ImmGt:
            {
                result ~= "Gt SP[" ~ to!string(lw >> 16) ~ "], #" ~ to!string(hi) ~ "\n";
            }
            break;
        case LongInst.Add:
            {
                result ~= "Add SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Sub:
            {
                result ~= "Sub SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Mul:
            {
                result ~= "Mul SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Div:
            {
                result ~= "Div SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.And:
            {
                result ~= "And SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Or:
            {
                result ~= "Or SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Lsh:
            {
                result ~= "Lsh SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Rsh:
            {
                result ~= "Rsh SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Mod:
            {
                result ~= "Mod SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Eq:
            {
                result ~= "Eq SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;

        case LongInst.Set:
            {
                result ~= "Set SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;

        case LongInst.Lt:
            {
                result ~= "Lt SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Gt:
            {
                result ~= "Gt SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Jmp:
            {
                result ~= "Jmp &" ~ to!string(hi) ~ "\n";
            }
            break;

        case LongInst.JmpFalse:
            {
                result ~= "JmpFalse &" ~ to!string((has4ByteOffset ? hi - 4 : hi)) ~ "\n";
            }
            break;
        case LongInst.JmpTrue:
            {
                result ~= "JmpTrue &" ~ to!string((has4ByteOffset ? hi - 4 : hi)) ~ "\n";
            }
            break;

        case LongInst.JmpNZ:
            {
                result ~= "JmpNZ SP[" ~ to!string(hi & 0xFFFF) ~ "], &" ~ to!string(
                    (has4ByteOffset ? (hi >> 16) - 4 : hi >> 16)) ~ "\n";
            }
            break;

        case LongInst.JmpZ:
            {
                result ~= "JmpZ SP[" ~ to!string(hi & 0xFFFF) ~ "], &" ~ to!string(
                    (has4ByteOffset ? (hi >> 16) - 4 : hi >> 16)) ~ "\n";
            }
            break;

        case LongInst.Lds:
            {
                result ~= "Lds SP[" ~ to!string(hi & 0xFFFF) ~ "], DS[align4(SP[" ~ to!string(
                    hi >> 16) ~ "])]\n";
            }
            break;
        case LongInst.Lss:
            {
                result ~= "Lss SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;
        case LongInst.Lsb:
            {
                result ~= "Lsb SP[" ~ to!string(hi & 0xFFFF) ~ "], SP[" ~ to!string(hi >> 16) ~ "]\n";
            }
            break;

            // We have a short instruction

        case LongInst.Ret:
            {
                result ~= "Ret SP[" ~ to!string(lw >> 16) ~ "] \n";
            }
            break;
        case LongInst.RelJmp:
            {
                result ~= "RelJmp &" ~ to!string(cast(short)(lw >> 16) + (pos - 1)) ~ "\n";
            }
            break;
        case LongInst.Prt:
            {
                result ~= "Prt SP[" ~ to!string(lw >> 16) ~ "] \n";
            }
            break;
        case LongInst.Soh: // Set StackOffset high
        {
                result ~= "Soh #" ~ to!string((lw & ~0xFF)) ~ "\n";
            }
            break;
        case LongInst.Sdh: // Set DataSegmentOffset high
        {
                result ~= "Sdh #" ~ to!string((lw & ~0xFF)) ~ "\n";
            }
            break;
        case LongInst.Neg:
            {
                result ~= "Neg SP[" ~ to!string(lw >> 16) ~ "] \n";
            }
            break;

        case LongInst.Mod4:
            {
                result ~= "Mod4 SP[" ~ to!string(lw >> 16) ~ "] \n";
            }
            break;
        case LongInst.Flg:
            {
                result ~= "Flg SP[" ~ to!string(lw >> 16) ~ "] \n";
            }
            break;

        case LongInst.Drb:
            result ~= "Drb" ~ to!string(
                (lw & 0x0F00) >> 8) ~ " SP[" ~ to!string(cast(short)(lw >> 16)) ~ "]" ~ "\n";
            //                      ((lw & ~0xFF) << 24);
        }

    }
    return result ~ "\n EndInstructionDump";
}

/+string genToParentEnum(P, E)()
{
    assert(__ctfe, "This only makes sense at CTFE");

    string result = " " ~ P.stringof ~ " toParentEnum (const " ~ E.stringof ~ " e) const pure {
        \tfinal switch(e) with (typeof(e)) {\n";
    import std.format : format;

    foreach (M; __traits(allMembers, E))
    {
        result ~= format("\t\tcase %s : return %s.%s;\n", M, P.stringof, M);
    }
    return result ~ "\n\t}\n}";
}

mixin template mxToParentEnum(P, E)
{
    static assert(isIncluded!(E, P));
    mixin(genToParentEnum!(P, E)());
    pragma(msg, (genToParentEnum!(P, E)()));
}
/**
 * checks if ThisEnum is contained in ThatEnum
 */
template isIncluded(ThisEnum, ThatEnum)
{
    import std.traits : EnumMembers, allSatisfy;

    enum emThisEnum = EnumMembers!ThisEnum;
    enum emThatEnum = EnumMembers!ThatEnum;
    static assert(is(ThisEnum == enum) && is(ThatEnum == enum), "IsIncluded works only for enums");

    enum isIncluded = emThisEnum.length <= emThatEnum.length && hasMembers();

    bool hasMembers()
    {
        foreach (M; __traits(allMembers, ThisEnum))
        {
            static if (!is(typeof(mixin(`ThatEnum.` ~ M))))
            {
                static assert(0, "member: " ~ M ~ " is not included");
            }
        }
        return true;
    }
}
+/
uint interpret(const int[] byteCode, const BCValue[] args,
    const uint* dataSeg = null, void* ctfeState = null)
{
    import std.conv;

    auto stack = new uint[](ushort.max / 4);
    stack.length = ushort.max / 4; // just to make sure;

    ulong stackOffset = 4; /// 48bit wide stack offset
    ulong dataSegHigh = 0; /// 56bit wide of the dataSegOffset
    uint drOffset = 0; /// data registerOffset

    uint dr; ///dataRegister holds the value of dataDeg[dataSegHigh | drOffset]
    ubyte db; /// current dataByte a slice of the dataRegister with range [0 .. 4]

    // first push the args on
    debug (bc)
        if (!__ctfe)
        {
            import std.stdio;

            writeln("before pushing args");
        }
    foreach (arg; args)
    {
        switch (arg.type.type)
        {
        case BCTypeEnum.i32:
            {
                *(stack.ptr + (stackOffset / 4)) = arg.imm32;
                stackOffset += uint.sizeof;
            }
            break;
        case BCTypeEnum.i64:
            {
                pragma(msg, "I treat ulongs as uints ... beware");
                *(stack.ptr + (stackOffset / 4)) = arg.imm32;
                stackOffset += uint.sizeof;
            }
            break;
        case BCTypeEnum.Struct, BCTypeEnum.String, BCTypeEnum.Array:
            {

            }
            break;
        default:
            //return -1;
            //       assert(0, "unsupported Type " ~ to!string(arg.type));
        }
    }

    uint ip = 4;
    bool cond;

    // debug(bc) { import std.stdio; writeln("BC.len = ", byteCode.length); }
    if (byteCode.length == 0 || byteCode.length == 1)
        return typeof(return).init;

    while (true)
    {
        import std.range;

        foreach (si; 0 .. stackOffset)
        {
            if (!__ctfe)
                printf("%d : %x".ptr, si, stack[cast(uint) si]);
        }
        //writeln(stack[0 ..)
        if (ip >= byteCode.length-1)
        {
            return -1;
        }
        const lw = byteCode[ip++];
        const hi = byteCode[ip++];
       

        bool indirect = !!(lw & IndirectionFlagMask);

        debug (bc)
            if (!__ctfe)
            {
                import std.stdio;

                writeln("indirect: ", indirect);
            }

        auto lhsOffset = hi & 0xFFFF;
        auto rhsOffset = (hi >> 16);
        uint* lhsRef = (stack.ptr + (lhsOffset / 4));
        uint rhs = *(stack.ptr + (rhsOffset / 4));
        uint* lhsStackRef = (stack.ptr + ((lw >> 16) / 4));
        auto opRef = stack.ptr + ((lw >> 16) / 4);

        if (indirect)
        {
            lhsStackRef = (stack.ptr + ((*lhsStackRef) / 4));
            lhsRef = (stack.ptr + ((*lhsRef) / 4));
            opRef = (stack.ptr + ((*opRef) / 4));

        }

        final switch (cast(LongInst)(lw & InstMask))
        {
        case LongInst.ImmAdd:
            {
                (*lhsStackRef) += hi;
            }
            break;

        case LongInst.ImmSub:
            {
                (*lhsStackRef) -= hi;
            }
            break;

        case LongInst.ImmMul:
            {
                (*lhsStackRef) *= hi;
            }
            break;

        case LongInst.ImmDiv:
            {
                (*lhsStackRef) /= hi;
            }
            break;

        case LongInst.ImmAnd:
            {
                (*lhsStackRef) &= hi;
            }
            break;
        case LongInst.ImmOr:
            {
                (*lhsStackRef) |= hi;
            }
            break;

        case LongInst.ImmLsh:
            {
                (*lhsStackRef) <<= hi;
            }
            break;
        case LongInst.ImmRsh:
            {
                (*lhsStackRef) >>= hi;
            }
            break;

        case LongInst.ImmMod:
            {
                (*lhsStackRef) %= hi;
            }
            break;

        case LongInst.ImmSet:
            {
                (*lhsStackRef) = hi;
            }
            break;
        case LongInst.ImmEq:
            {

                if ((*lhsStackRef) == hi)
                {
                    cond = true;
                }
                else
                {
                    cond = false;
                }

            }
            break;
        case LongInst.ImmLt:
            {
                if ((*lhsStackRef) < hi)
                {
                    cond = true;
                }
                else
                {
                    cond = false;
                }

            }
            break;
        case LongInst.ImmGt:
            {
                if ((*lhsStackRef) > hi)
                {
                    cond = true;
                }
                else
                {
                    cond = false;
                }

            }
            break;
        case LongInst.Add:
            {
                (*lhsRef) += rhs;
            }
            break;
        case LongInst.Sub:
            {
                (*lhsRef) -= rhs;
            }
            break;
        case LongInst.Mul:
            {
                (*lhsRef) *= rhs;
            }
            break;
        case LongInst.Div:
            {
                (*lhsRef) /= rhs;
            }
            break;
        case LongInst.And:
            {
                (*lhsRef) &= rhs;
            }
            break;
        case LongInst.Or:
            {
                (*lhsRef) |= rhs;
            }
            break;

        case LongInst.Lsh:
            {

                (*lhsRef) <<= rhs;
            }
            break;
        case LongInst.Rsh:
            {
                (*lhsRef) >>= rhs;
            }
            break;

        case LongInst.Mod:
            {
                (*lhsRef) %= rhs;
            }
            break;

        case LongInst.Eq:
            {
                if ((*lhsRef) == rhs)
                {
                    cond = true;
                }
                else
                {
                    cond = false;
                }

            }
            break;

        case LongInst.Set:
            {
                (*lhsRef) = rhs;
            }
            break;

        case LongInst.Lt:
            {
                if ((*lhsRef) < rhs)
                {
                    cond = true;
                }
                else
                {
                    cond = false;
                }

            }
            break;
        case LongInst.Gt:
            {
                if ((*lhsRef) > rhs)
                {
                    cond = true;
                }
                else
                {
                    cond = false;
                }

            }
            break;

        case LongInst.Jmp:
            {
                ip = hi;
            }
            break;
        case LongInst.JmpNZ:
            {
                if ((*lhsRef) != 0)
                {
                    ip = rhsOffset;
                }
            }
            break;
        case LongInst.JmpZ:
            {
                if ((*lhsRef) == 0)
                {
                    ip = rhsOffset;
                }
            }
            break;
        case LongInst.JmpFalse:
            {
                if (!cond)
                {
                    ip = (hi);
                }
            }
            break;
        case LongInst.JmpTrue:
            {
                if (cond)
                {
                    ip = (hi);
                }
            }
            break;

        case LongInst.Lds:
            {
                (*lhsRef) = *(dataSeg + (dataSegHigh | rhs));
                //{ SP[hi & 0xFFFF] = DS[align4(SP[hi >> 16])] }
            }
            break;
        case LongInst.Lss:
            {
                (*lhsRef) = *(stack.ptr + (rhs / 4));
                //{ SP[hi & 0xFFFF] = DS[align4(SP[hi >> 16])] }
            }
            break;
        case LongInst.Lsb:
            {

                uint _dr = *(stack.ptr + (rhs / 4));
                debug (bc)
                {
                    import std.stdio;

                    writeln("Lsb_ _dr:", _dr, "  rhs: ", rhs, " rhs/4: ",
                        rhs / 4, " rhs & 3: ", rhs & 3);
                }
                final switch (rhs & 3)
                {
                case 0:
                    (*lhsRef) = _dr & 0xFF;
                    break;
                case 1:
                    (*lhsRef) = (_dr & 0xFF00) >> 8;
                    break;
                case 2:
                    (*lhsRef) = (_dr & 0xFF0000) >> 16;
                    break;
                case 3:
                    (*lhsRef) = _dr >> 24;
                    break;
                }

            }
            break;

            // We have a short instruction

        case LongInst.Ret:
            {
                debug (bc)
                    if (!__ctfe)
                    {
                        printf("Ret: %d".ptr, (*opRef));
                    }
                return *opRef;
            }

        case LongInst.RelJmp:
            {
                ip += (cast(short)(lw >> 16)) - 1;
            }
            break;
        case LongInst.Neg:
            {

                (*opRef) = -(*opRef);
            }
            break;
        case LongInst.Prt:
            {
                if (!__ctfe)
                {
                    printf("SP[%d](%d)".ptr, (lw >> 16), (*opRef));
                }
            }
            break;
        case LongInst.Soh:
            {
                stackOffset = (lw & ~0xFF) << 24;
            }
            break;
        case LongInst.Sdh:
            {
                dataSegHigh = ulong(lw & ~0xFF) << 32L;
            }
            break;
        case LongInst.Drb:
            const DsIdx = *(stack.ptr + ((lw >> 16) / 4));
            const alignedIdx = (DsIdx & ~3);
            if (drOffset != alignedIdx)
            {
                dr = *(dataSeg + (dataSegHigh | alignedIdx));
                drOffset = alignedIdx;
            }
            final switch (DsIdx & 3)
            {
            case 0:
                db = dr & 0x00FF;
                break;
            case 1:
                db = (dr & 0xFF00) >> 8;
                break;
            case 2:
                db = (dr & 0xFF0000) >> 16;
                break;
            case 3:
                db = dr >> 24;
                break;
            }
            break;
        case LongInst.Flg:
            {
                (*opRef) = cond;
            }
            break;

        case LongInst.Mod4:
            {
                (*opRef) &= 3;
            }
            break;
        }
    }
}

int[] testArith()
{
    BCGen gen;
    auto one = BCValue(Imm32(1));

    auto two = BCValue(Imm32(2));
    auto sixteen = BCValue(Imm32(16));
    auto four = BCValue(Imm32(4));

    auto result = gen.genTemporary(BCType(BCTypeEnum.i32));

    gen.Mul3(result.value, two, sixteen);
    gen.Div3(result.value, result.value, four);
    gen.Sub3(result.value, result.value, one);
    gen.emitReturn(result.value);
    return (gen.byteCodeArray[0 .. gen.ip].dup);
}

int[] testLt()
{
    BCGen gen;
    with (gen)
    {
        auto p1 = BCValue(StackAddr(4), BCType(BCTypeEnum.i32)); //first parameter gets push on here
        auto p2 = BCValue(StackAddr(8), BCType(BCTypeEnum.i32)); //the second goes here
        sp += 8;
        // we dont want to overwrite our parameters;
        BCValue result = genTemporary(BCType(BCTypeEnum.i32)).value;
        auto eval_label = genLabel();
        emitPrt(p1);
        emitPrt(p2);
        Lt3(BCValue.init, p1, p2);
        auto jnt = beginCndJmp();
        emitSet(result, BCValue(Imm32(1)));
        auto toReturn = beginJmp();
        auto ifFalse = genLabel();
        emitSet(result, (BCValue(Imm32(0))));
        endCndJmp(jnt, ifFalse);
        endJmp(toReturn, genLabel());
        emitReturn(result);
    }

    return (gen.byteCodeArray[0 .. gen.ip].dup);
}

int[] testBC()
{
    BCGen gen;
    with (gen)
    {
        auto p1 = BCValue(StackAddr(4), BCType(BCTypeEnum.i32));
        sp += 4;
        auto cond = Eq3(BCValue.init, p1, BCValue(Imm32(16)));
        auto cndJmp = beginCndJmp();
        emitReturn(p1);

        auto target = genLabel();

        auto result = Mul3(p1, p1, BCValue(Imm32(4)));

        result = Div3(result, result, BCValue(Imm32(2)));
        emitReturn(p1);

        endCndJmp(cndJmp, target);

        return byteCodeArray[0 .. ip].dup;
    }
}

int[] testDs()
{
    BCGen gen;
    with (gen)
    {
        auto p1 = BCValue(StackAddr(4), BCType(BCTypeEnum.i32));
        sp += 4;

        auto result = genTemporary(BCType(BCTypeEnum.i32)).value;

        emitLongInst(LongInst64(LongInst.Lds, result.stackAddr, p1.stackAddr)); // *lhsRef = DS[aligin4(rhs)]

        emitReturn(result); // return result;

        return byteCodeArray[0 .. ip].dup;
    }

}

int[] testLss()
{
    BCGen gen;
    with (gen)
    {
        emitSet(BCValue(StackAddr(4), BCType(BCTypeEnum.i32)), BCValue(Imm32(8)));
        emitSet(BCValue(StackAddr(8), BCType(BCTypeEnum.i32)), BCValue(Imm32(32)));
        sp += 8;

        auto result = BCValue(StackAddr(4) ,BCType(BCTypeEnum.i32Ptr));

        //emitLongInst(LongInst64(LongInst.Lss, result.stackAddr, p1.stackAddr)); // *lhsRef = DS[aligin4(rhs)]

        emitReturn(result); // return result;

        return byteCodeArray[0 .. ip].dup;
    }

}

static assert(interpret(testArith(), []) == 7);
//pragma(msg, testDs.printInstructions);

static assert(interpret(testLt(), [BCValue(Imm32(21)), BCValue(Imm32(25))]));
static assert(!interpret(testLt(), [BCValue(Imm32(27)), BCValue(Imm32(25))]));
static assert(!interpret(testLt(), [BCValue(Imm32(25)), BCValue(Imm32(25))]));

//pragma(msg, testLt.printInstructions);

static assert(interpret(testBC(), [BCValue(Imm32(12))]) == 24);
static assert(interpret(testBC(), [BCValue(Imm32(16))]) == 16);

//pragma(msg, testBC.printInstructions);

static assert(cast(dchar) testDs.interpret([BCValue(Imm32(1))],
    (cast(uint[]) "hello"d).ptr) == "e"[0]);

//pragma(msg, printInstructions(testLss));
static assert (interpret(testLss(), []) == 32);