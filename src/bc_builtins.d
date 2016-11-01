module ddmd.ctfe.bc_builtins;
/**
 This module the ctfe-versions of bultin-functions
 Such as String-Iteration functions and the like

 They are _NOT_ written as generic bytecode that a backend may execute however it wishes
 This was a pretty bad idea.
*/

import ddmd.ctfe.bc_common;


const (uint) intrin_Byte3(const uint word, int idx) pure @safe @nogc {
    switch(idx) {
        case 0 :
            return word & 0xFF;
        case 1 :
            return (word & 0xFF00) >> 8;
        case 2 :
            return (word & 0xFF0000) >> 16;
        case 3 :
            return (word & 0xFF000000) >> 24;
        default : assert(0, "index must go from 0 to 3");
    }
}


alias BCBuilitin_NextUTF8CharT = BCValue function(const BCHeap* heapPtr, int* argPtr) pure;
alias BCBuilitin_StringConcatT = BCValue function(BCHeap* heapPtr, int* resultPtr, const int* lhsPtr, const int* rhsPtr) pure;

enum BCBuiltin
{
    StringCat,
    NextChar,
}

static immutable BCBuilitin_StringConcatT stringCat = (BCHeap* heapPtr, int* resultPtr, const int* lhsPtr, const int* rhsPtr) pure @safe {
    {
        const result = BCValue(Imm32(heapPtr.heapSize));
        HeapAddr lhs = HeapAddr(*lhsPtr);
        HeapAddr rhs = HeapAddr(*lhsPtr);

        const lhsLength = heapPtr._heap[lhs.addr++];
        const rhsLength = heapPtr._heap[rhs.addr++];

        auto effectiveSize = ((lhsLength + rhsLength) / 4) + 1;

        foreach(i;0 .. lhsLength/4)
        {
            heapPtr._heap[result.imm32 + i] =  heapPtr._heap[lhs.addr + i];
        }

        if (auto rest = lhsLength % 4)
        {
            foreach(i;0..rest)
            {
                import ddmd.ctfe.bc_builtins : intrin_Byte3;
                heapPtr._heap[result.imm32 + lhsLength/4] |= (intrin_Byte3(heapPtr._heap[lhsLength/4], i) << (8*i));
            }
            const effective_start = (result.imm32 + lhsLength/4);
            foreach(i;0 .. rhsLength)
            {
                const effictive_offset = (rest + i);
                heapPtr._heap[effective_start + effictive_offset/4] |= (intrin_Byte3(heapPtr._heap[effictive_offset/4], effictive_offset%4) << (8*(effictive_offset%4)));
            }

        }
        else
        {
            const start = result.imm32 + lhsLength/4;
            foreach(i;0 .. rhsLength/4)
            {
                heapPtr._heap[start + i] =  heapPtr._heap[rhs.addr + i];
            }

            if (auto rest = rhsLength % 4)
            {
                foreach(i;0..rest)
                {
                    import ddmd.ctfe.bc_builtins : intrin_Byte3;
                    heapPtr._heap[start + lhsLength] |= (intrin_Byte3(heapPtr._heap[rhsLength/4], i) << (8*i));
                }
            }
        }

        heapPtr.heapSize += effectiveSize;
        debug (ctfe) if (!__ctfe)
        {
            import std.c.string : strlen;
            import std.stdio;
            () @trusted {assert(strlen(cast(char*)(cast(uint*)(heapPtr._heap.ptr + result.imm32.imm32))) == lhsLength + rhsLength);
                writeln(cast(char*)(cast(uint*)(heapPtr._heap.ptr + result.imm32.imm32 + 1))[0 ..(lhsLength + rhsLength)]);
            }();
        }
        return(result);
    }
};

/**
     BCValue NextUTF8Char(BCValue stringPtr)
*/
static immutable BCBuilitin_NextUTF8CharT nextChar = (const BCHeap* heapPtr, int* argPtr) pure @safe
{
    uint stringPos = *argPtr;
    uint stringPosOverFour = stringPos / 4;
    uint bytePos = stringPos % 4;
    uint loWord = heapPtr._heap[stringPosOverFour];

    uint c = intrin_Byte3(loWord, bytePos);
    if (!(c & 0x80))
    {
        (*argPtr) = stringPos + 1;
        return BCValue(Imm32(c));
    }
    else
    {
        //TODO check for invalid sequences!
        if ((c & 0b0010_0000) == 0)
        { // we have a 2byte sequence
            if (bytePos++ == 3)
            { // probability for this is 1/4;
                c |= (intrin_Byte3(heapPtr._heap[++stringPosOverFour], 0) & 0x80) >> 5;
            }
            else
            {
                c |= (intrin_Byte3(loWord, bytePos) & 0x80) >> 5;
            }
            (*argPtr) = stringPos + 2;
        }
        else if ((c & 0b0001_0000) == 0)
        { // we have a 3 byte sequence
            if (bytePos++ == 3)
            { // probability for this is 1/4;
                uint hiWord = heapPtr._heap[++stringPosOverFour];
                c |= (intrin_Byte3(hiWord, 0) & 0x80) >> 4;
                c |= (intrin_Byte3(hiWord, 1) & 0x80) >> (4+6);
            }
            else
            {
                c |= (intrin_Byte3(loWord, bytePos++) & 0x7f) >> 4;
                if (bytePos++ == 3)
                { // the probabilty for this is 2/4
                    c |= (intrin_Byte3(heapPtr._heap[++stringPosOverFour], 0) & 0x7f) >> (4+6);
                }
            }
            (*argPtr) = stringPos + 3;
        }
        else
        {
            //TODO when we support more the 3 chars cache the hi-word;
            assert(0, "For now we don't support UTF8 Chars bigger then 3");
        }

        return(BCValue(Imm32(c)));
    }
};
