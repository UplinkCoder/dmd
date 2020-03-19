import dmd.trace_file;
import std.stdio;
import std.file;
void main(string[] args)
{
    TraceFileHeader header;
    void[] fileBytes = read(args[1]);
    (cast(void*)&header)[0 .. header.sizeof] = fileBytes[0 .. header.sizeof];
    writeln(structToString(header));


    string[] kinds = readStrings(fileBytes, header.offset_kinds, header.n_kinds);
    string[] phases = readStrings(fileBytes, header.offset_phases, header.n_phases);

    writeln("phases:\n    ", phases);
    writeln("kinds:\n    ", kinds);

    SymbolProfileRecord[] records = readRecords(fileBytes, header.offset_records, header.n_records);
    uint strange_record_count;
    ulong lastBeginTicks;

    foreach(r;records)
    {
        if (r.begin_ticks <= lastBeginTicks)
        {
            strange_record_count++;
            writeln("Symbol: ", getSymbolName(fileBytes, r), "is proucing a strange record");
        }
        lastBeginTicks = r.begin_ticks;
    }

    if (!strange_record_count)
    {
        writeln(strange_record_count, " strange records encounterd");
        return ;
    }
    // if we get here records are sorted  by begin_ticks
    // as they should be

    //writeln("records are sorted that's good n_records: ", records.length);

    // now can start establishing parent child relationships;

}

struct NoPrint {}

string structToString(T)(auto ref T _struct)
{
    char[] result;

    result ~= T.stringof ~ " (";

    foreach(i, e;_struct.tupleof)
    {
        bool skip = false;

        foreach(attrib;__traits(getAttributes, _struct.tupleof[i]))
        {
            static if (is(attrib == NoPrint))
            skip = true;
        }

        if (!skip)
        {
            alias type = typeof(_struct.tupleof[i]);
            const fieldName = _struct.tupleof[i].stringof["_struct.".length .. $];

            result ~= "" ~ fieldName ~ " : ";

            static if (is(type == enum))
            {
                result ~= enumToString(e);
            }
            else static if (is(type : ulong))
            {
                result ~= itos64(e);
            }
            else
            {
                pragma(msg, type);
                import std.conv : to;
                result ~= to!string(e);
            }
            result ~= ", ";
        }
    }

    result = result[0 .. $-1];
    result[$-1] = ')';

    return cast(string) result;
}

const(uint) fastLog10(const uint val) pure nothrow @nogc @safe
{
    return (val < 10) ? 0 : (val < 100) ? 1 : (val < 1000) ? 2 : (val < 10000) ? 3
        : (val < 100000) ? 4 : (val < 1000000) ? 5 : (val < 10000000) ? 6
        : (val < 100000000) ? 7 : (val < 1000000000) ? 8 : 9;
}

/*@unique*/
static immutable fastPow10tbl = [
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
];

string itos(const uint val) pure @trusted nothrow
{
    immutable length = fastLog10(val) + 1;
    char[] result;
    result.length = length;

    foreach (i; 0 .. length)
    {
        immutable _val = val / fastPow10tbl[i];
        result[length - i - 1] = cast(char)((_val % 10) + '0');
    }

    return cast(string) result;
}

static assert(mixin(uint.max.itos) == uint.max);

string itos64(const ulong val) pure @trusted nothrow
{
    if (val <= uint.max)
        return itos(val & uint.max);

    uint lw = val & uint.max;
    uint hi = val >> 32;

    auto lwString = itos(lw);
    auto hiString = itos(hi);

    return cast(string) "((" ~ hiString ~ "<< 32)" ~ "|" ~ lwString ~ ")";
}


string enumToString(E)(E v)
{
    static assert(is(E == enum),
        "emumToString is only meant for enums");
    string result;

    Switch : switch(v)
    {
        foreach(m;__traits(allMembers, E))
        {
            case mixin("E." ~ m) :
                result = m;
            break Switch;
        }

        default :
        {
            result = "cast(" ~ E.stringof ~ ")";
            uint val = v;
            enum headLength = cast(uint)(E.stringof.length + "cast()".length);
            uint log10Val = (val < 10) ? 0 : (val < 100) ? 1 : (val < 1000) ? 2 :
                (val < 10000) ? 3 : (val < 100000) ? 4 : (val < 1000000) ? 5 :
                (val < 10000000) ? 6 : (val < 100000000) ? 7 : (val < 1000000000) ? 8 : 9;
            result.length += log10Val + 1;
            for(uint i;i != log10Val + 1;i++)
            {
                cast(char)result[headLength + log10Val - i] = cast(char) ('0' + (val % 10));
                val /= 10;
            }
        }
    }

    return result;
}

enum hexString = (ulong value)
{
    const wasZero = !value;
    static immutable NibbleRep = "0123456789abcdef";
    char[] resultBuffer;
    resultBuffer.length = 18; // ulong.sizeof * 2 + "0x".length
    resultBuffer[] = '0';
    int p;
    for(ubyte currentNibble = value & 0xF;
        value;
        currentNibble = ((value >>>= 4) & 0xF)
    )
    {
        resultBuffer[17 - p++] = NibbleRep[currentNibble];
    }
    resultBuffer[17 - wasZero - p++] = 'x';
    return cast(string) resultBuffer[17 - p - wasZero .. 18];
};
