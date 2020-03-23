import dmd.trace_file;
import std.stdio;
import std.file;
void main(string[] args)
{

    string[] supportedModes = ["Tree", "Toplist", "Header", "PhaseHist"];

    if (args.length != 3)
    {
        writeln("Invalid invocatoion: ", args);
        writeln("Expected: ", args[0], " traceFile mode");
        writeln("Modes are", supportedModes);
        return ;
    }

    auto traceFile = args[1];

    if (!exists(traceFile))
    {
        writeln(`TraceFile: "`, traceFile, `" does not exist.`);
        return ;
    }

    auto mode = args[2];


    TraceFileHeader header;
    void[] fileBytes = read(args[1]);
    if (fileBytes.length < header.sizeof)
    {
        writeln("Tracefile truncated.");
    }

    (cast(void*)&header)[0 .. header.sizeof] = fileBytes[0 .. header.sizeof];

    if (header.magic_number != (*cast(ulong*)"DMDTRACE".ptr))
    {
        writeln("Tracefile does not have the correct magic number.");
        return ;
    }

    string[] kinds = readStrings(fileBytes, header.offset_kinds, header.n_kinds);
    string[] phases = readStrings(fileBytes, header.offset_phases, header.n_phases);

    // writeln("phases:\n    ", phases);
    // writeln("kinds:\n    ", kinds);

    SymbolProfileRecord[] records = readRecords(fileBytes, header.offset_records, header.n_records);

    static ulong hashRecord(SymbolProfileRecord r) pure
    {
        ulong hash;
        hash ^= r.begin_mem;
        hash ^= (r.end_mem << 8);
        hash ^= (r.end_ticks << 16);
        hash ^= (r.begin_ticks << 24);
        hash ^= (ulong(r.symbol_id) << 32);
        return hash;
    }

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

    if (strange_record_count)
    {
        writeln(strange_record_count, " strange records encounterd");
        return ;
    }
    // if we get here records are sorted  by begin_ticks
    // as they should be

    //writeln("records are sorted that's good n_records: ", records.length);

    // now can start establishing parent child relationships;
    import core.stdc.stdlib;

    uint[] parents = (cast(uint*)calloc(records.length, uint.sizeof))[0 .. records.length];
    uint[] depth = (cast(uint*)calloc(records.length, uint.sizeof))[0 .. records.length];
    uint[2][] selfTime = (cast(uint[2]*)calloc(records.length, uint.sizeof*2))[0 .. records.length];

    {
        uint currentDepth = 1;
        foreach(i; 0 .. records.length)
        {
            const r = records[i];
            const time = cast(uint)(r.end_ticks - r.begin_ticks);
            selfTime[i][0] = cast(uint)i;
            selfTime[i][1] = time;
            // either our parent is right above us
            if (i && records[i-1].end_ticks > r.end_ticks)
            {
                parents[i] = cast(uint)(i-1);
                depth[i] = currentDepth++;
            }
            else if (i) // or it's the parent of our parent
            {
                // the indent does not increase now we have to check if we have to pull back or not
                // our indent level is the one of the first record that ended after we ended

                uint currentParent = parents[i-1];
                while(currentParent)
                {
                    auto p = records[currentParent];
                    if (p.end_ticks > r.end_ticks)
                    {
                        selfTime[currentParent][1] -= time;
                        assert(selfTime[currentParent][1] > 1);
                        currentDepth = depth[currentParent] + 1;
                        depth[i] = currentDepth;
                        break;
                    }
                    currentParent = parents[currentParent];
                }

                //assert(currentParent);
            }
        }

    }

    if (mode == "Tree")
    {
        const char[1024] indent = '-';

        foreach(i; 0 .. records.length)
        {
            const r = records[i];

            writeln(indent[0 .. depth[i-1]], ' ',
                r.end_ticks - r.begin_ticks, "|",
                selfTime[i], "|",
                phases[r.phase_id - 1], "|",
                getSymbolName(fileBytes, r), "|",
                getSymbolLocation(fileBytes, r), "|",
            );

        }
        import std.algorithm;
        auto sorted_selfTimes  = 
            selfTime.sort!((a, b) => a[1] > b[1]).release;
        writeln("SelfTimes");
        foreach(st;sorted_selfTimes[0 .. 2000])
        {
            const r = records[st[0]];
            writeln(st[1], "|", kinds[r.kind_id-1], "|", getSymbolLocation(fileBytes, r));
        }

        free(depth.ptr);
    }
    else if (mode == "Toplist")
    {
        import std.algorithm;
        auto sorted_records = 
            records.sort!((a, b) => (a.end_ticks - a.begin_ticks > b.end_ticks - b.begin_ticks)).release;
        writeln("Toplist");
        foreach(r;sorted_records)
        {
            writeln(r.end_ticks - r.begin_ticks, "|", kinds[r.kind_id-1], "|", getSymbolLocation(fileBytes, r));
        }
    }
    else if (mode == "PhaseHist")
    {
        static struct SortRecord
        {
            uint phaseId;
            uint freq;
            float absTime = 0;
            float avgTime = 0;
        }

        SortRecord[] sortRecords;
        sortRecords.length = header.n_phases;

        foreach(i, r;records)
        {
            sortRecords[r.phase_id-1].absTime += selfTime[i][1];
            sortRecords[r.phase_id-1].freq++;
        }
        foreach(i; 0 .. header.n_phases)
        {
            sortRecords[i].phaseId = i + 1;
            sortRecords[i].avgTime = sortRecords[i].absTime / double(sortRecords[i].freq);
        }
        import std.algorithm : sort;
        sortRecords.sort!((a,b) => a.absTime > b.absTime);
        writeln(" === Phase Time Distribution : ===");
        writefln(" %-90s %-10s %-12s %-7s ", "phase",  "avgTime", "absTime", "freq");
        foreach(sr;sortRecords)
        {
            writefln(" %-90s %-10.2f %-12.0f %-7d ", phases[sr.phaseId - 1],  sr.avgTime, sr.absTime, sr.freq);
        }
    }
    else if (mode == "Header")
    {
        writeln(structToString(header));
        writeln("kinds:\n\t", kinds);
        writeln("phases:\n\t", phases);
    }
    else 
        writeln("Mode unsupported: ", mode, "\nsupported modes are: ", supportedModes);
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
