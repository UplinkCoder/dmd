module dmd.trace_file;

struct SymbolProfileRecord
{
    ulong begin_ticks;
    ulong end_ticks;
    
    ulong begin_mem;
    ulong end_mem;
    
    uint symbol_id;
    ushort kind_id;
    ushort phase_id;
}

pragma(msg, "ProfileRecord.sizeof = ", SymbolProfileRecord.sizeof);

struct TraceFileHeader
{
    ulong magic_number;

    uint FileVersion;

    uint n_records;
    uint n_phases;
    uint n_kinds;
    uint n_symbols;
    
    uint offset_records;
    uint offset_phases;
    uint offset_kinds;
    uint offset_symbol_info_descriptors;
}

align(1) struct SymbolInfoPointers
{
align(1):
    uint symbol_name_start;
    uint symobol_location_start;
    uint one_past_symbol_location_end;
}

align(1) struct StringPointer
{
align(1):
    uint string_start;
    uint one_past_string_end;
}

// the only reason this is a template is becuase d does not allow one to
// specify inline linkage ... sigh
static string[] readStrings()(const void[] file, uint offset_strings, uint n_strings)
{
    const (char)[][] result;
    result.length = n_strings;

    StringPointer* stringPointers = cast(StringPointer*)(file.ptr + offset_strings);
    foreach(i; 0 .. n_strings)
    {
        StringPointer p = *stringPointers++;
        result[i] = (cast(char*)file.ptr)[p.string_start .. p.one_past_string_end];
    }

    return (cast(string*)result.ptr)[0 .. result.length];
}