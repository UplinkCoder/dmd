module dmd.trace_file;

struct SymbolProfileRecord
{
    ulong begin_ticks;
    ulong end_ticks;
    
    ulong begin_mem;
    ulong end_mem;
    
    uint symbol_id;
    uint kind_id;
    uint phase_id;
    
    ubyte[4] pad;
}

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
static string[] readString()(const void[] file, uint offset_strings, uint n_strings)
{
    const (char)[][] result;

    StringPointer* stringPointers = cast(StringPointer*)(startOfFile.ptr + offset_strings);
    foreach(i; 0 .. header.n_strings)
    {
        StringPointer p = *stringPointers++;
        result ~= (cast(char*)startOfFile.ptr)[p.string_start .. p.one_past_string_end];
    }

    return (cast(string*)result.ptr)[0 .. result.length];
}