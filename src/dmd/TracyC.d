extern (C):

void TracyMessage(string msg, int callstack = 64)
{
    import core.stdc.stdlib : alloca;
    auto mem = cast(char*)alloca(msg.length + 1);
    mem[0 .. msg.length] = msg[0 .. msg.length];
    mem[msg.length] = '\0';
    ___tracy_emit_message(mem, msg.length, callstack);
}

string zoneMixin(string name, uint callstack_depth = 64, string file = __FILE__, string function_ = __FUNCTION__, uint line = __LINE__)
{
    static string itos_(const uint val) pure @trusted nothrow
    {
        /*@unique*/
        static immutable fastPow10tbl = [
            1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
        ];

        immutable length =
                    (val < 10) ?
                1 : (val < 100) ?
                2 : (val < 1000) ?
                3 : (val < 10000) ?
                4 : (val < 100000) ?
                5 : (val < 1000000) ?
                6 : (val < 10000000) ?
                7 : (val < 100000000) ?
                8 : (val < 1000000000) ?
                9 : 10;

        char[] result;
        result.length = length;

        foreach (i; 0 .. length)
        {
            immutable _val = val / fastPow10tbl[i];
            result[length - i - 1] = cast(char)((_val % 10) + '0');
        }

        return cast(string) result;
    }

    static assert(mixin(itos_(uint.max)) == uint.max);

    const loc_hash = (itos_(cast(uint)file.hashOf) ~ "_" ~ itos_(line));
    const loc_string =  "loc_" ~ loc_hash;
    const ctx_string = "ctx_" ~ loc_hash;
    const callstack_depth_string = itos_(callstack_depth);

    return `static immutable ` ~ loc_string
        ~ ` = cast(immutable) ___tracy_source_location_data ("` ~ name ~ `", "`~ function_ ~ `", "` ~ file ~ `", ` ~ itos_(line) ~ `);`
            ~ ` immutable ` ~ ctx_string ~ `= ___tracy_emit_zone_begin_callstack(&` ~ loc_string ~ `, 64, 1);`
            ~ `scope(exit) ___tracy_emit_zone_end(cast()` ~ ctx_string ~ `);`;
}


extern (C):

struct ___tracy_source_location_data
{
    const(char)* name;
    const(char)* function_;
    const(char)* file;
    uint line;
    uint color;
}

struct ___tracy_c_zone_context
{
    uint id;
    int active;
}


// Some containers don't support storing const types.
// This struct, as visible to user, is immutable, so treat it as if const was declared here.
/*const*/
alias TracyCZoneCtx = ___tracy_c_zone_context;

TracyCZoneCtx ___tracy_emit_zone_begin (const(___tracy_source_location_data)* srcloc, int active);
TracyCZoneCtx ___tracy_emit_zone_begin_callstack (const(___tracy_source_location_data)* srcloc, int depth, int active);
void ___tracy_emit_zone_end (TracyCZoneCtx ctx);
void ___tracy_emit_zone_text (TracyCZoneCtx ctx, const(char)* txt, size_t size);
void ___tracy_emit_zone_name (TracyCZoneCtx ctx, const(char)* txt, size_t size);

void ___tracy_emit_memory_alloc (const(void)* ptr, size_t size);
void ___tracy_emit_memory_alloc_callstack (const(void)* ptr, size_t size, int depth);
void ___tracy_emit_memory_free (const(void)* ptr);
void ___tracy_emit_memory_free_callstack (const(void)* ptr, int depth);

void ___tracy_emit_message (const(char)* txt, size_t size, int callstack);
void ___tracy_emit_messageL (const(char)* txt, int callstack);
void ___tracy_emit_messageC (const(char)* txt, size_t size, uint color, int callstack);
void ___tracy_emit_messageLC (const(char)* txt, uint color, int callstack);

void ___tracy_emit_frame_mark (const(char)* name);
void ___tracy_emit_frame_mark_start (const(char)* name);
void ___tracy_emit_frame_mark_end (const(char)* name);
void ___tracy_emit_frame_image (const(void)* image, ushort w, ushort h, ubyte offset, int flip);

void ___tracy_emit_plot (const(char)* name, double val);
void ___tracy_emit_message_appinfo (const(char)* txt, size_t size);

//alias TracyCAllocS = ___tracy_emit_memory_alloc_callstack;
//alias TracyCFreeS = ___tracy_emit_memory_alloc_free_callstack;
