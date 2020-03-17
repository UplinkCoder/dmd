/**
 * Compiler implementation of the
 * $(LINK2 http://www.dlang.org, D programming language).
 *
 * Copyright:   Copyright (C) 1999-2018 by The D Language Foundation, All Rights Reserved
 * Authors:     Stefan Koch
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/trace.d, _trace.d)
 * Documentation:  https://dlang.org/phobos/dmd_trace.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/trace.d
 */

module dmd.trace;
import dmd.dsymbol;
import dmd.expression;

enum SYMBOL_TRACE = true;
enum COMPRESSED_TRACE = false;


struct SymbolProfileEntry
{
    Dsymbol sym;

    ulong begin_ticks;
    ulong end_ticks;

    ulong begin_mem;
    ulong end_mem;

    string kind;
    string fn;

    Expression exp;
}

enum ProfileNodeType
{
    exp,
    sym
}

extern (C) __gshared uint dsymbol_profile_array_count;
extern (C) __gshared SymbolProfileEntry* dsymbol_profile_array;
enum dsymbol_profile_array_size = ushort.max * 512; // 32 million entries should do, no ?
void initTraceMemory()
{
    static if (SYMBOL_TRACE)
    {
        enum alloc_size = dsymbol_profile_array_size * SymbolProfileEntry.sizeof;
        import core.stdc.stdlib : malloc;

        if (!dsymbol_profile_array)
        {
            dsymbol_profile_array = cast(SymbolProfileEntry*) malloc(alloc_size);
        }
        assert(dsymbol_profile_array, "cannot allocate space for dsymbol_profile_array");
    }
}

string traceString(string vname, string fn = null)
{
    import TracyC;
    static if (SYMBOL_TRACE)
{
    return "import TracyC;\nmixin(zoneMixin(\"" ~ vname ~ "\"));" ~ q{
    import queryperf : QueryPerformanceCounter;
    import dmd.root.rmem;
    ulong begin_sema_ticks;
    ulong end_sema_ticks;
    ulong begin_sema_mem = Mem.allocated;

    auto insert_pos = dsymbol_profile_array_count++;
    assert(dsymbol_profile_array_count < dsymbol_profile_array_size,
        "Trying to push more then" ~ dsymbol_profile_array_size.stringof ~ " symbols");
    QueryPerformanceCounter(&begin_sema_ticks);
} ~ `
    scope(exit)
    {
        alias v_type = typeof(` ~ vname ~ `);
        QueryPerformanceCounter(&end_sema_ticks);
        static if (is(v_type : Dsymbol))
        {
            dsymbol_profile_array[insert_pos] =
                SymbolProfileEntry(` ~ vname ~ `,
            begin_sema_ticks, end_sema_ticks,
            begin_sema_mem, Mem.allocated,
            v_type.stringof, ` ~ (fn
                ? `"` ~ fn ~ `"` : "__FUNCTION__") ~ `);
        } else static if (is(v_type : Expression))
        {
            dsymbol_profile_array[insert_pos] =
                SymbolProfileEntry(null,
            begin_sema_ticks, end_sema_ticks,
            begin_sema_mem, Mem.allocated,
            v_type.stringof, ` ~ (fn
                ? `"` ~ fn ~ `"` : "__FUNCTION__") ~ ", " ~ vname ~ `);
        }
        else static assert(0, "we dont know how to deal with: " ~ v_type.stringof);
    }
`;
}
    else
        return "";
}

void writeRecord(SymbolProfileEntry dp, ref char* bufferPos)
{
    import core.stdc.stdio;
    import dmd.globals : Loc;

    static if (COMPRESSED_TRACE)
    {
        static running_id = 1;

        import dmd.root.aav;

        static struct SymInfo
        {
            void* sym;
            ulong id;

            const (char)* name;
            const (char)* loc;
        }

        SymInfo[Expression] expArray;
        SymInfo[Dsymbol] symArray;

        ulong id;

        if (dp.sym !is null)
        {
            if (auto symInfo = dp.sym in symArray)
            {
                id = symInfo.id;
            }
            else
            {
                id = running_id++;
                SymInfo symInfo = SymInfo(cast(void*)dp.sym, id);

                symInfo.name = dp.sym.toChars();
                symInfo.loc = dp.sym.loc.toChars();

                symArray[dp.sym] = symInfo;
            }
        }
        else if (dp.exp !is null)
        {
            if (auto symInfo = dp.exp in expArray)
            {
                id = symInfo.id;
            }
            else
            {
                id = running_id++;
                SymInfo symInfo = SymInfo(cast(void*)dp.exp, id);

                symInfo.name = dp.exp.toChars();
                symInfo.loc = dp.exp.loc.toChars();

                expArray[dp.exp] = symInfo;
            }
        } else
            // we should assert here I guess
            return ;
    }
    else
    {
        Loc loc;
        const (char)* name;

        if (dp.sym !is null)
        {
            loc = dp.sym.loc;
            name = dp.sym.toChars();
        }
        else if (dp.exp !is null)
        {
            loc = dp.exp.loc;
            name = dp.exp.toChars();
        }
        else
            // we probably should assert here, but whaverever.
            return ;
    }
    // Identifier id = dp.sym.ident ? dp.sym.ident : dp.sym.getIdent();
    static if (COMPRESSED_TRACE)
    {
        bufferPos += sprintf(cast(char*) bufferPos,
            "%lld|%llu|%s|%s|%lld|%lld|%lld|%lld|\n",
            dp.end_ticks - dp.begin_ticks,
            id, &dp.kind[0], &dp.fn[0],
            dp.begin_ticks, dp.end_ticks,
            dp.begin_mem, dp.end_mem
        );
    }
    else
    {
        bufferPos += sprintf(cast(char*) bufferPos,
            "%lld|%s|%s|%s|%s|%lld|%lld|%lld|%lld|\n",
            dp.end_ticks - dp.begin_ticks,
            name, &dp.kind[0], &dp.fn[0],
            loc.toChars(), dp.begin_ticks, dp.end_ticks,
            dp.begin_mem, dp.end_mem
        );
    }

}

void writeTrace(char*[] arguments)
{
    static if (SYMBOL_TRACE)
    {
        import core.stdc.stdlib;
        import core.stdc.string;
        import core.stdc.stdio;
        import dmd.root.file;

        // this is debug code we simply hope that we will not need more
        // then 2G of log-buffer;
        char* fileBuffer = cast(char*)malloc(int.max);
        char* bufferPos = fileBuffer;

        char[255] fileNameBuffer;
        import core.stdc.time : ctime, time;
        auto now = time(null);

        auto timeString = ctime(&now);
        // replace the ' ' by _ and '\n' or '\r' by '\0'
        {
            int len = 0;
            char c = void;
            for(;;)
            {
                c = timeString[len++];
                // break on null, just to be safe;
                if (!c)
                    break;

                if (c == ' ')
                    timeString[len - 1] = '_';

                if (c == '\r' || c == '\n')
                {
                    timeString[len - 1] = '\0';
                    break;
                }
            }
        }

        auto fileNameLength = 
            sprintf(&fileNameBuffer[0], "symbol-%s.1.csv".ptr, timeString);

        printf("traced_symbols: %d\n", dsymbol_profile_array_count);

        bufferPos += sprintf(cast(char*) bufferPos, "//");
        foreach(arg;arguments)
        {
            bufferPos += sprintf(bufferPos, "%s ", arg);
        }
        bufferPos += sprintf(cast(char*) bufferPos, "\n");
        static if (COMPRESSED_TRACE)
        {
            bufferPos += sprintf(cast(char*) bufferPos,
                "%s|%s|%s|%s|%s|%s|%s|%s|\n",
                "inclusive ticks".ptr,
                "id".ptr, "kind".ptr, "phase".ptr,
                "begin_ticks".ptr, "end_ticks".ptr,
                "begin_mem".ptr, "end_mem".ptr
            );
        }
        else
        {
            bufferPos += sprintf(cast(char*) bufferPos,
                "%s|%s|%s|%s|%s|%s|%s|%s|%s|\n",
                "inclusive ticks".ptr,
                "name".ptr, "kind".ptr, "phase".ptr,
                "location".ptr, "begin_ticks".ptr, "end_ticks".ptr,
                "begin_mem".ptr, "end_mem".ptr
            );
        }


        foreach(dp;dsymbol_profile_array[0 .. dsymbol_profile_array_count / 2])
        {
            writeRecord(dp, bufferPos);
        }

        printf("trace_file_size: %dk\n ", (bufferPos - fileBuffer) / 1024);
        auto data = fileBuffer[0 .. bufferPos - fileBuffer];
        auto errorcode_write = File.write(fileNameBuffer[0 .. fileNameLength], data);

        fileNameLength = sprintf(&fileNameBuffer[0], "symbol-%s.2.csv".ptr, timeString);

        auto f2 = File();
        bufferPos = fileBuffer;

        foreach(dp;dsymbol_profile_array[dsymbol_profile_array_count / 2
            .. dsymbol_profile_array_count])
        {
            writeRecord(dp, bufferPos);
        }

        data = fileBuffer[0 .. bufferPos - fileBuffer];
        errorcode_write = File.write(fileNameBuffer[0 .. fileNameLength], data);


        free(fileBuffer);
    }
}
