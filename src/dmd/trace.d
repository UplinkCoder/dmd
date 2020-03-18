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
import dmd.mtype;
import dmd.statement;
import dmd.root.rootobject;

enum SYMBOL_TRACE = true;
enum COMPRESSED_TRACE = true;


struct SymbolProfileEntry
{
    ProfileNodeType nodeType;

    ulong begin_ticks;
    ulong end_ticks;

    ulong begin_mem;
    ulong end_mem;

    string kind; // asttypename
    string fn; // which function is being traced

    union
    {
        RootObject ro;
        Dsymbol sym;
        Expression exp;
        Statement stmt;
        Type type;
        void* vp;
    }
}

enum ProfileNodeType
{
    Invalid,
    Dsymbol,
    Expression,
    Statement,
    Type,
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
    static if (SYMBOL_TRACE)
{
    return q{
    import queryperf : QueryPerformanceCounter;
    import dmd.root.rmem;
    import dmd.asttypename;
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
        auto v = ` ~ vname ~ `;
        alias v_type = typeof(v);
        QueryPerformanceCounter(&end_sema_ticks);
        static if (is(v_type : Dsymbol))
        {
            dsymbol_profile_array[insert_pos] =
                SymbolProfileEntry(ProfileNodeType.Dsymbol,
                begin_sema_ticks, end_sema_ticks,
                begin_sema_mem, Mem.allocated,
                astTypeName(v), ` ~ (fn
                    ? `"` ~ fn ~ `"` : `__FUNCTION__`) ~ `);
            dsymbol_profile_array[insert_pos].sym = v; 
        } else static if (is(v_type : Expression))
        {
            dsymbol_profile_array[insert_pos] =
                SymbolProfileEntry(ProfileNodeType.Expression,
                begin_sema_ticks, end_sema_ticks,
                begin_sema_mem, Mem.allocated,
                astTypeName(v), ` ~ (fn
                    ? `"` ~ fn ~ `"` : `__FUNCTION__`) ~ `);
            dsymbol_profile_array[insert_pos].exp = v; 
        } else static if (is(v_type : Statement))
        {
            dsymbol_profile_array[insert_pos] =
                SymbolProfileEntry(ProfileNodeType.Statement,
                begin_sema_ticks, end_sema_ticks,
                begin_sema_mem, Mem.allocated,
                astTypeName(v), ` ~ (fn
                    ? `"` ~ fn ~ `"` : `__FUNCTION__`) ~ `);
            dsymbol_profile_array[insert_pos].stmt = v; 
        } else static if (is(v_type : Type))
        {
            dsymbol_profile_array[insert_pos] =
                SymbolProfileEntry(ProfileNodeType.Type,
                begin_sema_ticks, end_sema_ticks,
                begin_sema_mem, Mem.allocated,
                astTypeName(v), ` ~ (fn
                    ? `"` ~ fn ~ `"` : `__FUNCTION__`) ~ `);
            dsymbol_profile_array[insert_pos].type = v; 
        }
        else static assert(0, "we dont know how to deal with: " ~ v_type.stringof);
    }
`;
}
    else
        return "";
}

__gshared ulong numInvalidProfileNodes = 0;

static if (COMPRESSED_TRACE)
{
    static struct SymInfo
    {
        void* sym;
        uint id;
        uint pad;
        
        const (char)* name;
        const (char)* loc;
        const (char)* typename;
    }

	uint[string] kindArray;
	uint kindArrayNextId = 1;
	uint[string] phaseArray;
	uint phaseArrayNextId = 1;

    SymInfo[Expression] expArray;
    SymInfo[Dsymbol] symArray;
    SymInfo[Statement] stmtArray;
    SymInfo[Type] typeArray;
}

void writeRecord(SymbolProfileEntry dp, ref char* bufferPos)
{
    import core.stdc.stdio;
    import dmd.globals : Loc;

    static if (COMPRESSED_TRACE)
    {
		int result;

		int getKindId(string kind, bool justLookup = true)
		{
			if (auto id = kind in kindArray)
			{
				result = *id;
			}
			else
			{
				assert(!justLookup);
				auto id = kindArrayNextId++;
				kindArray[kind] = id;
				result = id;
			}

			return result;
		}

		int getPhaseId(string phase, bool justLookup = true)
		{
			int result;

			if (auto id = phase in phaseArray)
			{
				result = *id;
			}
			else
			{
				assert(!justLookup);
				auto id = phaseArrayNextId++;
				phaseArray[phase] = id;
				result = id;
			}

			return result;
		}

		auto kindId = getKindId(dp.kind, false);
		auto phaseId = getPhaseId(dp.fn, false);

        static uint running_id = 1;

        uint id;
        SymInfo info;

        final switch(dp.nodeType)
        {
            case ProfileNodeType.Dsymbol :
                if (auto symInfo = dp.sym in symArray)
                {
                    id = symInfo.id;
                }
                break;
            case ProfileNodeType.Expression :
                if (auto symInfo = dp.exp in expArray)
                {
                    id = symInfo.id;
                }
                break;             
            case ProfileNodeType.Statement :
                if (auto symInfo = dp.stmt in stmtArray)
                {
                    id = symInfo.id;
                }
                break;             
            case ProfileNodeType.Type :
                if (auto symInfo = dp.exp in expArray)
                {
                    id = symInfo.id;
                }
                break;             
                // we should probably assert here.
            case ProfileNodeType.Invalid:
                numInvalidProfileNodes++;
                return ;
        }

        if (!id) // we haven't haven't seen this symbol before
        {
            id = running_id++;
            SymInfo symInfo = SymInfo(dp.vp, id);

            final switch(dp.nodeType)
            {
                case ProfileNodeType.Dsymbol :
                    symInfo.name = dp.sym.toChars();
                    symInfo.loc = dp.sym.loc.toChars();
                    
                    symArray[dp.sym] = symInfo;
                break;
                case ProfileNodeType.Expression :
                    symInfo.name = dp.exp.toChars();
                    symInfo.loc = dp.exp.loc.toChars();
                    
                    expArray[dp.exp] = symInfo;
                break;
                case ProfileNodeType.Statement:
                    symInfo.name = ((dp.stmt.isForwardingStatement() || dp.stmt.isPeelStatement()) ? 
                        "toChars() for statement not implemented" :
                        dp.stmt.toChars());
                    symInfo.loc = dp.stmt.loc.toChars();

                    stmtArray[dp.stmt] = symInfo;
                break;
                case ProfileNodeType.Type :
                    symInfo.name = dp.type.toChars();

                    typeArray[dp.type] = symInfo;
                break;

                 case ProfileNodeType.Invalid:
                     assert(0); // this cannot happen
            }
        }
    }
    else
    {
        Loc loc;
        const (char)* name;

        final switch(dp.nodeType)
        {
            case ProfileNodeType.Dsymbol :
                loc = dp.sym.loc;
                name = dp.sym.toChars();
            break;
            case ProfileNodeType.Expression :
                loc = dp.exp.loc;
                name = dp.exp.toChars();
            break;             
            case ProfileNodeType.Statement :
                loc = dp.stmt.loc;
                name = ((dp.stmt.isForwardingStatement() || dp.stmt.isPeelStatement) ?
					"toChars() for statement not implemented" :
					dp.stmt.toChars()
				);
            break;
            case ProfileNodeType.Type :
                name = dp.type.toChars();
            break;
            // we should probably assert here.
            case ProfileNodeType.Invalid:
                return ;
    
        }
    }
    // Identifier ident = dp.sym.ident ? dp.sym.ident : dp.sym.getIdent();
    static if (COMPRESSED_TRACE)
    {
		SymbolProfileRecord* rp = cast(SymbolProfileRecord*) bufferPos;
		bufferPos += SymbolProfileRecord.sizeof;

		SymbolProfileRecord r = {
			dp.begin_ticks, dp.end_ticks,
			dp.begin_mem, dp.end_mem,
			id, kindId, phaseId
		};
        (*rp) = r;
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
struct SymbolProfileRecord
{
    ulong begin_ticks;
    ulong end_ticks;
    
    ulong begin_mem;
    ulong end_mem;

    uint symbol_id;
    uint kind_id;
    uint phase_id;
}

struct TraceFile
{
    uint n_phases;
    uint n_kinds;
    uint n_symbols;
    uint n_records;

    uint offset_phases;
    uint offset_kinds;
    uint offset_symbol_names;
    uint offset_symbol_locations;
    uint offset_records;

    SymbolProfileRecord[] records;

    string[] phases;
    string[] kinds;
    string[] symbol_names;
    string[] symbol_locations;
}
 

void writeTrace(char*[] arguments, char* traceFileName = null)
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
            sprintf(&fileNameBuffer[0], "symbol-%s.1.csv".ptr, traceFileName ? traceFileName : timeString);

        printf("traced_symbols: %d\n", dsymbol_profile_array_count);

        static if (COMPRESSED_TRACE)
        {
			// we don't write the args ... will need to be fixed at some point
			// no schema information is written for the compressed trance,
			// since it's known by the reader
        }
        else
        {
			bufferPos += sprintf(cast(char*) bufferPos, "//");
			foreach(arg;arguments)
			{
				bufferPos += sprintf(bufferPos, "%s ", arg);
			}
			bufferPos += sprintf(cast(char*) bufferPos, "\n");

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

        fileNameLength = sprintf(&fileNameBuffer[0], "symbol-%s.2.csv".ptr, traceFileName ? traceFileName : timeString);

        auto f2 = File();
        bufferPos = fileBuffer;

        foreach(dp;dsymbol_profile_array[dsymbol_profile_array_count / 2
            .. dsymbol_profile_array_count])
        {
            writeRecord(dp, bufferPos);
        }

        data = fileBuffer[0 .. bufferPos - fileBuffer];
        errorcode_write = File.write(fileNameBuffer[0 .. fileNameLength], data);

        static if (COMPRESSED_TRACE)
        {
            bufferPos = fileBuffer;
            writeMetadata(bufferPos);
        }


        free(fileBuffer);
    }
}
