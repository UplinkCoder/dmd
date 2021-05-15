module dmd.root.ticket;

import core.atomic;


import core.stdc.stdio;
import core.stdc.string;
import core.stdc.stdlib;
import core.sys.posix.dlfcn;


extern (C) @nogc nothrow __gshared {
    void dummy (void *p) {}
    /**
 * @brief Quit spin loop without acquiring spin object
 */
    void function (void* addr) __itt_sync_cancel = &dummy;
 /**
 * @brief Successful spin loop completion (sync object acquired)
 */
    void function (void* addr) __itt_sync_acquired = &dummy;
    /**
 * @brief Start sync object releasing code. Is called before the lock release call.
 */
    void function (void* addr) __itt_sync_releasing = &dummy;
}

shared static this()
{
    auto insp_dir_c = getenv("INSPECTOR_2021_DIR");
    auto insp_dir = insp_dir_c ? cast(string) insp_dir_c[0 .. strlen(insp_dir_c)] : null;
    if (insp_dir)
    {
        char[1024] pathbuf;
        sprintf(pathbuf.ptr, "%s/lib64/runtime/libittnotify.so", insp_dir_c);
        auto lib = dlopen(pathbuf.ptr, RTLD_NOW);
        if (lib)
        {
            __itt_sync_cancel = cast(typeof(__itt_sync_cancel))dlsym(lib, "__itt_sync_cancel");
            __itt_sync_acquired = cast(typeof(__itt_sync_acquired))dlsym(lib, "__itt_sync_acquired");
            __itt_sync_releasing = cast(typeof(__itt_sync_releasing))dlsym(lib, "__itt_sync_releasing");

            return ;
        }
    }

    fprintf(stderr, "intel inspector library functions could not be loaded\n");
}


/// Ticket Lock ordered syncronisation mechanism
struct TicketCounter
{
@nogc: nothrow:
    shared align(16) uint nextTicket;
    shared align(16) uint currentlyServing;


    Loc lastAquiredLoc;
    struct Loc
    {
        string func;
        string file;
        int line;
    }

    uint drawTicket() shared
    {
        pragma(inline, true);
        return atomicFetchAdd(nextTicket, 1);
    }

    void releaseTicket(uint ticket) shared
    {
        lastAquiredLoc = Loc("free","free", 0);
        __itt_sync_releasing(cast(void*) &this);
        pragma(inline, true);
        assert(ticket == currentlyServing);
        atomicFetchAdd(currentlyServing, 1);
    }

    bool servingMe(uint ticket, string func = __FUNCTION__, string file = __FILE__, int line = __LINE__) shared
    {
        pragma(inline, true);

        auto result = atomicLoad(currentlyServing) == ticket;
        if (result)
        {
            __itt_sync_acquired(cast(void*) &this);
            lastAquiredLoc = Loc(func, file, line);
        }


        return result;
    }

    void redrawTicket(ref uint ticket) shared
    {
        pragma(inline, true);
        releaseTicket(ticket);
        ticket = drawTicket();
    }
}
