module dmd.root.ticket;

import core.atomic;

import core.stdc.stdio;
import core.stdc.string;
import core.stdc.stdlib;
import core.sys.posix.dlfcn;

import dmd.root.intel_inspector;

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
